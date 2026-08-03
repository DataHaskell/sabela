{-# LANGUAGE OverloadedStrings #-}

{- | What a module card is allowed to say: a @:browse@ that printed a GHC
diagnostic is not a listing, and a module an installed package re-exports is not
absent. Stated over arbitrary module, package and type names.
-}
module Test.ModuleCardTruthSpec (spec) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.List (sort)
import Data.Maybe (isJust, isNothing, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (findExecutable)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Sabela.AI.Capabilities.ModuleCard (browseFailed, browseHidden)
import Sabela.AI.Capabilities.ModuleCard.Browse (browseOutcome)
import Sabela.AI.Capabilities.ModuleCard.Card (moduleCardValue)
import Sabela.AI.Capabilities.ModuleCard.Hit (hitJSON)
import Sabela.AI.Capabilities.ModuleCard.Resolve (resolveModule)
import Sabela.AI.Capability (
    Hit (..),
    Match (..),
    parseCapabilities,
    searchCapabilities,
 )
import Sabela.AI.PackageIndex (PackageEntry (..), parsePackageDump)
import Test.HarnessGen (genConIdent, genIdent, genModuleName, genPackageName)

spec :: Spec
spec = do
    probeTruthSpec
    probeWiringSpec
    cardTruthSpec
    reExportTruthSpec
    synonymSpec

{- | The recognizer decides, but only if the probe consults it. GHC exits zero
and writes to stderr, so this runs a real one against an empty package db.
-}
probeWiringSpec :: Spec
probeWiringSpec = describe "a real probe reports its own failure" $ do
    it "an error page is a failure, whatever the exit code" $
        withEmptyDb $ \db -> do
            r <- browseHidden db "base" "Zzz.NoSuchModule.Here"
            r `shouldSatisfy` either (T.isInfixOf "Zzz.NoSuchModule.Here") no

    it "a listing is still a listing" $
        withEmptyDb $ \db -> do
            r <- browseHidden db "base" "Data.List"
            r `shouldSatisfy` either no (T.isInfixOf "::")
  where
    no = const False

withEmptyDb :: (FilePath -> IO ()) -> IO ()
withEmptyDb act = do
    mGhci <- findExecutable "ghci"
    case mGhci of
        Nothing -> pendingWith "ghci not on PATH"
        Just _ -> withSystemTempDirectory "d2-browse" $ \dir -> do
            let db = dir </> "pkgdb"
            (code, _, _) <- readProcessWithExitCode "ghc-pkg" ["init", db] ""
            if code == ExitSuccess
                then act db
                else pendingWith "ghc-pkg init unavailable"

{- | GHC's diagnostic shells, wrapped around whatever module name is asked for.
The last has no @error:@ in it, so only the bracketed code marks it.
-}
errorPages :: Text -> [Text]
errorPages m =
    [ header "error: [GHC-35235]" ("Could not find module " <> q)
    , header "error: [GHC-45102]" ("Ambiguous module name " <> q)
    , header "error:" ("Not in scope: " <> q)
    , header "[GHC-87110]" ("Could not load module " <> q)
    ]
  where
    q = "\8216" <> m <> "\8217"
    header h body = "<no location info>: " <> h <> "\n    " <> body <> "."

genListing :: Gen Text
genListing =
    T.unlines . map sig <$> listOf1 ((,) <$> genIdent <*> genConIdent)
  where
    sig (n, ty) = n <> " :: " <> ty

entry :: Text -> Text -> [Text] -> PackageEntry
entry n v = PackageEntry n v ""

somePkg :: PackageEntry
somePkg = entry "somepkg" "1.0.0" ["Some.Module", "Syn.Mod"]

cardOf :: [PackageEntry] -> PackageEntry -> Text -> Either Text Text -> Value
cardOf idx p m = moduleCardValue Nothing idx p m Nothing ""

field :: Text -> Value -> Maybe Value
field k (Object o) = KM.lookup (Key.fromText k) o
field _ _ = Nothing

intField :: Text -> Value -> Int
intField k v = case field k v of
    Just (Number n) -> round n
    _ -> 0

exportsOf :: Value -> [Text]
exportsOf v = case field "exports" v of
    Just (Array es) -> [s | String s <- foldr (:) [] es]
    _ -> []

coverageFlag :: Value -> Maybe Value
coverageFlag v = field "coverage" v >>= field "enumerated"

rendered :: Value -> Text
rendered = T.pack . show

probeTruthSpec :: Spec
probeTruthSpec = describe "a probe that failed is not a result" $ do
    prop "reads a GHC diagnostic as a diagnostic, whatever module it names" $
        forAll genModuleName (all (isJust . browseFailed) . errorPages)
    prop "reads a listing as a listing" $
        forAll genListing (isNothing . browseFailed)
    it "counts silence as nothing enumerated, not as a module without exports" $
        browseFailed "   \n  " `shouldSatisfy` isJust
    -- GHC writes the diagnostic to stderr and leaves stdout empty, so a probe
    -- judged on stdout alone reports the error page as an empty module.
    prop "finds the reason on whichever stream carries it" $
        forAll genModuleName $ \m ->
            all
                (either (m `T.isInfixOf`) (const False) . browseOutcome "")
                (errorPages m)
    prop "a warning beside a listing does not make the listing a failure" $
        forAll genListing $ \l ->
            browseOutcome l "ghci: warning: something\n" == Right l

cardTruthSpec :: Spec
cardTruthSpec = describe "a module card enumerates only what was enumerated" $ do
    prop "a failed probe never yields exports" $
        forAll genModuleName $ \m ->
            all (null . exportsOf . failedCard m) (errorPages m)
    prop "a failed probe is disclosed as unenumerated" $
        forAll genModuleName $ \m ->
            all (unenumerated . failedCard m) (errorPages m)
    prop "the card carries the reason the probe gave, not a claim of its own" $
        forAll genModuleName $ \m ->
            ("boom " <> m) `T.isInfixOf` rendered (failedCard m ("boom " <> m))
    prop "an empty parse is never promoted to a claim about the module" $
        forAll genModuleName $ \m ->
            let cards = [listedCard m "", listedCard m "\8212 no decls \8212"]
             in not (any (T.isInfixOf "exposes nothing" . rendered) cards)
                    && all unenumerated cards
    prop "a real listing enumerates, and says nothing about coverage" $
        forAll ((,) <$> genModuleName <*> genListing) $ \(m, l) ->
            let card = listedCard m l
             in not (null (exportsOf card)) && isNothing (coverageFlag card)
  where
    failedCard m = cardOf [somePkg] somePkg m . Left
    listedCard m = cardOf [somePkg] somePkg m . Right
    unenumerated = (== Just (Bool False)) . coverageFlag

reExportTruthSpec :: Spec
reExportTruthSpec = describe "a re-exported module is reachable, and says so" $ do
    prop "a module an installed package re-exports resolves" $
        forAll distinct $ \(a, b, m) ->
            isJust (resolveModule (parsePackageDump (dump a b m)) m)
    prop "the card names the other installed packages that expose it" $
        forAll distinct $ \(a, b, m) ->
            field "exposedBy" (reCard a b m m) == Just (Array (pure (String b)))

    prop "a package that does not expose it is not named" $
        forAll distinct $ \(a, b, m) ->
            isNothing (field "exposedBy" (reCard a b m (m <> ".Elsewhere")))

    -- The store registers every version it ever built; GHCi links the newest,
    -- so a card built from whichever the dump listed first reports a version
    -- the browse did not use.
    prop "resolves to the newest installed entry, not the first listed" $
        forAll ((,) <$> genPackageName <*> genModuleName) $ \(p, m) ->
            let idx = [entry p v [m] | v <- ["0.3.3.7", "10.0.0", "2.1.0"]]
             in fmap (peVersion . snd) (resolveModule idx m) == Just "10.0.0"
  where
    distinct =
        ((,,) <$> genPackageName <*> genPackageName <*> genModuleName)
            `suchThat` (\(a, b, _) -> a /= b)
    reCard a b m asked =
        let idx = parsePackageDump (dump a b m)
         in cardOf idx (head [p | p <- idx, peName p == a]) asked (Right "")
    dump owner reExporter m =
        T.unlines
            [ "name: " <> owner
            , "version: 1.0.0"
            , "exposed-modules: " <> m
            , "---"
            , "name: " <> reExporter
            , "version: 2.0.0"
            , "exposed-modules:"
            , "    " <> m <> " from unit-1.0.0:" <> m
            ]

-- | Heads with and without parameters; the last three expansions carry an @=@.
genSyn :: Gen (Text, Text)
genSyn = do
    hd <- (<>) <$> genConIdent <*> elements ["", " a", " a b"]
    a <- genConIdent
    b <- genConIdent
    rhs <-
        elements
            [ "(" <> a <> ", " <> b <> ")"
            , a <> " -> " <> b
            , "(" <> a <> " ~ " <> b <> ") => " <> a
            , "Rec [\"k\" = " <> a <> "]"
            ]
    pure (hd, rhs)

{- | Fix Y. @mixedExports@ quotes every row but the synonym verbatim from the
parser's output before the fix, so truncating a data, newtype or class head at
its @=@ — which is right for a constructor — still fails here.
-}
synonymSpec :: Spec
synonymSpec = describe "a synonym expansion is API information" $ do
    prop "the card states the declaration, '=' inside it and all" $
        forAll genSyn (\s -> cardExports (synOf s) === [synOf s])
    prop "a head the parser read no expansion for never grows one" $
        forAll genSyn $ \(hd, _) ->
            not (any (T.isInfixOf " = ") (cardExports ("type " <> hd)))
    it "every other declaration kind renders as it did" $
        sort (cardExports mixed) `shouldBe` sort mixedExports
    prop "a hit states the declaration the listing gave, not a bare head" $
        forAll genSyn $ \s ->
            field "type" (hitOf (synOf s)) === Just (String (synOf s))
    prop "a synonym a shown export names is carried past the width cap" $
        forAll twoNames $ \(a, b) ->
            elem (declOf a) (cardExports (crowded a a))
                && notElem (declOf a) (cardExports (crowded b a))
    prop "shown and omitted account for every export the listing declares" $
        forAll genConIdent $ \a ->
            let raw = crowded a a
             in length (cardExports raw) + intField "omitted" (cardFor raw)
                    === length (parseCapabilities "" raw)
    prop "an export naming the module outranks one that does not, either kind" $
        forAll twoNames $ \(a, b) ->
            let rows k = [k b "Int", k a namesake]
                sig n t = n <> "Fn :: " <> t
             in conjoin
                    [ cardExports (T.unlines (rows k)) === reverse (rows k)
                    | k <- [sig, expands]
                    ]
    -- The precondition is the case where a token of the expansion is the
    -- synonym's own name, which the ranker matches by name either way.
    prop "the keyword the parser wrote earns no type match" $
        forAll genSyn $ \s@(hd, rhs) ->
            not (T.toLower (nameOf hd) `T.isInfixOf` T.toLower rhs) ==>
                (viaOf ("type " <> rhs) (capOf s) /= Just ByType, viaOf rhs (capOf s))
                    === (True, Just ByType)
  where
    synMod = "Syn.Mod"
    namesake = "Mod"
    cardFor raw = cardOf [somePkg] somePkg synMod (Right raw)
    cardExports = exportsOf . cardFor
    hitOf raw = hitJSON "" (Hit (head (parseCapabilities "" raw)) 0 ByName)
    capOf s = head (parseCapabilities synMod (synOf s))
    viaOf q c = hitVia <$> listToMaybe (searchCapabilities [] [c] q)
    nameOf = T.takeWhile (/= ' ')
    twoNames = ((,) <$> genConIdent <*> genConIdent) `suchThat` uncurry (/=)
    synOf (hd, rhs) = "type " <> hd <> " = " <> rhs
    expands n rhs = "type " <> n <> " = " <> rhs
    declOf n = expands n "Maybe Int"
    crowded named syn =
        T.unlines $
            ["useIt :: " <> named <> " -> Int"]
                ++ ["f" <> T.pack (show i) <> " :: Int" | i <- [1 :: Int .. 64]]
                ++ [declOf syn]
    mixed =
        T.unlines
            [ "type Handle = Maybe (Slot Int)"
            , "data Slot a = Slot {slotKey :: Int, slotVal :: a}"
            , "newtype Tag = Tag Int"
            , "class Eq a => Sized a where"
            , "  sizeOf :: a -> Int"
            , "  {-# MINIMAL sizeOf #-}"
            , "measure :: Slot Int -> Int"
            ]
    mixedExports =
        [ "type Handle = Maybe (Slot Int)"
        , "Slot :: Slot a"
        , "slotKey :: Slot a -> Int"
        , "slotVal :: Slot a -> a"
        , "Tag :: Tag"
        , "Sized :: Sized a"
        , "sizeOf :: (Sized a) => a -> Int"
        , "measure :: Slot Int -> Int"
        ]
