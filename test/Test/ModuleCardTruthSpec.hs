{-# LANGUAGE OverloadedStrings #-}

{- | What a module card is allowed to say: a @:browse@ that printed a GHC
diagnostic is not a listing, and a module an installed package re-exports is not
absent. Stated over arbitrary module, package and type names.
-}
module Test.ModuleCardTruthSpec (spec) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (findExecutable)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Test.Hspec
import Test.QuickCheck

import Sabela.AI.Capabilities.ModuleCard (
    browseFailed,
    browseHidden,
    moduleCardValue,
 )
import Sabela.AI.Capabilities.ModuleCard.Browse (browseOutcome)
import Sabela.AI.Capabilities.ModuleCard.Resolve (resolveModule)
import Sabela.AI.PackageIndex (PackageEntry (..), parsePackageDump)
import Test.HarnessGen (genConIdent, genIdent, genModuleName, genPackageName)

spec :: Spec
spec = do
    probeTruthSpec
    probeWiringSpec
    cardTruthSpec
    reExportTruthSpec

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

listingOf :: [(Text, Text)] -> Text
listingOf rows = T.unlines [n <> " :: " <> ty | (n, ty) <- rows]

genListing :: Gen Text
genListing = listingOf <$> listOf1 ((,) <$> genIdent <*> genConIdent)

entry :: Text -> Text -> [Text] -> PackageEntry
entry n v = PackageEntry n v ""

cardOf ::
    Maybe Text ->
    [PackageEntry] ->
    PackageEntry ->
    Text ->
    Either Text Text ->
    Value
cardOf q idx p m = moduleCardValue q idx p m Nothing ""

field :: Text -> Value -> Maybe Value
field k (Object o) = KM.lookup (Key.fromText k) o
field _ _ = Nothing

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
    it "reads a GHC diagnostic as a diagnostic, whatever module it names" $
        property $
            forAll genModuleName $ \m ->
                all (isJust . browseFailed) (errorPages m)

    it "reads a listing as a listing" $
        property $
            forAll genListing (isNothing . browseFailed)

    it "counts silence as nothing enumerated, not as a module without exports" $
        browseFailed "   \n  " `shouldSatisfy` isJust

    -- GHC writes the diagnostic to stderr and leaves stdout empty, so a probe
    -- judged on stdout alone reports the error page as an empty module.
    it "finds the reason on whichever stream carries it" $
        property $
            forAll genModuleName $ \m ->
                all
                    (either (m `T.isInfixOf`) (const False) . browseOutcome "")
                    (errorPages m)

    it "a warning beside a listing does not make the listing a failure" $
        property $
            forAll genListing $ \l ->
                browseOutcome l "ghci: warning: something\n" == Right l

cardTruthSpec :: Spec
cardTruthSpec = describe "a module card enumerates only what was enumerated" $ do
    let pkg = entry "somepkg" "1.0.0" ["Some.Module", "Some.Other"]

    it "a failed probe never yields exports" $
        property $
            forAll genModuleName $ \m ->
                all
                    (null . exportsOf . cardOf Nothing [pkg] pkg m . Left)
                    (errorPages m)

    it "a failed probe is disclosed as unenumerated" $
        property $
            forAll genModuleName $ \m ->
                all
                    ( (== Just (Bool False))
                        . coverageFlag
                        . cardOf Nothing [pkg] pkg m
                        . Left
                    )
                    (errorPages m)

    it "the card carries the reason the probe gave, not a claim of its own" $
        property $
            forAll genModuleName $ \m ->
                let card = cardOf Nothing [pkg] pkg m (Left ("boom " <> m))
                 in ("boom " <> m) `T.isInfixOf` rendered card

    it "an empty parse is never promoted to a claim about the module" $
        property $
            forAll genModuleName $ \m ->
                let cards =
                        [ cardOf Nothing [pkg] pkg m (Right "")
                        , cardOf Nothing [pkg] pkg m (Right "\8212 no decls \8212")
                        ]
                 in all
                        (\c -> not ("exposes nothing" `T.isInfixOf` rendered c))
                        cards
                        && all ((== Just (Bool False)) . coverageFlag) cards

    it "a real listing enumerates, and says nothing about coverage" $
        property $
            forAll ((,) <$> genModuleName <*> genListing) $ \(m, l) ->
                let card = cardOf Nothing [pkg] pkg m (Right l)
                 in not (null (exportsOf card)) && isNothing (coverageFlag card)

reExportTruthSpec :: Spec
reExportTruthSpec = describe "a re-exported module is reachable, and says so" $ do
    let dump owner reExporter m =
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
        distinct = do
            a <- genPackageName
            b <- genPackageName
            m <- genModuleName
            pure (a, b, m)

    it "a module an installed package re-exports resolves" $
        property $
            forAll distinct $ \(a, b, m) ->
                a /= b ==>
                    isJust (resolveModule (parsePackageDump (dump a b m)) m)

    it "the card names the other installed packages that expose it" $
        property $
            forAll distinct $ \(a, b, m) ->
                a /= b ==>
                    let idx = parsePackageDump (dump a b m)
                        own = head [p | p <- idx, peName p == a]
                        card = cardOf Nothing idx own m (Right "")
                     in field "exposedBy" card == Just (Array (pure (String b)))

    it "a package that does not expose it is not named" $
        property $
            forAll distinct $ \(a, b, m) ->
                a /= b ==>
                    let idx = parsePackageDump (dump a b m)
                        own = head [p | p <- idx, peName p == a]
                        card = cardOf Nothing idx own (m <> ".Elsewhere") (Right "")
                     in isNothing (field "exposedBy" card)

    -- The store registers every version it ever built; GHCi links the newest,
    -- so a card built from whichever the dump listed first reports a version
    -- the browse did not use.
    it "resolves to the newest installed entry, not the first listed" $
        property $
            forAll ((,) <$> genPackageName <*> genModuleName) $ \(p, m) ->
                let idx = [entry p v [m] | v <- ["0.3.3.7", "10.0.0", "2.1.0"]]
                 in fmap (peVersion . snd) (resolveModule idx m) == Just "10.0.0"
