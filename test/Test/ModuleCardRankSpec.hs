{-# LANGUAGE OverloadedStrings #-}

{- | The query reaches the ranker, or the card is ordered by nothing: the
module-browse path handed 'Nothing' to a scorer that then collapsed to input
order. Driven end to end against a session that answers with one listing.
-}
module Test.ModuleCardRankSpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Unique (newUnique)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Sabela.AI.Capabilities.BrowseCard (browseCardFor)
import Sabela.AI.Capabilities.ModuleCard.Card (moduleCardValue)
import Sabela.AI.Capabilities.ModuleSearch (execFindFunction)
import Sabela.AI.PackageIndex (PackageEntry (..))
import Sabela.AI.Types (ToolOutcome, toolOutcomeValue)
import Sabela.Server (newApp)
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..))
import Sabela.State.SessionManager (setHaskellSession)

spec :: Spec
spec = do
    describe "a card ranks against the query that produced it" $
        it "leads with the export that produces the type asked about" $ do
            out <- browseThrough "Widgetry" widgetryListing
            take 1 (exportsOf (toolOutcomeValue out))
                `shouldBe` ["zetaBuild :: Int -> Widgetry"]
    pragmaTailSpec

{- | A5: a pragma is a directive about a declaration, not part of it. Glued to
the last member of a class or record body it states a type no signature gave,
and both card renderers read a listing through one decomposition.
-}
pragmaTailSpec :: Spec
pragmaTailSpec = describe "no emitted signature carries a pragma tail (A5)" $ do
    prop "neither renderer states a row with pragma text in it" $
        forAll genPragmaListing $ \(raw, _) ->
            let rows = renderedRows raw
             in counterexample (show rows) (not (any (T.isInfixOf "{-#") rows))
    prop "the signature the pragma followed is the one the listing declared" $
        forAll genPragmaListing $ \(raw, want) ->
            let rows = renderedRows raw
             in counterexample (show rows) (all (`elem` rows) want)
    it "and the card the browse path builds end to end carries neither" $ do
        (raw, want) <- generate genPragmaListing
        rows <- exportsOf . toolOutcomeValue <$> browseThrough pragmaModule raw
        rows `shouldSatisfy` (not . any (T.isInfixOf "{-#"))
        map subject rows `shouldSatisfy` (\ns -> all ((`elem` ns) . subject) want)

-- | The entity a row is about, which is what two renderers agree on.
subject :: Text -> Text
subject = T.takeWhile (/= ' ')

pragmaModule :: Text
pragmaModule = "Some.Module"

renderedRows :: Text -> [Text]
renderedRows raw =
    concatMap
        exportsOf
        [ browseCardFor Nothing pragmaModule raw
        , moduleCardValue Nothing [pkg] pkg pragmaModule Nothing "" (Right raw)
        ]
  where
    pkg = PackageEntry "somepkg" "1.0.0" "" [pragmaModule]

{- | A listing whose class body and record body each carry the pragma GHCi
prints after them, beside the rows the listing declares. Both bodies matter:
one glues the pragma to a method's type, the other stands after a selector's.
-}
genPragmaListing :: Gen (Text, [Text])
genPragmaListing = do
    c <- genConName
    a <- genLowerName
    b <- genLowerName `suchThat` (/= a)
    directive <- elements ["MINIMAL " <> a, "COMPLETE " <> c]
    pure
        ( T.unlines
            [ "class " <> c <> " x where"
            , "  " <> a <> " :: x -> Int"
            , "  " <> b <> " :: x -> Bool"
            , "  {-# " <> directive <> " #-}"
            , "data " <> c <> "R = " <> c <> "R {" <> a <> "F :: Int}"
            ]
        ,
            [ a <> " :: (" <> c <> " x) => x -> Int"
            , b <> " :: (" <> c <> " x) => x -> Bool"
            , a <> "F :: " <> c <> "R -> Int"
            ]
        )

genConName :: Gen Text
genConName = (\h t -> T.pack (h : t)) <$> elements ['A' .. 'Z'] <*> lowers

genLowerName :: Gen Text
genLowerName = (\h t -> T.pack (h : t)) <$> elements ['a' .. 'z'] <*> lowers

lowers :: Gen String
lowers = choose (2, 6) >>= \n -> vectorOf n (elements ['a' .. 'z'])

{- | A listing whose relevant rows sort last and whose producer and consumer are
the same length, so only relevance can separate them.
-}
widgetryListing :: Text
widgetryListing =
    T.unlines
        [ "alphaHelper :: Int -> Int"
        , "betaHelper :: Int -> Int"
        , "consumeIt :: Widgetry -> Int"
        , "zetaBuild :: Int -> Widgetry"
        ]

exportsOf :: Value -> [Text]
exportsOf v = case v of
    Object o -> case KM.lookup (Key.fromText "exports") o of
        Just (Array es) -> [s | String s <- foldr (:) [] es]
        _ -> []
    _ -> []

-- | Drive @find_function@ against a session that answers with one listing.
browseThrough :: Text -> Text -> IO ToolOutcome
browseThrough modName raw = do
    mgr <- newManager defaultManagerSettings
    app <- newApp "." Set.empty (Just mgr) Nothing []
    backend <- listingBackend modName raw
    setHaskellSession (appSessions app) (Just backend)
    execFindFunction app (object ["query" .= modName])

listingBackend :: Text -> Text -> IO ST.SessionBackend
listingBackend modName raw = do
    uid <- newUnique
    let backend =
            ST.SessionBackend
                { ST.sbSessionId = uid
                , ST.sbJsonDiagnostics = False
                , ST.sbRunBlock = \_ -> pure ("", "")
                , ST.sbRunBlockStreaming = \_ _ -> pure ("", "")
                , ST.sbClose = pure ()
                , ST.sbReset = pure backend
                , ST.sbInterrupt = pure ()
                , ST.sbBusy = pure False
                , ST.sbSessionGen = pure 0
                , ST.sbRequestStale = \_ -> pure False
                , ST.sbQueryComplete = \_ -> pure [modName]
                , ST.sbQueryType = \_ -> pure ""
                , ST.sbQueryInfo = \_ -> pure ""
                , ST.sbQueryKind = \_ -> pure ""
                , ST.sbQueryBrowse = \m -> pure (if m == modName then raw else "")
                , ST.sbQueryBindings = pure ""
                , ST.sbQueryDoc = \_ -> pure ""
                , ST.sbQueryHoleFits = \_ -> pure ""
                , ST.sbEvalPureLive = \req ->
                    pure (ST.pureEvalUnavailableResult req "listing backend")
                }
    pure backend
