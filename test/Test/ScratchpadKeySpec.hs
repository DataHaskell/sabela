{-# LANGUAGE OverloadedStrings #-}

module Test.ScratchpadKeySpec (spec) where

import Control.Exception (SomeException, try)
import Data.IORef (newIORef)
import Data.List (sort)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client (defaultManagerSettings, newManager)
import System.FilePath ((</>))
import Test.Hspec
import Test.QuickCheck

import Test.ScratchScopeSpec (recordingBackend)

import qualified ScriptHs.Parser as Scripths

import Sabela.AI.Capabilities.Scratchpad (ensureScratchpad)
import Sabela.AI.Capabilities.Scratchpad.Key (
    ScratchpadPlan (..),
    scratchpadIdentity,
    scratchpadKey,
    scratchpadPlan,
 )
import Sabela.AI.Store (AIStore, newAIStore, setScratchpad)
import Sabela.AI.Types (ScratchpadSession (..))
import Sabela.Anthropic.Types (AnthropicConfig (..))
import Sabela.Model (Cell (..), CellType (..), Notebook (..))
import Sabela.Session.GhcProbe (resolvedGhcVersion)
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..), newApp)
import Sabela.State.Environment (Environment (..))
import Sabela.State.NotebookStore (modifyNotebook)

{- | A metadata token: any non-blank word a directive could carry. Stated over
arbitrary tokens so no package, extension or flag name is load-bearing.
-}
genToken :: Gen Text
genToken =
    T.pack <$> listOf1 (elements (['a' .. 'z'] ++ ['A' .. 'Z'] ++ "0123456789-."))

genTokens :: Gen [Text]
genTokens = resize 5 (listOf genToken)

emptyMeta :: Scripths.CabalMeta
emptyMeta =
    Scripths.CabalMeta
        { Scripths.metaDeps = []
        , Scripths.metaExts = []
        , Scripths.metaGhcOptions = []
        , Scripths.metaExtraLibDirs = []
        , Scripths.metaExtraIncludeDirs = []
        , Scripths.metaPackages = []
        , Scripths.metaSourceRepos = []
        , Scripths.metaUnknownKeys = []
        }

keyOf :: [Text] -> Scripths.CabalMeta -> [Text]
keyOf = scratchpadKey "9.6.7" []

-- | A notebook whose only cell declares the directive values it is given.
notebookWith :: [Text] -> Notebook
notebookWith vals = Notebook "spec" [cell]
  where
    cell =
        Cell
            { cellId = 0
            , cellType = CodeCell
            , cellLang = ST.Haskell
            , cellSource =
                T.unlines ["-- cabal: default-extensions: " <> v | v <- vals]
            , cellOutputs = []
            , cellError = Nothing
            , cellDirty = False
            }

identityOf :: Notebook -> [Text] -> (Scripths.CabalMeta, [Text])
identityOf nb deps = let (meta, _, key) = idAt "9.6.7" nb deps in (meta, key)

-- | The key ensureScratchpad itself derives: the RESOLVED compiler version.
liveIdentityKey :: Notebook -> [Text] -> IO [Text]
liveIdentityKey nb deps =
    (\v -> let (_, _, key) = idAt v nb deps in key) <$> resolvedGhcVersion

idAt :: Text -> Notebook -> [Text] -> (Scripths.CabalMeta, [FilePath], [Text])
idAt v = scratchpadIdentity v "." [] ST.Haskell Set.empty

fakeBackend :: IO ST.SessionBackend
fakeBackend = do
    ran <- newIORef []
    recordingBackend ran (const ("", ""))

liveSession :: [Text] -> IO ScratchpadSession
liveSession key = do
    backend <- fakeBackend
    pure (ScratchpadSession backend "" ST.Haskell key)

{- | An app whose scratchpad directory does not exist, so building a fresh
session fails at its first step: reuse is a returned backend, eviction is a
thrown exception, and neither needs a compiler.
-}
mkFixture :: IO (App, AIStore)
mkFixture = do
    mgr <- newManager defaultManagerSettings
    app0 <- newApp "." Set.empty (Just mgr) Nothing []
    let env = appEnv app0
        app = app0{appEnv = env{envTmpDir = envTmpDir env </> "absent"}}
        cfg =
            AnthropicConfig
                { acApiKey = ""
                , acModel = "placeholder"
                , acBaseUrl = "https://api.anthropic.com"
                }
    store <- newAIStore cfg mgr
    pure (app, store)

reuses :: App -> AIStore -> ScratchpadSession -> [Text] -> IO Bool
reuses app store sp deps = do
    setScratchpad store (Just sp)
    r <-
        try (ensureScratchpad app store ST.Haskell deps) ::
            IO (Either SomeException ST.SessionBackend)
    pure $ case r of
        Right backend -> ST.sbSessionId backend == ST.sbSessionId (spBackend sp)
        Left _ -> False

isReuse :: ScratchpadPlan -> Bool
isReuse (ReuseScratchpad _) = True
isReuse _ = False

spec :: Spec
spec = describe "scratchpad reuse key (intention)" $ do
    describe "no needless eviction" $ do
        it "is invariant under the order a declaration was written in" $
            property $ \(Positive n) -> forAll genTokens $ \ts ->
                let rotated = drop (n `mod` (length ts + 1)) ts ++ take (n `mod` (length ts + 1)) ts
                 in keyOf [] emptyMeta{Scripths.metaDeps = rotated}
                        `shouldBe` keyOf [] emptyMeta{Scripths.metaDeps = ts}

        it "is invariant under repeating a declaration" $
            property $
                forAll genTokens $ \ts ->
                    keyOf [] emptyMeta{Scripths.metaDeps = ts ++ ts}
                        `shouldBe` keyOf [] emptyMeta{Scripths.metaDeps = ts}

        it "is invariant under surrounding whitespace" $
            property $
                forAll genTokens $ \ts ->
                    keyOf [] emptyMeta{Scripths.metaDeps = map (\t -> "  " <> t <> " ") ts}
                        `shouldBe` keyOf [] emptyMeta{Scripths.metaDeps = ts}

        it "is already canonical, so it never re-sorts to something else" $
            property $
                forAll genTokens $ \ts ->
                    let k = keyOf ts emptyMeta in sort k `shouldBe` k

    describe "no stale reuse" $ do
        it "changes when a dependency the notebook did not have appears" $
            property $
                forAll ((,) <$> genTokens <*> genToken) $ \(ts, t) ->
                    t `notElem` map T.strip ts ==>
                        keyOf [] emptyMeta{Scripths.metaDeps = t : ts}
                            /= keyOf [] emptyMeta{Scripths.metaDeps = ts}

        it "changes when a field other than build-depends changes" $
            property $
                forAll ((,) <$> genTokens <*> genToken) $ \(ts, t) ->
                    let base = emptyMeta{Scripths.metaDeps = ts}
                        fields =
                            [ \m v -> m{Scripths.metaExts = [v]}
                            , \m v -> m{Scripths.metaGhcOptions = [v]}
                            , \m v -> m{Scripths.metaExtraLibDirs = [v]}
                            , \m v -> m{Scripths.metaExtraIncludeDirs = [v]}
                            , \m v -> m{Scripths.metaPackages = [v]}
                            ]
                     in conjoin
                            [ keyOf [] (f base t) =/= keyOf [] base
                            | f <- fields
                            ]

        it "changes when a git pin changes" $
            property $
                forAll ((,) <$> genToken <*> genToken) $ \(loc, ref) ->
                    let pin r = Scripths.SourceRepoPin loc r Nothing
                        with r = emptyMeta{Scripths.metaSourceRepos = [pin r]}
                     in ref /= loc ==> keyOf [] (with ref) =/= keyOf [] (with loc)

    describe "fields are distinguished, not pooled" $
        it "the same token in two fields is not the same key" $
            property $
                forAll genToken $ \t ->
                    keyOf [] emptyMeta{Scripths.metaDeps = [t]}
                        =/= keyOf [] emptyMeta{Scripths.metaExts = [t]}

    describe "the caller's own deps join the notebook's" $
        it "a dep passed at the call site is folded into the same field" $
            property $
                forAll ((,) <$> genTokens <*> genToken) $ \(ts, t) ->
                    keyOf [t] emptyMeta{Scripths.metaDeps = ts}
                        `shouldBe` keyOf [] emptyMeta{Scripths.metaDeps = t : ts}

    describe "a session is looked up by the identity it was stored under" $ do
        it "reuses a session stored under the key its own plan yielded" $
            property $
                forAll ((,) <$> genTokens <*> genTokens) $ \(vals, deps) ->
                    ioProperty $ do
                        let nb = notebookWith vals
                        case scratchpadPlan "9.6.7" "." [] ST.Haskell Set.empty nb deps Nothing of
                            ReuseScratchpad _ -> pure False
                            FreshScratchpad _ _ key -> do
                                sp <- liveSession key
                                pure
                                    ( isReuse
                                        (scratchpadPlan "9.6.7" "." [] ST.Haskell Set.empty nb deps (Just sp))
                                    )

        it "does not reuse a session stored under the caller's dep list alone" $
            property $
                forAll ((,) <$> genTokens <*> genTokens) $ \(vals, deps) ->
                    ioProperty $ do
                        sp <- liveSession deps
                        pure
                            ( not
                                ( isReuse
                                    ( scratchpadPlan
                                        "9.6.7"
                                        "."
                                        []
                                        ST.Haskell
                                        Set.empty
                                        (notebookWith vals)
                                        deps
                                        (Just sp)
                                    )
                                )
                            )

        it "does not reuse a session stored before the notebook declared more" $
            property $
                forAll ((,) <$> genTokens <*> genToken) $ \(vals, v) ->
                    let nb = notebookWith vals
                        (meta, key) = identityOf nb []
                     in v `notElem` Scripths.metaExts meta ==> ioProperty $ do
                            sp <- liveSession key
                            pure
                                ( not
                                    ( isReuse
                                        ( scratchpadPlan
                                            "9.6.7"
                                            "."
                                            []
                                            ST.Haskell
                                            Set.empty
                                            (notebookWith (v : vals))
                                            []
                                            (Just sp)
                                        )
                                    )
                                )

    describe "the live scratchpad is kept exactly when its identity holds" $ do
        fixture <- runIO mkFixture

        it "keeps a session whose stored identity is the notebook's" $
            property $
                forAll ((,) <$> genTokens <*> genTokens) $ \(vals, deps) ->
                    ioProperty $ do
                        let (app, store) = fixture
                            nb = notebookWith vals
                        modifyNotebook (appNotebook app) (const nb)
                        sp <- liveSession =<< liveIdentityKey nb deps
                        reuses app store sp deps

        it "evicts a session identified only by the caller's dep list" $
            property $
                forAll ((,) <$> genTokens <*> genTokens) $ \(vals, deps) ->
                    ioProperty $ do
                        let (app, store) = fixture
                        modifyNotebook (appNotebook app) (const (notebookWith vals))
                        sp <- liveSession deps
                        not <$> reuses app store sp deps
