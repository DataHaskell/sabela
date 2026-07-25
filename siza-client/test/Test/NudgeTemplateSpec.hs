{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{- | G3 task 1: no nudge template prescribes incomplete code. Two independent
guards, neither a hand-maintained string list: a PROPERTY over the real
generators, run across a grid of ledgers, drafts and probe states; and a
SOURCE scan of every model-facing agent module for a hole binder in a
literal, so a template that reintroduces one fails here before it ever
reaches a transcript.
-}
module Test.NudgeTemplateSpec (nudgeTemplateSpec) where

import Control.Monad (forM_)
import Data.Aeson (Value (..))
import qualified Data.Aeson.KeyMap as KM
import Data.List (isSuffixOf, subsequences)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import Test.Hspec

import Siza.Agent.Discover.Candidate (candidateClause, candidateClauseFrom)
import Siza.Agent.Discover.MissLadder (missAdvice, withCandidate)
import Siza.Agent.Loop.Escalate (forceWriteMsgWith)
import Siza.Agent.Loop.Support (forceActMsgWith)

-- | The hole binder no model-facing template may contain.
holeBinder :: Text
holeBinder = "_ ::"

-- | Held-fact shapes: consumers with gaps, all-literal consumers, installs,
-- and the probe conclusions that turn a gap into a fill.
factPool :: [Text]
factPool =
    [ "`bars` :: [(Text, Double)] -> Plot -> Text — found in Cumulus.Plot (cumulus)"
    , "`gust` :: Int -> Int — found in Zephyr.Core (zephyr)"
    , "`render` :: Nimbus -> Text — found in Nimbus.Draw (nimbus)"
    , "cumulus (hidden): -- cabal: build-depends: cumulus — provides `bars`"
    , "`Plot` is produced by: `defaultPlot`, `mkPlot` (via: hole-probe)"
    , "no producer of `Nimbus` found in scope (via: hole-probe)"
    ]

factGrid :: [[Text]]
factGrid = [fs | fs <- subsequences factPool, length fs <= 3]

-- | Draft seeds the loop may hold, including one the model holed itself.
draftGrid :: [Maybe Text]
draftGrid =
    [ Nothing
    , Just ""
    , Just "import Cumulus.Plot\nbars [(\"a\", 1.0)] defaultPlot"
    , Just "import Cumulus.Plot\nbars [(\"a\", 1.0)] (_ :: Plot)"
    ]

-- | Every text a nudge/close generator can put in front of the model.
generatedTexts :: [Text]
generatedTexts =
    concat
        [ [candidateClause fs | fs <- factGrid]
        , [candidateClauseFrom d fs | fs <- factGrid, d <- draftGrid]
        , [contentOf (forceActMsgWith fs "3 turns left.") | fs <- factGrid]
        , [ contentOf (forceWriteMsgWith mempty d fs "3 turns left.")
          | fs <- factGrid
          , d <- draftGrid
          ]
        , [renderValue (withCandidate mempty fs missEnvelope) | fs <- factGrid]
        , [ renderValue (missAdvice goal mempty mempty fs Nothing [] rung "plot" missEnvelope)
          | fs <- factGrid
          , goal <- [Nothing, Just "Plot"]
          , rung <- [1 .. 5]
          ]
        ]

missEnvelope :: Value
missEnvelope =
    Object
        ( KM.fromList
            [ ("query", String "plot")
            , ("state", String "not_found")
            , ("next", String "No match for 'plot'.")
            ]
        )

contentOf :: Value -> Text
contentOf (Object o) = case KM.lookup "content" o of
    Just (String s) -> s
    _ -> ""
contentOf _ = ""

-- | Every string in an envelope, so no nested advice field escapes the check.
renderValue :: Value -> Text
renderValue (String s) = s
renderValue (Object o) = T.unlines [renderValue v | (_, v) <- KM.toList o]
renderValue (Array a) = T.unlines (map renderValue (foldr (:) [] a))
renderValue _ = ""

-- | A text recommends a write when it names one of the write tools.
recommendsWrite :: Text -> Bool
recommendsWrite t = any (`T.isInfixOf` t) ["insert_cell", "replace_cell_source"]

-- | The agent modules whose literals reach the model.
agentSourceDir :: FilePath
agentSourceDir = "src" </> "Siza" </> "Agent"

{- | The only modules allowed a hole binder: those that ASK the compiler one
(a @find_by_type@ goal, a @try@ probe). Every other module — present or
future — is banned by default, so a new template cannot drift past this.
-}
compilerQueryModules :: [FilePath]
compilerQueryModules =
    [ "Discover/Construct.hs"
    , "Discover/HoleProbe.hs"
    , "Repair.hs"
    ]

asksTheCompiler :: FilePath -> Bool
asksTheCompiler f = any (`isSuffixOf` f) compilerQueryModules

haskellSourcesUnder :: FilePath -> IO [FilePath]
haskellSourcesUnder dir = do
    entries <- listDirectory dir
    fmap concat (mapM (expand . (dir </>)) entries)
  where
    expand p = do
        isDir <- doesDirectoryExist p
        if isDir
            then haskellSourcesUnder p
            else pure [p | ".hs" `isSuffixOf` p]

{- | Lines that put a hole binder inside a double-quoted literal. Haddock and
ordinary comments are exempt: they document the ban, they do not emit it.
-}
holeLiteralLines :: Text -> [Text]
holeLiteralLines src =
    [ l
    | l <- T.lines src
    , holeBinder `T.isInfixOf` l
    , "\"" `T.isInfixOf` l
    , not ("--" `T.isPrefixOf` T.strip l)
    ]

-- | The (file, line) pairs of a source's banned hole-bearing literals.
offendersIn :: FilePath -> IO [(FilePath, Text)]
offendersIn f = map (f,) . holeLiteralLines <$> TIO.readFile f

nudgeTemplateSpec :: Spec
nudgeTemplateSpec = describe "G3: no nudge template prescribes incomplete code" $ do
    it "the generator grid is non-trivial (the property has something to check)" $
        length (filter (not . T.null) generatedTexts) `shouldSatisfy` (> 50)

    it "no generated nudge text contains a hole binder" $
        forM_ generatedTexts $ \t ->
            (t, holeBinder `T.isInfixOf` t) `shouldBe` (t, False)

    it "no generated text that recommends a write hands back incomplete code" $
        forM_ (filter recommendsWrite generatedTexts) $ \t ->
            (t, holeBinder `T.isInfixOf` t) `shouldBe` (t, False)

    it "a model draft holding a hole is never handed back for transcription" $ do
        let holed = Just "bars [(\"a\", 1.0)] (_ :: Plot)"
        forM_ factGrid $ \fs ->
            candidateClauseFrom holed fs
                `shouldSatisfy` (not . T.isInfixOf holeBinder)

    it "no agent source literal outside the compiler-query modules holds a hole binder" $ do
        present <- doesDirectoryExist agentSourceDir
        if not present
            then pendingWith "agent sources not reachable from the test cwd"
            else do
                files <- filter (not . asksTheCompiler) <$> haskellSourcesUnder agentSourceDir
                files `shouldSatisfy` (not . null)
                offenders <- concat <$> mapM offendersIn files
                offenders `shouldBe` []

    it "the exempt modules exist and really are the ones that ask the compiler" $ do
        present <- doesDirectoryExist agentSourceDir
        if not present
            then pendingWith "agent sources not reachable from the test cwd"
            else do
                files <- haskellSourcesUnder agentSourceDir
                let exempt = filter asksTheCompiler files
                length exempt `shouldBe` length compilerQueryModules
