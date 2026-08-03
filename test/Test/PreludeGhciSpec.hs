{-# LANGUAGE OverloadedStrings #-}

{- | The compiler's own answer to the question the static checker models: with
a generated module already imported unqualified, does the harness's injected
code still load, and can the user still hide the collision it caused?
-}
module Test.PreludeGhciSpec (spec) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Test.Hspec
import Test.QuickCheck (Gen, arbitrary, generate)

import Sabela.Bridge (widgetPreamble)
import Sabela.Output (displayPrelude)
import Test.Live (liveSpecs, requireExecutable)
import Test.PreludeAmbient (AmbientModule (..), ambientSource, collidingNames)

spec :: Spec
spec = liveSpecs $
    describe "the harness's injection under a real ambient import" $
        it "loads, runs and stays hideable in a real GHCi" $ do
            requireExecutable "ghci"
            ambient <- generate ambientForPrelude
            out <- runProbe ambient
            report out (checks out)

checks :: Text -> [(String, Bool)]
checks out =
    [ ("the injected prelude loaded", has preludeMark)
    , ("the widget preamble ran", has preambleMark)
    , ("the user's hiding clause resolved every collision", has hidingMark)
    , ("nothing was ambiguous", not (has "Ambiguous occurrence"))
    , ("nothing errored", not (has "error:"))
    ]
  where
    has = (`T.isInfixOf` out)

report :: Text -> [(String, Bool)] -> Expectation
report out results = case [label | (label, ok) <- results, not ok] of
    [] -> pure ()
    failed ->
        expectationFailure
            (unlines failed <> "\nghci said:\n" <> T.unpack out)

{- | A module exporting every ambient name the harness's injected code depends
on, so the collision is the largest one that code can suffer. Its name is
generated: the property is about any such module, not about a fixture.
-}
ambientForPrelude :: Gen AmbientModule
ambientForPrelude = do
    m <- arbitrary
    pure (m{ambientExports = collidingNames displayPrelude})

runProbe :: AmbientModule -> IO Text
runProbe ambient =
    withSystemTempDirectory "sabela-prelude-scope" $ \dir -> do
        let modPath = dir </> T.unpack (ambientName ambient) <> ".hs"
        writeFile modPath (T.unpack (ambientSource ambient))
        (_, out, err) <-
            readProcessWithExitCode
                "ghci"
                ["-v0", "-ignore-dot-ghci", "-i" <> dir]
                (T.unpack (probeScript modPath ambient))
        pure (T.pack out <> T.pack err)

{- | One session, in the order the loss happened in: the ambient module is
imported first, the harness's code goes in after it, and only then does the
user reach for the remedy the episode's other surface found unaided.
-}
probeScript :: FilePath -> AmbientModule -> Text
probeScript modPath ambient =
    T.unlines
        ( [ ":load " <> T.pack modPath
          , ":module"
          , "import qualified System.IO as ProbeIO"
          , "import " <> ambientName ambient
          , displayPrelude
          , mark preludeMark
          , widgetPreamble 0 (M.fromList [("probe", "[]")])
          , mark preambleMark
          , "import Prelude hiding (" <> T.intercalate ", " names <> ")"
          ]
            <> ["ProbeIO.print " <> n | n <- names]
            <> [mark hidingMark]
        )
  where
    names = ambientExports ambient

mark :: Text -> Text
mark m = "ProbeIO.putStrLn " <> T.pack (show (T.unpack m))

preludeMark, preambleMark, hidingMark :: Text
preludeMark = "SABELA-PRELUDE-LOADED"
preambleMark = "SABELA-PREAMBLE-RAN"
hidingMark = "SABELA-HIDING-RESOLVED"
