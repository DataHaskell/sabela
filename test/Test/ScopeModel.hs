{-# LANGUAGE OverloadedStrings #-}

{- | GHCi's import-scope rule, as a model the scope properties reason over and
as an observation of a real session, so the model is checked rather than
assumed.
-}
module Test.ScopeModel (
    preludeScope,
    applyScopeLine,
    observedScope,
    earlierModule,
    askedModule,
) where

import Data.Text (Text)
import qualified Data.Text as T
import System.Process (readProcessWithExitCode)

import Sabela.AI.Capabilities.Edit.ScratchVet (cellImportLines)
import Sabela.Output (displayPrelude)
import Test.TryFrontierGen (moduleOf)

{- | The modules a scratchpad has in scope before any caller asks for one, read
off the prelude it is initialised with.
-}
preludeScope :: [Text]
preludeScope = map moduleOf (cellImportLines displayPrelude)

{- | GHCi's scope rules, as far as these properties need them: a bare @:module@
withdraws every module a caller added, and an import line adds one.
-}
applyScopeLine :: [Text] -> Text -> [Text]
applyScopeLine scope line
    | stripped == ":module" = []
    | "import " `T.isPrefixOf` stripped = scope <> [moduleOf stripped]
    | otherwise = scope
  where
    stripped = T.strip line

{- | The scope a real GHCi reports after running these blocks, so the rule the
model above encodes is observed rather than assumed.
-}
observedScope :: [Text] -> IO [Text]
observedScope blocks = do
    (_, out, _) <-
        readProcessWithExitCode
            "ghci"
            ["-v0", "-ignore-dot-ghci"]
            (T.unpack (T.unlines (blocks <> [":show imports"])))
    pure
        [ moduleOf (T.strip (T.replace "-- implicit" "" line))
        | line <- T.lines (T.pack out)
        , "import " `T.isPrefixOf` T.strip line
        ]

-- | Two base modules an establish-line probe can name without naming a library.
earlierModule, askedModule :: Text
earlierModule = "Data.Char"
askedModule = "Data.Bits"
