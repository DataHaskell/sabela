{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Type-directed display: the source injected into every session, and the
GHCi settings that put it in charge of printing. The Haskell it injects lives
in @prelude/display.hs@, where it is real source rather than escaped string
literals.
-}
module Sabela.Output.Display (
    attributeInteractivePrint,
    displayDefs,
    displayImports,
    displayPragmas,
    interactivePrintName,
    interactivePrintSetting,
) where

import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Sabela.Output.Grid (gridDefs)

{- | The class, its instances, and the renderers they reach for, with the
table view's runtime spliced in beside them.
-}
displayDefs :: Text
displayDefs = gridDefs <> "\n" <> embeddedDisplay <> "\n" <> embeddedFrames

embeddedDisplay :: Text
embeddedDisplay =
    TE.decodeUtf8
        $(makeRelativeToProject "prelude/display.hs" >>= embedFile)

-- | The frame registry and query behind a table's expression box.
embeddedFrames :: Text
embeddedFrames =
    TE.decodeUtf8
        $(makeRelativeToProject "prelude/frames.hs" >>= embedFile)

{- | Generic representations, under the same one-alias discipline as the rest
of the injected block.
-}
displayImports :: [Text]
displayImports = ["import qualified GHC.Generics as SabelaGen"]

{- | What the injected block needs to compile, set on the session rather than
in the notebook's cabal metadata: a @:set@ costs no environment rebuild, and
these are the extensions a user's own @Display@ or @Tabular@ instance wants
anyway.
-}
displayPragmas :: [Text]
displayPragmas =
    [ ":set"
        <> mconcat
            [ " -X" <> ext
            | ext <-
                [ "DefaultSignatures"
                , "DeriveAnyClass"
                , "DeriveGeneric"
                , "FlexibleContexts"
                , "FlexibleInstances"
                , "TypeOperators"
                , "TypeSynonymInstances"
                , "UndecidableInstances"
                ]
            ]
    ]

{- | The printer GHCi calls for a trailing expression. Underscore-prefixed so
a user cell that binds a @display@ of its own cannot unseat it.
-}
interactivePrintName :: Text
interactivePrintName = "_sabelaPrint"

interactivePrintSetting :: Text
interactivePrintSetting = ":set -interactive-print=" <> interactivePrintName

{- | GHCi names the printer it used when it reports a value it cannot show.
The printer is the harness's own, so the frames naming it are rewritten to the
name a stock session would have used: a reader is told about its own cell, not
about us. Only those frames are rewritten, so a listing that offers the binder
still reads as a harness binder and is still dropped.
-}
attributeInteractivePrint :: Text -> Text
attributeInteractivePrint t
    | not (interactivePrintName `T.isInfixOf` t) = t
    | otherwise = foldr (uncurry T.replace) t frames
  where
    frames =
        [ ("\8216" <> interactivePrintName <> "\8217", "\8216print\8217")
        , ("'" <> interactivePrintName <> "'", "'print'")
        , ("command: " <> interactivePrintName <> " it", "command: print it")
        ]
