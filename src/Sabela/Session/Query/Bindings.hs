{-# LANGUAGE OverloadedStrings #-}

{- | Telling the user's bindings apart from the session's own. A baseline is
captured once, and every later listing is scrubbed against it, either verbatim
or reduced to signatures for a fingerprint comparison.
-}
module Sabela.Session.Query.Bindings (
    captureBindingsBaseline,
    scrubBindings,
    scrubBindingShapes,
    bindingSignature,
    groupEntries,
    itFingerprint,
) where

import Data.Char (isSpace)
import Data.IORef (writeIORef)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.Session (Session (..))
import Sabela.Session.Query.Command (QueryCommand (..), runQueryCommand)

captureBindingsBaseline :: Session -> IO ()
captureBindingsBaseline sess = do
    raw <- runQueryCommand sess QueryBindings
    writeIORef (sessBaselineBindings sess) (groupEntries raw)

scrubBindings :: [Text] -> Text -> Text
scrubBindings baseline = renderScrubbedBindings baseline id

scrubBindingShapes :: [Text] -> Text -> Text
scrubBindingShapes baseline = renderScrubbedBindings baseline bindingSignature

renderScrubbedBindings :: [Text] -> (Text -> Text) -> Text -> Text
renderScrubbedBindings baseline norm current =
    T.intercalate "\n" (map norm (filter keep (groupEntries current)))
  where
    keep e = e `notElem` baseline && not (isHidden (T.strip e))
    isHidden s =
        any
            (`T.isPrefixOf` s)
            ["it ::", "it =", "_sab", "instance "]

bindingSignature :: Text -> Text
bindingSignature entry =
    case T.breakOn " = " entry of
        (sig, rest) | not (T.null rest) -> T.stripEnd sig
        _ -> entry

groupEntries :: Text -> [Text]
groupEntries = map (T.intercalate "\n") . collect . T.lines
  where
    collect [] = []
    collect (l : ls)
        | starts l = let (cont, rest) = span continues ls in (l : cont) : collect rest
        | otherwise = collect ls
    starts x = not (T.null x) && not (isSpace (T.head x))
    continues x = not (T.null x) && isSpace (T.head x)

itFingerprint :: Text -> Text
itFingerprint =
    T.intercalate "\n"
        . filter ("it ::" `T.isPrefixOf`)
        . map T.strip
        . groupEntries
