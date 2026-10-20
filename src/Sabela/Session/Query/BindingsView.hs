{-# LANGUAGE OverloadedStrings #-}

{- | Turning a scrubbed @:show bindings@ listing into structured entries for
the editor's info panel, and reading the heap figures the RTS statement
prints. Pure; the handler in "Sabela.Server.Bindings" supplies the text.
-}
module Sabela.Session.Query.BindingsView (
    parseBindings,
    parseEntry,
    parseRtsStats,
    rtsStatsStatement,
    valueCharBound,
) where

import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)

import Sabela.Api.Bindings (BindingEntry (..), MemoryStats (..))
import Sabela.Session.Query.Bindings (groupEntries)

-- | Longest value text kept per entry; GHCi shows evaluated values in full.
valueCharBound :: Int
valueCharBound = 160

-- | Entries in GHCi order, each marked live when a current cell defines it.
parseBindings :: [Text] -> Text -> [BindingEntry]
parseBindings defined = mapMaybe (fmap mark . parseEntry) . groupEntries
  where
    mark e = e{beLive = bareName (beName e) `elem` map bareName defined}

{- | One grouped entry. Kind signatures (@type T :: *@) are dropped: GHCi
prints one before every declaration and the declaration itself follows.
-}
parseEntry :: Text -> Maybe BindingEntry
parseEntry entry = case T.words collapsed of
    (kw : name : _)
        | kw `elem` ["data", "newtype", "class"] ->
            Just (decl kw name)
        | kw == "type" && " = " `T.isInfixOf` collapsed ->
            Just (decl kw name)
        | kw == "type" -> Nothing
    _ -> valueEntry collapsed
  where
    collapsed = T.unwords (map T.strip (T.lines entry))
    decl kw name = BindingEntry name kw collapsed Nothing False

valueEntry :: Text -> Maybe BindingEntry
valueEntry line = case T.breakOn " :: " line of
    (name, rest)
        | T.null rest || T.null (T.strip name) || T.any (== ' ') (T.strip name) ->
            Nothing
        | otherwise ->
            let (ty, val) = T.breakOn " = " (T.drop 4 rest)
             in Just
                    ( BindingEntry
                        (T.strip name)
                        "value"
                        (T.strip ty)
                        (shownValue (T.strip (T.drop 3 val)))
                        False
                    )

shownValue :: Text -> Maybe Text
shownValue v
    | T.null v || v == "_" = Nothing
    | T.length v > valueCharBound = Just (T.take valueCharBound v <> "…")
    | otherwise = Just v

bareName :: Text -> Text
bareName n = maybe n (T.dropWhileEnd (== ')')) (T.stripPrefix "(" n)

{- | A GHCi bind statement, so @it@ is left alone and nothing echoes; the
@_sab@ prefix keeps the binding out of the user's listing. Prints one line:
@SABELA_RTS live maxLive memInUse gcs@, or @SABELA_RTS off@ without @-T@.
-}
rtsStatsStatement :: Text
rtsStatsStatement =
    "_sabRtsStats <- GHC.Stats.getRTSStatsEnabled >>= \\e -> if e then \
    \GHC.Stats.getRTSStats >>= \\s -> putStrLn (\"SABELA_RTS \" ++ unwords \
    \[show (GHC.Stats.gcdetails_live_bytes (GHC.Stats.gc s)), \
    \show (GHC.Stats.max_live_bytes s), \
    \show (GHC.Stats.gcdetails_mem_in_use_bytes (GHC.Stats.gc s)), \
    \show (GHC.Stats.gcs s)]) else putStrLn \"SABELA_RTS off\""

parseRtsStats :: Text -> Maybe MemoryStats
parseRtsStats out =
    case [ws | l <- T.lines out, ("SABELA_RTS" : ws) <- [T.words l]] of
        ([a, b, c, d] : _) ->
            MemoryStats <$> num a <*> num b <*> num c <*> num d
        _ -> Nothing
  where
    num = readMaybe . T.unpack
