{-# LANGUAGE OverloadedStrings #-}

{- | Reading structure out of a GHCi @:type@/@:info@ dump: the type
constructors a signature mentions, a record declaration with its field names,
and the classes a type instantiates.
-}
module Sabela.AI.Capabilities.Query.Struct (
    typeConstructors,
    recordDecl,
    instanceClasses,
    typeStructure,
    withInstances,
    looksResolved,
) where

import Data.Char (isUpper)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.LeakShape (leakyLine)
import Sabela.AI.VerifierDistill (distillInfo)
import Sabela.SessionTypes (SessionBackend (..))

withInstances :: Text -> Text -> Text
withInstances raw answer = case instanceClasses raw of
    [] -> answer
    cs -> answer <> "\n\ninstances: " <> T.intercalate ", " cs

looksResolved :: Text -> Bool
looksResolved t =
    let lt = T.toLower t
     in not (T.null (T.strip t))
            && not ("not in scope" `T.isInfixOf` lt)
            && not ("error:" `T.isInfixOf` lt)
            && not ("illegal term-level" `T.isInfixOf` lt)

typeStructure :: SessionBackend -> Text -> IO Text
typeStructure backend = go . take 4 . candidates
  where
    go [] = pure ""
    go (c : cs) = do
        decl <- recordDecl . distillInfo <$> sbQueryInfo backend c
        maybe (go cs) pure decl
    candidates = concatMap variants . typeConstructors
    variants t =
        let bare = lastSeg t
         in if bare == t then [t] else [t, bare]

instanceClasses :: Text -> [Text]
instanceClasses info =
    nubKeep
        [ cls
        | l <- map T.strip (T.lines info)
        , Just rest <- [T.stripPrefix "instance " l]
        , cls : _ <- [T.words (dropContext (fst (T.breakOn "--" rest)))]
        , maybe False (isUpper . fst) (T.uncons cls)
        ]
  where
    dropContext r = case T.breakOn "=>" r of
        (_, m) | not (T.null m) -> T.strip (T.drop 2 m)
        _ -> r

lastSeg :: Text -> Text
lastSeg = last . T.splitOn "."

typeConstructors :: Text -> [Text]
typeConstructors s = nubKeep (filter isCtorAtom atoms)
  where
    rhs = case T.breakOn "::" s of
        (_, r) | not (T.null r) -> T.drop 2 r
        _ -> s
    atoms =
        filter (not . T.null) $
            T.split (`elem` (" \t\n[]()->,!{}=|" :: String)) rhs
    isCtorAtom t = maybe False (isUpper . fst) (T.uncons (lastSeg t))

recordDecl :: Text -> Maybe Text
recordDecl info
    | hasAdt && hasField = Just kept
    | otherwise = Nothing
  where
    kept = T.intercalate "\n" (filter keep (T.lines info))
    keep l =
        let t = T.strip l
         in not (T.null t)
                && not (leakyLine t)
                && not ("instance " `T.isPrefixOf` t)
                && not ("-- Defined in" `T.isInfixOf` t)
    hasAdt = "data " `T.isInfixOf` kept || "newtype " `T.isInfixOf` kept
    hasField = "{" `T.isInfixOf` kept

nubKeep :: (Eq a) => [a] -> [a]
nubKeep = foldr (\x acc -> x : filter (/= x) acc) []
