{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Capabilities.Edit.Assemble (
    applicationCandidates,
    probeArity,
    vacuous,
) where

import Data.Char (isAlphaNum)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.Edit.Repair (resultErrorText)
import Sabela.AI.Capabilities.Util (featureEnabled)
import Sabela.AI.Health (healthOfTypeQuery, isClean)
import Sabela.AI.HoleRepair (arityFromError)
import Sabela.AI.Spine (
    Spine (..),
    renderSpine,
    spineArity,
    splitSpine,
    trimTo,
 )
import Sabela.AI.Types (ExecutionResult (..))
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..), getHaskellSession)

maxProbeArity :: Int
maxProbeArity = 4

applicationCandidates ::
    App -> Either Text ExecutionResult -> Text -> IO [Text]
applicationCandidates app res src = do
    enabled <- featureEnabled "SABELA_ARITY_FIX"
    mBackend <- getHaskellSession (appSessions app)
    case (enabled, mBackend, arityFromError (resultErrorText res)) of
        (True, Just backend, Just _) -> candidatesFor backend
        _ -> pure []
  where
    candidatesFor backend = do
        fixes <- mapM (trimmed backend) (overApplied src)
        pure [s | Just s <- fixes, s /= src, not (vacuous s)]
    trimmed backend (whole, sp) = do
        mArity <- headArity backend (resultErrorText res) (spHead sp)
        pure $ case mArity of
            Just n
                | n < spineArity sp ->
                    Just (T.replace whole (parenthesise (trimTo n sp)) src)
            _ -> Nothing
    parenthesise sp = "(" <> renderSpine sp <> ")"

overApplied :: Text -> [(Text, Spine)]
overApplied src = mapMaybe withSpine (parenGroups src)
  where
    withSpine g = (,) g <$> splitSpine (unwrap g)
    unwrap g = T.dropEnd 1 (T.drop 1 g)

parenGroups :: Text -> [Text]
parenGroups = go . T.unpack
  where
    go [] = []
    go ('"' : cs) = go (skipStr cs)
    go ('(' : cs) = case grab 1 [] cs of
        Just (body, rest) ->
            let whole = T.pack ('(' : body)
             in whole : parenGroups (T.pack (init body)) ++ go rest
        Nothing -> []
    go (_ : cs) = go cs
    skipStr [] = []
    skipStr ('\\' : _ : cs) = skipStr cs
    skipStr ('"' : cs) = cs
    skipStr (_ : cs) = skipStr cs
    grab :: Int -> String -> String -> Maybe (String, String)
    grab _ _ [] = Nothing
    grab d acc ('"' : cs) =
        let (lit, rest) = spanStr cs
         in grab d (acc ++ '"' : lit) rest
    grab d acc ('(' : cs) = grab (d + 1) (acc ++ "(") cs
    grab d acc (')' : cs)
        | d == 1 = Just (acc ++ ")", cs)
        | otherwise = grab (d - 1) (acc ++ ")") cs
    grab d acc (c : cs) = grab d (acc ++ [c]) cs
    spanStr [] = ([], [])
    spanStr ('\\' : c : cs) = let (l, r) = spanStr cs in ('\\' : c : l, r)
    spanStr ('"' : cs) = ("\"", cs)
    spanStr (c : cs) = let (l, r) = spanStr cs in (c : l, r)

headArity :: ST.SessionBackend -> Text -> Text -> IO (Maybe Int)
headArity backend errText hd = case statedArity errText hd of
    Just n -> pure (Just n)
    Nothing -> probeArity backend hd

statedArity :: Text -> Text -> Maybe Int
statedArity errText hd = do
    rest <- afterInfix ("The function `" <> hd <> "' is applied to") errText
    only <- afterInfix "has only " rest
    numberWord (T.takeWhile (/= '\n') only)
  where
    afterInfix needle t = case T.breakOn needle t of
        (_, r) | not (T.null r) -> Just (T.drop (T.length needle) r)
        _ -> Nothing

numberWord :: Text -> Maybe Int
numberWord t = lookup (T.takeWhile (/= ' ') (T.strip t)) tbl
  where
    tbl = [("one", 1), ("two", 2), ("three", 3), ("four", 4), ("five", 5)]

probeArity :: ST.SessionBackend -> Text -> IO (Maybe Int)
probeArity backend headTok = do
    ok0 <- checks 0
    if not ok0 then pure Nothing else Just <$> climb 0
  where
    climb k
        | k >= maxProbeArity = pure k
        | otherwise = do
            ok <- checks (k + 1)
            if ok then climb (k + 1) else pure k
    checks k = do
        out <- ST.sbQueryType backend (applied k)
        pure (isClean (healthOfTypeQuery out))
    applied k = T.unwords (headTok : replicate k "undefined")

vacuous :: Text -> Bool
vacuous s = "undefined" `elem` T.split (not . identChar) s
  where
    identChar c = isAlphaNum c || c `elem` ("_'." :: String)
