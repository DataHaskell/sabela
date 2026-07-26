{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Capabilities.ApiRef (
    execApiReference,
    ApiRefSources (..),
    ArgKind (..),
    argKind,
    mergeApiRef,
) where

import Data.Aeson (Value, object, (.=))
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.Util (fieldText)
import Sabela.AI.HoogleResolve (HoogleHit (..), hoogleQuery)
import Sabela.AI.Types (ToolOutcome, okOutcome)
import Sabela.SessionTypes (SessionBackend (..))
import Sabela.State (App (..))
import Sabela.State.SessionManager (getHaskellSession)

apiRefHoogleK :: Int
apiRefHoogleK = 40

data ArgKind = AsType | AsModuleOrName
    deriving (Eq, Show)

argKind :: Text -> ArgKind
argKind t0
    | "->" `T.isInfixOf` t || "::" `T.isInfixOf` t || "_" `T.isPrefixOf` t = AsType
    | otherwise = AsModuleOrName
  where
    t = T.strip t0

data ApiRefSources = ApiRefSources
    { arsArg :: Text
    , arsHoogle :: [HoogleHit]
    , arsBrowse :: Maybe Text
    , arsHoles :: Maybe Text
    }

execApiReference :: App -> Value -> IO ToolOutcome
execApiReference app input = do
    let arg = T.strip (apiRefArg input)
    if T.null arg
        then pure (okOutcome (mergeApiRef (ApiRefSources "" [] Nothing Nothing)))
        else do
            hoogle <- scopedHoogle arg
            mBackend <- getHaskellSession (appSessions app)
            (browse, holes) <-
                maybe (pure (Nothing, Nothing)) (gatherLive arg) mBackend
            pure (okOutcome (mergeApiRef (ApiRefSources arg hoogle browse holes)))

apiRefArg :: Value -> Text
apiRefArg input =
    let m = fieldText "module" input
     in if T.null m then fieldText "query" input else m

scopedHoogle :: Text -> IO [HoogleHit]
scopedHoogle arg = do
    hits <- hoogleQuery apiRefHoogleK arg
    pure $ case argKind arg of
        AsType -> hits
        AsModuleOrName -> filter scoped hits
  where
    needle = T.toLower arg
    scoped h =
        needle `T.isInfixOf` T.toLower (hhModule h)
            || needle `T.isInfixOf` T.toLower (hhName h)

gatherLive :: Text -> SessionBackend -> IO (Maybe Text, Maybe Text)
gatherLive arg backend = case argKind arg of
    AsType -> do
        fits <- sbQueryHoleFits backend (asHole arg)
        pure (Nothing, Just fits)
    AsModuleOrName -> do
        raw <-
            if T.any (== ' ') arg
                then pure Nothing
                else Just <$> sbQueryBrowse backend arg
        pure (raw, Nothing)
  where
    asHole g = if "_" `T.isPrefixOf` g then g else "_ :: " <> g

mergeApiRef :: ApiRefSources -> Value
mergeApiRef s
    | not (hasDynamic s) =
        object
            [ "module" .= arsArg s
            , "source" .= ("none" :: Text)
            , "note"
                .= ( "No signatures found via hoogle, :browse, or hole fits. Try a"
                        <> " different module or name, a type query, or run a cell so"
                        <> " :browse can see an in-scope binding." ::
                        Text
                   )
            ]
    | otherwise =
        object $
            ["module" .= arsArg s, "source" .= ("live" :: Text)]
                <> ["hoogle" .= map hoogleJSON hs | let hs = arsHoogle s, not (null hs)]
                <> ["exports" .= b | Just b <- [usableText (arsBrowse s)]]
                <> ["holeFits" .= h | Just h <- [usableText (arsHoles s)]]

hasDynamic :: ApiRefSources -> Bool
hasDynamic s =
    not (null (arsHoogle s))
        || present (arsBrowse s)
        || present (arsHoles s)
  where
    present = isJust . usableText

hoogleJSON :: HoogleHit -> Value
hoogleJSON h =
    object
        [ "name" .= hhName h
        , "module" .= hhModule h
        , "package" .= hhPackage h
        , "type" .= hhType h
        ]

usableText :: Maybe Text -> Maybe Text
usableText Nothing = Nothing
usableText (Just t)
    | T.null s || any (`T.isInfixOf` s) unusable = Nothing
    | otherwise = Just (T.strip t)
  where
    s = T.toLower (T.strip t)
    unusable =
        [ "not in scope"
        , "error:"
        , "could not load module"
        , "hidden package"
        , "\"severity\":\"error\""
        ]
