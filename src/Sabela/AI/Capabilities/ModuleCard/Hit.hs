{-# LANGUAGE OverloadedStrings #-}

{- | Rendering a single capability hit, and the doc synopsis attached to the
leading few.
-}
module Sabela.AI.Capabilities.ModuleCard.Hit (
    hitJSON,
    importLineFor,
    matchesOutcomeWithDocs,
    docSynopsis,
) where

import Data.Aeson (Value, object, (.=))
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capability (Capability (..), Hit (..), Match (..))
import Sabela.AI.Types (ToolOutcome, okOutcome)
import Sabela.SessionTypes (SessionBackend (..))

hitJSON :: Text -> Hit -> Value
hitJSON doc h =
    object
        ( [ "module" .= capModule (hitCap h)
          , "name" .= capName (hitCap h)
          , "type" .= capType (hitCap h)
          , "via" .= matchName (hitVia h)
          ]
            <> ["doc" .= doc | not (T.null doc)]
            <> ["import" .= imp | Just imp <- [importLineFor (hitCap h)]]
            <> [ "field" .= recordUpdateSyntax (hitCap h) ty
               | Just ty <- [capField (hitCap h)]
               ]
        )

recordUpdateSyntax :: Capability -> Text -> Text
recordUpdateSyntax c ty = ty <> " { " <> capName c <> " = ... }"

importLineFor :: Capability -> Maybe Text
importLineFor c
    | T.null m || T.null n = Nothing
    | T.all (`elem` operatorChars) n = Just (imp ("(" <> n <> ")"))
    | otherwise = Just (imp n)
  where
    m = capModule c
    n = capName c
    imp entity = "import " <> m <> " (" <> entity <> ")"

operatorChars :: String
operatorChars = "!#$%&*+./<=>?@\\^|-~:"

matchesOutcomeWithDocs :: SessionBackend -> Text -> [Hit] -> IO ToolOutcome
matchesOutcomeWithDocs backend q hits = do
    let (lead, rest) = splitAt docAttachCap hits
    withDocs <- mapM attach lead
    pure
        ( okOutcome
            ( object
                ["query" .= q, "matches" .= (withDocs <> map (hitJSON "") rest)]
            )
        )
  where
    attach h = do
        raw <- sbQueryDoc backend (capName (hitCap h))
        pure (hitJSON (docSynopsis raw) h)

docAttachCap :: Int
docAttachCap = 3

docSynopsisChars :: Int
docSynopsisChars = 240

docSynopsis :: Text -> Text
docSynopsis raw = case prose of
    [] -> ""
    ls -> T.take docSynopsisChars (T.unwords ls)
  where
    prose =
        take
            3
            [ l
            | l <- map (T.strip . strip) (T.lines raw)
            , not (T.null l)
            , not ("Identifier defined in" `T.isInfixOf` l)
            , not ("::" `T.isInfixOf` l)
            , not (T.isPrefixOf "<" l)
            ]
    strip =
        T.replace "{-|" ""
            . T.replace "-}" ""
            . T.replace "-- |" ""
            . T.replace "-- " ""

matchName :: Match -> Text
matchName ByName = "name"
matchName ByType = "type"
matchName BySynonym = "synonym"
matchName ByModule = "module"
