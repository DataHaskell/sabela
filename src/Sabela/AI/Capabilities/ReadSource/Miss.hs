{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | The miss payloads read_source answers with: several owning packages, or
a located archive that lacks the asked-for module.
-}
module Sabela.AI.Capabilities.ReadSource.Miss (
    missJson,
    severalOwners,
) where

import Data.Aeson (Value, (.=))
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.ReadSourceArgs (ReadSourceReq (..), readSourceCallText)
import Sabela.AI.Sdist.Locate (LocateMiss (..))
import Sabela.AI.SourceLocate (nearest)
import Sabela.Api (errorJson, errorJsonWith)

severalOwners :: Text -> [Text] -> Value
severalOwners m pkgs =
    errorJsonWith
        ( "`"
            <> m
            <> "` is exposed by "
            <> T.pack (show (length pkgs))
            <> " packages; call one of: "
            <> T.intercalate ", " (map call (take 4 pkgs))
        )
        ["owners" .= pkgs]
  where
    call p = readSourceCallText [("module", m), ("package", p)]

missJson :: ReadSourceReq -> Text -> Text -> LocateMiss -> Value
missJson req pkg ver = \case
    BadArchive e ->
        errorJson (pv <> " did not read as an sdist archive: " <> e)
    NoSuchModule present ->
        errorJsonWith
            (pv <> " contains no file for `" <> rsModule req <> "`")
            [ "candidates" .= nearest (rsModule req) present
            , "modules" .= take 20 present
            ]
  where
    pv = pkg <> "-" <> ver
