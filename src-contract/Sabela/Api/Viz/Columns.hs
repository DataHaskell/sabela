{-# LANGUAGE OverloadedStrings #-}

-- | The columns of a frame binding, as the kernel reports them to a builder.
module Sabela.Api.Viz.Columns (VizColumns (..)) where

import Data.Aeson
import Data.Text (Text)

data VizColumns = VizColumns
    { vcBinding :: Text
    , vcCols :: [Text]
    , vcTypes :: [Text]
    , vcError :: Maybe Text
    }
    deriving (Eq, Show)

instance ToJSON VizColumns where
    toJSON c =
        object
            [ "vcBinding" .= vcBinding c
            , "vcCols" .= vcCols c
            , "vcTypes" .= vcTypes c
            , "vcError" .= vcError c
            ]

instance FromJSON VizColumns where
    parseJSON = withObject "VizColumns" $ \o ->
        VizColumns
            <$> o .: "vcBinding"
            <*> o .: "vcCols"
            <*> o .: "vcTypes"
            <*> o .:? "vcError"
