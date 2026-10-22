{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | The chart language on the wire: the flat spec a chart's output carries,
which the builder sends back and the model's chart tool takes. Decoding
names the field and the allowed words when something does not fit, since
that message is what a model reads next.
-}
module Sabela.Api.Viz (
    VizSpec (..),
    VizLayer (..),
    Enc (..),
    orderChannels,
    VizSourceRequest (..),
    VizSourceResult (..),
    VizOwnership (..),
    channels,
    marks,
    aggregates,
    fieldTypes,
    scales,
    sorts,
    stacks,
    selections,
) where

import Data.Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Pair, Parser)

import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.Api.Viz.Vocab

-- | A column on a channel, with its modifiers as lowercase words.
data Enc = Enc
    { encField :: Text
    , encAggregate :: Maybe Text
    , encBin :: Maybe Int
    , encType :: Maybe Text
    , encSort :: Maybe Text
    , encScale :: Maybe Text
    , encTitle :: Maybe Text
    , encStack :: Maybe Text
    , encZero :: Maybe Bool
    -- ^ Whether the axis is anchored at zero; absent leaves it to the renderer.
    }
    deriving (Eq, Show)

-- | A mark and its channels, in channel order; tooltip holds many fields.
data VizLayer = VizLayer
    { vlMark :: Maybe Text
    , vlChannels :: [(Text, [Enc])]
    }
    deriving (Eq, Show)

data VizSpec = VizSpec
    { vsBase :: VizLayer
    , vsLayers :: [VizLayer]
    , vsTitle :: Maybe Text
    , vsWidth :: Maybe Int
    , vsHeight :: Maybe Int
    , vsSelect :: Maybe (Text, Text)
    -- ^ A selection's widget name and kind, @interval@ or @point@.
    }
    deriving (Eq, Show)

oneOf :: Text -> [Text] -> Object -> Parser (Maybe Text)
oneOf k allowed o = case KM.lookup (Key.fromText k) o of
    Nothing -> pure Nothing
    Just Null -> pure Nothing
    Just (String s)
        | s `elem` allowed -> pure (Just s)
        | otherwise ->
            fail
                ( T.unpack
                    ( k
                        <> ": expected one of "
                        <> T.intercalate ", " allowed
                        <> "; got "
                        <> T.pack (show s)
                    )
                )
    Just _ -> fail (T.unpack (k <> ": expected one of " <> T.intercalate ", " allowed))

instance FromJSON Enc where
    parseJSON = withObject "encoding" $ \o -> do
        rejectUnknown
            "encoding"
            ["field", "aggregate", "bin", "type", "sort", "scale", "title", "stack", "zero"]
            o
        Enc
            <$> o .: "field"
            <*> oneOf "aggregate" aggregates o
            <*> o .:? "bin"
            <*> oneOf "type" fieldTypes o
            <*> oneOf "sort" sorts o
            <*> oneOf "scale" scales o
            <*> o .:? "title"
            <*> oneOf "stack" stacks o
            <*> o .:? "zero"

instance ToJSON Enc where
    toJSON e =
        object
            ( catMaybes
                [ Just ("field" .= encField e)
                , ("aggregate" .=) <$> encAggregate e
                , ("bin" .=) <$> encBin e
                , ("type" .=) <$> encType e
                , ("sort" .=) <$> encSort e
                , ("scale" .=) <$> encScale e
                , ("title" .=) <$> encTitle e
                , ("stack" .=) <$> encStack e
                , ("zero" .=) <$> encZero e
                ]
            )

rejectUnknown :: String -> [Text] -> Object -> Parser ()
rejectUnknown what known o =
    case [k | k <- map Key.toText (KM.keys o), k `notElem` known] of
        [] -> pure ()
        bad ->
            fail
                ( what
                    <> ": unknown key(s) "
                    <> T.unpack (T.intercalate ", " bad)
                    <> "; the keys are "
                    <> T.unpack (T.intercalate ", " known)
                )

layerKeys :: [Text]
layerKeys = "mark" : channels

parseLayer :: Object -> Parser VizLayer
parseLayer o =
    VizLayer
        <$> oneOf "mark" marks o
        <*> fmap catMaybes (mapM channel channels)
  where
    channel ch = case KM.lookup (Key.fromText ch) o of
        Nothing -> pure Nothing
        Just Null -> pure Nothing
        Just v
            | ch == "tooltip" -> do
                encs <- parseJSON v
                pure (if null encs then Nothing else Just (ch, encs))
            | otherwise -> do
                e <- parseJSON v
                pure (Just (ch, [e]))

instance FromJSON VizLayer where
    parseJSON = withObject "layer" $ \o -> do
        rejectUnknown "layer" layerKeys o
        parseLayer o

instance FromJSON VizSpec where
    parseJSON = withObject "chart" $ \o -> do
        rejectUnknown
            "chart"
            (["v", "layers", "title", "width", "height", "select"] ++ layerKeys)
            o
        base <- parseLayer o
        VizSpec base
            <$> o .:? "layers" .!= []
            <*> o .:? "title"
            <*> o .:? "width"
            <*> o .:? "height"
            <*> (o .:? "select" >>= traverse selection)
      where
        selection = withObject "select" $ \s -> do
            name <- s .: "name"
            kind <- oneOf "type" selections s
            pure (name, fromMaybe "interval" kind)

layerPairs :: VizLayer -> [Pair]
layerPairs l =
    catMaybes [("mark" .=) <$> vlMark l]
        ++ [ Key.fromText ch
                .= (if ch == "tooltip" then toJSON encs else toJSON (head' encs))
           | (ch, encs) <- vlChannels l
           , not (null encs)
           ]
  where
    head' (e : _) = toJSON e
    head' [] = Null

instance ToJSON VizLayer where
    toJSON = object . layerPairs

instance ToJSON VizSpec where
    toJSON s =
        object
            ( ("v" .= (1 :: Int))
                : layerPairs (vsBase s)
                ++ catMaybes
                    [ if null (vsLayers s) then Nothing else Just ("layers" .= vsLayers s)
                    , ("title" .=) <$> vsTitle s
                    , ("width" .=) <$> vsWidth s
                    , ("height" .=) <$> vsHeight s
                    , (\(n, k) -> "select" .= object ["name" .= n, "type" .= k]) <$> vsSelect s
                    ]
            )

{- | What the builder or the chart tool sends: the spec, the data expression
(a binding name, usually), and the cell being rewritten when there is one.
-}
data VizSourceRequest = VizSourceRequest
    { vsrSpec :: VizSpec
    , vsrData :: Maybe Text
    , vsrCellId :: Maybe Int
    }
    deriving (Eq, Show)

instance FromJSON VizSourceRequest where
    parseJSON = withObject "VizSourceRequest" $ \o ->
        VizSourceRequest <$> o .: "vsrSpec" <*> o .:? "vsrData" <*> o .:? "vsrCellId"

instance ToJSON VizSourceRequest where
    toJSON r =
        object
            ( catMaybes
                [ Just ("vsrSpec" .= vsrSpec r)
                , ("vsrData" .=) <$> vsrData r
                , ("vsrCellId" .=) <$> vsrCellId r
                ]
            )

data VizOwnership = OwnCanonical | OwnEdited | OwnNew
    deriving (Eq, Show)

instance ToJSON VizOwnership where
    toJSON o = String $ case o of
        OwnCanonical -> "canonical"
        OwnEdited -> "edited"
        OwnNew -> "new"

instance FromJSON VizOwnership where
    parseJSON = withText "VizOwnership" $ \case
        "canonical" -> pure OwnCanonical
        "edited" -> pure OwnEdited
        "new" -> pure OwnNew
        _ -> fail "ownership: expected canonical, edited or new"

data VizSourceResult = VizSourceResult
    { vsSource :: Text
    , vsOwnership :: VizOwnership
    , vsMessage :: Text
    , vsDataExpr :: Text
    , vsCellHash :: Maybe Text
    }
    deriving (Eq, Show)

instance ToJSON VizSourceResult where
    toJSON r =
        object
            [ "vsSource" .= vsSource r
            , "vsOwnership" .= vsOwnership r
            , "vsMessage" .= vsMessage r
            , "vsDataExpr" .= vsDataExpr r
            , "vsCellHash" .= vsCellHash r
            ]

instance FromJSON VizSourceResult where
    parseJSON = withObject "VizSourceResult" $ \o ->
        VizSourceResult
            <$> o .: "vsSource"
            <*> o .: "vsOwnership"
            <*> o .: "vsMessage"
            <*> o .: "vsDataExpr"
            <*> o .:? "vsCellHash"
