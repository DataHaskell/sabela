{-# LANGUAGE OverloadedStrings #-}

{- | C1-14c: the delimited summariser must report a verdict backed by
evidence and must never emit a column name it did not read.
-}
module Test.PeekVerdictSpec (spec) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Test.Hspec
import Test.QuickCheck

import Sabela.AI.PeekData (peekData, peekResultJSON)
import Test.ArtefactGen (
    GridSpec (..),
    genArtefactText,
    genBlobText,
    genGridSpec,
    genJsonText,
    genProse,
    renderGrid,
 )

peekOf :: Text -> Value
peekOf = peekResultJSON . peekData 5

field :: Text -> Value -> Maybe Value
field k (Object o) = KM.lookup (K.fromText k) o
field _ _ = Nothing

arrayOf :: Maybe Value -> [Value]
arrayOf (Just (Array a)) = foldr (:) [] a
arrayOf _ = []

textOf :: Maybe Value -> Maybe Text
textOf (Just (String s)) = Just s
textOf _ = Nothing

columnNames :: Value -> [Text]
columnNames v = mapMaybe (textOf . field "name") (arrayOf (field "columns" v))

verdictOf :: Value -> Text
verdictOf v = fromMaybe "<none>" (textOf (field "verdict" v))

rowsOf :: Value -> [[Text]]
rowsOf v = [mapMaybe (textOf . Just) (arrayOf (Just r)) | r <- arrayOf (field "rows" v)]

spec :: Spec
spec = describe "peek verdict (C1-14c: no fabricated schema)" $ do
    it "never reports a column name that is not in the artefact" $
        property $
            forAll genArtefactText $ \txt ->
                let names = columnNames (peekOf txt)
                 in counterexample (show (T.take 200 txt, names)) $
                        all (`T.isInfixOf` txt) names

    it "reports a verdict for every input" $
        property $
            forAll genArtefactText $ \txt ->
                verdictOf (peekOf txt) `elem` ["delimited", "not-delimited"]

    it "answers prose, JSON and byte blobs with not-delimited, or round-trips" $
        property $
            forAll (oneof [genProse, genJsonText, genBlobText]) $ \txt ->
                let v = peekOf txt
                    ls = filter (not . T.null) (T.lines txt)
                    delim = fromMaybe "" (textOf (field "delimiter" v))
                 in counterexample (show (T.take 200 txt, v)) $
                        verdictOf v == "not-delimited"
                            || all (\r -> T.intercalate delim r `elem` ls) (rowsOf v)

    it "takes every column name it reports from the artefact's first line" $
        property $
            forAll genGridSpec $ \gs ->
                let txt = renderGrid gs
                    names = columnNames (peekOf txt)
                    firstLine = T.splitOn (gsDelim gs) (T.takeWhile (/= '\n') txt)
                 in counterexample (show (txt, names)) $
                        all (`elem` firstLine) names

    it "types a column only on evidence from its sampled cells" $
        property $
            forAll genArtefactText $ \txt ->
                let v = peekOf txt
                    cols = arrayOf (field "columns" v)
                    rows = rowsOf v
                    typed = [(i, t) | (i, c) <- zip [0 ..] cols, Just t <- [textOf (field "type" c)]]
                 in counterexample (show (T.take 200 txt, v)) $
                        all (evidenced rows) typed

evidenced :: [[Text]] -> (Int, Text) -> Bool
evidenced rows (i, ty) = all ok cells
  where
    cells = filter (not . T.null) (map T.strip (concatMap (take 1 . drop i) rows))
    ok c = case ty of
        "Int" -> isInt c
        "Double" -> isDouble c
        "Bool" -> T.toLower c `elem` ["true", "false"]
        _ -> True

isInt :: Text -> Bool
isInt t = case TR.signed TR.decimal t :: Either String (Integer, Text) of
    Right (_, rest) -> T.null rest
    _ -> False

isDouble :: Text -> Bool
isDouble t = case TR.signed TR.double t :: Either String (Double, Text) of
    Right (_, rest) -> T.null rest
    _ -> False
