{-# LANGUAGE OverloadedStrings #-}

{- | C1-15a/C1-15c: an over-bound value is answered with measured facts about
the rendering, and the choice of facts is a function of the rendering alone —
never of a type name and never of the caller's prose.
-}
module Test.ValueSynopsisSpec (spec) where

import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Sabela.AI.PeekData (
    DelimitedView (..),
    PeekVerdict (..),
    peekData,
    peekVerdict,
 )
import Sabela.AI.ValueEcho (echoCharBound, echoListing, synopsisSlack)
import Test.ArtefactGen (
    GridSpec (..),
    genGridSpec,
    genIdent,
    genNamedGrid,
    genNumericGrid,
    renderGrid,
 )

{- | A rendered value together with the counts a truthful synopsis must be
able to state about it.
-}
data Rendering = Rendering
    { rText :: Text
    , rLines :: Int
    , rElements :: Maybe Int
    , rColumns :: Maybe Int
    }
    deriving (Show)

genRendering :: Gen Rendering
genRendering = oneof [anyGridRendering, mapRendering, listRendering, textRendering]

anyGridRendering :: Gen Rendering
anyGridRendering = renderingOf <$> genGridSpec

{- | A grid wide and tall enough to BE a grid: two columns is the least a
consistent-field-count reading can be evidence for.
-}
gridRendering :: Gen Rendering
gridRendering = renderingOf <$> genGridSpec `suchThat` bigEnough
  where
    bigEnough gs = specWidth gs >= 2 && length (gsRows gs) >= 3

renderingOf :: GridSpec -> Rendering
renderingOf gs = Rendering txt (length ls) Nothing (Just (specWidth gs))
  where
    txt = renderGrid gs
    ls = filter (not . T.null) (T.lines txt)

specWidth :: GridSpec -> Int
specWidth gs = length (fromMaybe (concat (take 1 (gsRows gs))) (gsHeader gs))

mapRendering :: Gen Rendering
mapRendering = do
    n <- choose (4, 40 :: Int)
    ks <- nub <$> vectorOf n genIdent
    let body =
            T.intercalate
                ","
                [ "(" <> quoted k <> "," <> T.pack (show i) <> ")"
                | (i, k) <- zip [1 :: Int ..] ks
                ]
        txt = "fromList [" <> body <> "]"
    pure (Rendering txt 1 (Just (length ks)) Nothing)

listRendering :: Gen Rendering
listRendering = do
    n <- choose (4, 200 :: Int)
    xs <- vectorOf n (arbitrary :: Gen Int)
    let txt = T.pack (show xs)
    pure (Rendering txt 1 (Just (length xs)) Nothing)

textRendering :: Gen Rendering
textRendering = do
    n <- choose (4, 60 :: Int)
    ws <- vectorOf n genIdent
    let txt = T.unwords ws
    pure (Rendering txt 1 Nothing Nothing)

quoted :: Text -> Text
quoted t = "\"" <> t <> "\""

echoOf :: Text -> Text -> Text
echoOf ty v = echoListing (const (Just v)) ("b :: " <> ty <> " = _")

overBound :: Rendering -> Bool
overBound r = T.length (rText r) > echoCharBound

states :: Int -> Text -> Text -> Bool
states n word out = (T.pack (show n) <> " " <> word) `T.isInfixOf` out

typeNames :: [Text]
typeNames =
    [ "Data.Map.Map Text Int"
    , "Data.Text.Text"
    , "[Double]"
    , "Data.Aeson.Value"
    , "DataFrame"
    , "Quux.Widget"
    ]

spec :: Spec
spec = describe "value synopsis (C1-15a measured, C1-15c type-directed)" $ do
    it "states the rendering's real line count when it elides" $
        property $
            forAll genRendering $ \r ->
                overBound r ==>
                    let out = echoOf "Data.Map.Map Text Int" (rText r)
                     in counterexample (show (rLines r, out)) $
                            states (rLines r) "lines" out || rLines r == 1

    it "states the real element count of a bracketed collection" $
        property $
            forAll genRendering $ \r ->
                overBound r ==> case rElements r of
                    Nothing -> property True
                    Just n ->
                        let out = echoOf "Data.Map.Map Text Int" (rText r)
                         in counterexample (show (n, out)) (states n "elements" out)

    it "states the real column count of a grid" $
        property $
            forAll gridRendering $ \r ->
                overBound r ==> case rColumns r of
                    Nothing -> property True
                    Just n ->
                        let out = echoOf "DataFrame" (rText r)
                         in counterexample (show (n, out)) (states n "columns" out)

    it "invents nothing: every quoted fragment of the synopsis is in the rendering" $
        property $
            forAll genRendering $ \r ->
                let out = T.strip (echoOf "Data.Aeson.Value" (rText r))
                    frags = filter (not . T.null) (quotedFragments out)
                 in counterexample (show (out, frags)) $
                        all (`T.isInfixOf` rText r) frags

    it "stays within a fixed slack of the echo bound" $
        property $
            forAll genRendering $ \r ->
                let out = T.strip (echoOf "Data.Map.Map Text Int" (rText r))
                 in counterexample (show (T.length out, out)) $
                        T.length out <= echoCharBound + synopsisSlack

    it "answers the same for every type name, including one never seen before" $
        property $
            forAll genRendering $ \r ->
                let outs = [dropName (echoOf ty (rText r)) | ty <- typeNames]
                 in counterexample (show outs) (length (nub outs) == 1)

    it "attaches no synopsis to a function-typed binding" $
        property $
            forAll genRendering $ \r ->
                echoListing (const (Just (rText r))) "f :: Int -> Int = _"
                    === "f :: Int -> Int\n"

    it "claims no header over a rendering whose first row is data" $
        property $
            forAll (overBoundGrid genNumericGrid) $ \txt ->
                let out = echoOf "Data.Map.Map Text Int" txt
                 in counterexample (show (T.take 120 txt, out)) $
                        not ("header: " `T.isInfixOf` out)

    it "claims the header of a rendering that has one, from its first row" $
        property $
            forAll (overBoundGrid genNamedGrid) $ \txt ->
                let out = echoOf "Data.Map.Map Text Int" txt
                    firstRow = T.takeWhile (/= '\n') txt
                 in counterexample (show (T.take 120 txt, out)) $
                        "header: " `T.isInfixOf` out
                            && all (`T.isInfixOf` firstRow) (quotedFragments out)

    it "agrees with the delimited reader about whether there is a header" $
        property $
            forAll genRendering $ \r ->
                let out = echoOf "Quux.Widget" (rText r)
                 in counterexample (show (rText r, out)) $
                        not ("header: " `T.isInfixOf` out) || peekSaysHeader (rText r)

{- | Grids rendered long enough to be summarised rather than echoed, so the
header properties speak about the synopsis and not about a verbatim value.
-}
overBoundGrid :: Gen GridSpec -> Gen Text
overBoundGrid g = renderGrid <$> g `suchThat` ((> echoCharBound) . T.length . renderGrid)

-- | What the artefact reader makes of the same rendering.
peekSaysHeader :: Text -> Bool
peekSaysHeader t = case peekVerdict (peekData 5 t) of
    Delimited v -> dvHasHeader v
    NotDelimited _ -> False

{- | The synopsis without the binding's own name and type, so two renderings
can be compared for the part that describes the VALUE.
-}
dropName :: Text -> Text
dropName = snd . T.breakOn "= "

{- | The header cells the synopsis claims, which must every one have been
read from the rendering.
-}
quotedFragments :: Text -> [Text]
quotedFragments out
    | not ("header: " `T.isInfixOf` out) = []
    | otherwise = [T.strip f | f <- T.splitOn " | " cells, not (T.null (T.strip f))]
  where
    headerPart = T.takeWhile (/= ';') (snd (T.breakOnEnd "header: " out))
    cells = fst (T.breakOn " …" headerPart)
