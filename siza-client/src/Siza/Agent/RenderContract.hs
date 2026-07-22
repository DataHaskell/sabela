{-# LANGUAGE OverloadedStrings #-}

{- | Repair the last-mile display contract for a clean, output-less markup
write. Applicability is decided by the requested deliverable and the inferred
type of the written expression, never by a producer or library name.
-}
module Siza.Agent.RenderContract (
    displayCandidate,
    repairDisplayContract,
) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Foldable (toList)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.RepairDispatch (acceptRepair)
import Sabela.AI.Types (ToolOutcome (..))
import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.Discover (isOwningTool, toolCallSource)
import Siza.Agent.Loop.Support (replaceCall)
import Siza.Agent.Owned (ownedCellOutcome)
import Siza.Agent.Repair (Dispatch, snapshot)

{- | Produce a display-wrapped source when the goal asks for visible output and
the compiler says the expression is textual markup. Strict and lazy Text use
their matching unpacker; Html/SVG string aliases need no conversion.
-}
displayCandidate :: Text -> Text -> Text -> Maybe Text
displayCandidate goal inferred src
    | not (displayGoal goal) = Nothing
    | alreadyDisplayed src = Nothing
    | otherwise = do
        expr <- sourceExpression src
        (imp, wrap) <- wrapperFor (goal <> "\n" <> src) inferred
        let body
                | isDefinition src = T.stripEnd src <> "\n" <> wrap expr
                | otherwise = replaceExpression src (wrap expr)
        pure (addImport imp body)

repairDisplayContract ::
    Text ->
    Dispatch ->
    ToolCall ->
    Either Text ToolOutcome ->
    IO (Maybe (ToolCall, Either Text ToolOutcome))
repairDisplayContract goal disp call out
    | not (displayGoal goal) = pure Nothing
    | not (isOwningTool (tcName call)) = pure Nothing
    | Just (cid, True) <- ownedCellOutcome call out
    , noVisibleOutput out
    , let src = toolCallSource call
    , Just expr <- sourceExpression src = do
        ty <- inferType disp expr
        case displayCandidate goal ty src of
            Nothing -> pure Nothing
            Just candidate -> do
                before <- snapshot disp
                let rc = replaceCall cid candidate
                out' <- disp rc
                after <- snapshot disp
                let key = T.pack (show cid)
                    defined =
                        Set.fromList
                            (concat [names | (cell, _, names) <- before, cell == key])
                    accepted =
                        acceptRepair
                            defined
                            [(cell, health) | (cell, health, _) <- before]
                            [(cell, health) | (cell, health, _) <- after]
                            key
                if accepted && not (noVisibleOutput out')
                    then pure (Just (rc, out'))
                    else do
                        _ <- disp (replaceCall cid src)
                        pure Nothing
    | otherwise = pure Nothing

displayGoal :: Text -> Bool
displayGoal =
    anyWord
        [ "show"
        , "display"
        , "render"
        , "chart"
        , "plot"
        , "visual"
        , "markdown"
        , "summary"
        , "report"
        ]

anyWord :: [Text] -> Text -> Bool
anyWord needles hay = any (`T.isInfixOf` T.toLower hay) needles

alreadyDisplayed :: Text -> Bool
alreadyDisplayed =
    anyWord
        [ "displaysvg"
        , "displayhtml"
        , "displaymarkdown"
        , "dfdisplay.display"
        , "dataframe.display.display"
        ]

wrapperFor :: Text -> Text -> Maybe (Maybe Text, Text -> Text)
wrapperFor goal ty
    | dataFrame =
        Just
            ( Just "import qualified DataFrame.Display as DFDisplay"
            , \e -> "DFDisplay.display DFDisplay.defaultDisplayOptions (" <> e <> ")"
            )
    | lazyText =
        Just (Just "import qualified Data.Text.Lazy as TL", applyText "TL.unpack")
    | strictText =
        Just (Just "import qualified Data.Text as T", applyText "T.unpack")
    | stringType = Just (Nothing, applyString)
    | htmlLike = Just (Nothing, \e -> "displayHtml (" <> e <> ")")
    | svgLike = Just (Nothing, \e -> "displaySvg (" <> e <> ")")
    | otherwise = Nothing
  where
    norm = T.toLower (T.unwords (T.words (typeTail ty)))
    strictText = norm `elem` ["text", "data.text.text", "t.text"]
    lazyText = norm `elem` ["lazy.text", "data.text.lazy.text", "tl.text"]
    stringType = norm `elem` ["string", "[char]"]
    dataFrame = lastTypeName norm == "dataframe"
    htmlLike = "html" `T.isInfixOf` norm
    svgLike = "svg" `T.isInfixOf` norm
    display = textDisplay goal
    applyText unpack e = display <> " (" <> unpack <> " (" <> e <> "))"
    applyString e = display <> " (" <> e <> ")"
    lastTypeName = lastOrEmpty . T.splitOn "."
    lastOrEmpty [] = ""
    lastOrEmpty xs = last xs
    typeTail t = case T.breakOnEnd "::" t of
        (pre, post) | not (T.null pre) -> T.strip post
        _ -> t

textDisplay :: Text -> Text
textDisplay context
    | "<svg" `T.isInfixOf` lower = "displaySvg"
    | looksLikeHtml lower = "displayHtml"
    | anyWord ["html", "webpage", "web page"] lower = "displayHtml"
    | anyWord ["chart", "plot", "visual", "svg"] lower = "displaySvg"
    | otherwise = "displayMarkdown"
  where
    lower = T.toLower context
    looksLikeHtml = any (T.isPrefixOf "<" . trimLiteral . rhs) . T.lines
    trimLiteral = T.dropWhile (`elem` [' ', '\t', '\r', '"', '\''])
    rhs t = case T.breakOn "=" t of
        (_, rest) | not (T.null rest) -> T.drop 1 rest
        _ -> t

sourceExpression :: Text -> Maybe Text
sourceExpression src =
    case reverse [name | l <- codeLines, Just name <- [definedName l]] of
        (name : _) -> Just name
        []
            | null codeLines -> Nothing
            | otherwise -> Just (T.unwords (map T.strip codeLines))
  where
    codeLines =
        [ l
        | l <- T.lines src
        , let s = T.strip l
        , not (T.null s)
        , not ("import " `T.isPrefixOf` s)
        , not ("{-#" `T.isPrefixOf` s)
        , not ("-- cabal:" `T.isPrefixOf` s)
        ]

isDefinition :: Text -> Bool
isDefinition = any (isJust . definedName) . T.lines

definedName :: Text -> Maybe Text
definedName line =
    case T.breakOn "=" (T.strip line) of
        (lhs, rhs)
            | not (T.null rhs)
            , [name] <- T.words lhs
            , T.all validName name ->
                Just name
        _ -> Nothing
  where
    validName c = c == '_' || c == '\'' || isDigit c || isAsciiUpper c || isAsciiLower c

replaceExpression :: Text -> Text -> Text
replaceExpression src wrapped = T.unlines (headers ++ [wrapped])
  where
    headers =
        [ l
        | l <- T.lines src
        , let s = T.strip l
        , "import " `T.isPrefixOf` s
            || "{-#" `T.isPrefixOf` s
            || "-- cabal:" `T.isPrefixOf` s
        ]

addImport :: Maybe Text -> Text -> Text
addImport Nothing src = src
addImport (Just imp) src
    | imp `T.isInfixOf` src = src
    | otherwise = T.unlines (insertAfterHeader (T.lines src))
  where
    insertAfterHeader (l : ls)
        | "-- cabal:" `T.isPrefixOf` T.stripStart l || "{-#" `T.isPrefixOf` T.stripStart l =
            l : insertAfterHeader ls
    insertAfterHeader ls = imp : ls

inferType :: Dispatch -> Text -> IO Text
inferType disp expr = do
    r <- disp (ToolCall "check_type" (object ["expr" .= expr]))
    pure $ case r of
        Right (ToolOk v) -> firstText "result" v
        _ -> ""

firstText :: Text -> Value -> Text
firstText key (Object o) =
    case KM.lookup (K.fromText key) o of
        Just (String s) -> s
        _ -> firstNonEmpty [firstText key v | v <- KM.elems o]
firstText key (Array a) = firstNonEmpty [firstText key v | v <- toList a]
firstText _ _ = ""

firstNonEmpty :: [Text] -> Text
firstNonEmpty = fromMaybe "" . findNonEmpty
  where
    findNonEmpty [] = Nothing
    findNonEmpty (x : xs)
        | T.null x = findNonEmpty xs
        | otherwise = Just x

noVisibleOutput :: Either Text ToolOutcome -> Bool
noVisibleOutput (Right (ToolOk v)) = not (hasOutputs v)
noVisibleOutput _ = True

hasOutputs :: Value -> Bool
hasOutputs (Object o) =
    any hasOutputs (KM.elems o)
        || case KM.lookup "outputs" o of
            Just (Array a) -> not (null a)
            _ -> False
hasOutputs (Array a) = any hasOutputs a
hasOutputs _ = False
