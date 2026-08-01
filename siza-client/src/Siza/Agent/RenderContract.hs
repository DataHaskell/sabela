{-# LANGUAGE OverloadedStrings #-}

{- | A committed cell that produced nothing visible is offered a rendering.
Every candidate is proposed for every value and the kernel settles it, so no
branch here reads a type name or the caller's prose.
-}
module Siza.Agent.RenderContract (
    displayCandidates,
    displayProbe,
    repairDisplayContract,
) where

import Control.Monad (void, when)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Maybe (isJust, mapMaybe)
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

{- | Every rendering worth offering for this source, richest unpacking last,
in the order they will be tried. The goal text chooses the medium and nothing
else: whether a rendering is offered at all is a question about the source.
-}
displayCandidates :: Text -> Text -> [Text]
displayCandidates goal src
    | alreadyRendered src = []
    | otherwise = case sourceExpression src of
        Nothing -> []
        Just expr -> map (candidate expr) (renderers (goal <> "\n" <> src))
  where
    candidate expr r = addImport (renderImport r) (body (renderWrap r expr))
    body wrapped
        | isDefinition src = T.stripEnd src <> "\n" <> wrapped
        | otherwise = replaceExpression src wrapped

{- | One way to make a value visible: the import it needs and how it wraps the
expression.
-}
data Renderer = Renderer
    { renderImport :: Maybe Text
    , renderWrap :: Text -> Text
    }

{- | The renderings proposed for every value, cheapest first. A value the
table has never seen is offered the same list as any other; the kernel is
what rejects the ones that do not fit it.
-}
renderers :: Text -> [Renderer]
renderers context =
    [ Renderer Nothing (\e -> display <> " (" <> e <> ")")
    , Renderer
        (Just "import qualified Data.Text as T")
        (\e -> display <> " (T.unpack (" <> e <> "))")
    , Renderer
        (Just "import qualified Data.Text.Lazy as TL")
        (\e -> display <> " (TL.unpack (" <> e <> "))")
    ]
        ++ bridges
  where
    display = textDisplay context

{- | Renderings that take the value whole rather than a String. They are
proposed like any other candidate, so adding one teaches the double-wrap
guard about it too.
-}
bridges :: [Renderer]
bridges =
    [ Renderer
        (Just "import qualified DataFrame.Display as DFDisplay")
        (\e -> "DFDisplay.display DFDisplay.defaultDisplayOptions (" <> e <> ")")
    ]

repairDisplayContract ::
    Text ->
    Dispatch ->
    ToolCall ->
    Either Text ToolOutcome ->
    IO (Maybe (ToolCall, Either Text ToolOutcome))
repairDisplayContract goal disp call out
    | not (isOwningTool (tcName call)) = pure Nothing
    | Just (cid, True) <- ownedCellOutcome call out
    , noVisibleOutput out
    , let src = toolCallSource call
    , candidates@(_ : _) <- displayCandidates goal src =
        offer disp cid src candidates
    | otherwise = pure Nothing

{- | Asks the kernel about each rendering in turn and commits the first that
both type-checks and then shows something. A rendering the kernel rejects
costs nothing; a commit that shows nothing is put back.
-}
offer ::
    Dispatch ->
    Int ->
    Text ->
    [Text] ->
    IO (Maybe (ToolCall, Either Text ToolOutcome))
offer disp cid src = go False
  where
    go committed [] = do
        when committed (void (disp (replaceCall cid src)))
        pure Nothing
    go committed (c : cs) = do
        fits <- typeChecks disp (displayProbe c)
        if not fits
            then go committed cs
            else do
                before <- snapshot disp
                let rc = replaceCall cid c
                out' <- disp rc
                after <- snapshot disp
                if kept before after && not (noVisibleOutput out')
                    then pure (Just (rc, out'))
                    else go True cs
    key = T.pack (show cid)
    kept before after =
        acceptRepair
            (Set.fromList (concat [ns | (cell, _, ns) <- before, cell == key]))
            [(cell, health) | (cell, health, _) <- before]
            [(cell, health) | (cell, health, _) <- after]
            key

{- | The candidate reduced to what the kernel needs to judge it: its imports
and the rendering call. The definition above them is already committed, so
the notebook's own bindings supply it.
-}
displayProbe :: Text -> Text
displayProbe candidate = T.unlines (headers ++ take 1 (reverse body))
  where
    ls = [l | l <- T.lines candidate, not (T.null (T.strip l))]
    (headers, body) = span (isHeaderLine . T.strip) ls

-- | Whether the kernel accepts the rendering without it being committed.
typeChecks :: Dispatch -> Text -> IO Bool
typeChecks disp code = do
    r <- disp (ToolCall "try" (object ["code" .= code]))
    pure $ case r of
        Right (ToolOk _) -> True
        _ -> False

anyWord :: [Text] -> Text -> Bool
anyWord needles hay = any (`T.isInfixOf` T.toLower hay) needles

{- | A source that already calls a renderer is left alone. The markers are
read off the renderers themselves, so a renderer cannot be added without the
guard learning it.
-}
alreadyRendered :: Text -> Bool
alreadyRendered = anyWord markers
  where
    markers =
        displayFunctions
            ++ [T.toLower (T.takeWhile (/= '(') (renderWrap b "")) | b <- bridges]

-- | The notebook's own ways of showing a String.
displayFunctions :: [Text]
displayFunctions = ["displaymarkdown", "displayhtml", "displaysvg"]

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
    case reverse (mapMaybe definedName codeLines) of
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
        , not (isHeaderLine s)
        ]

-- | A line that sets a cell up rather than computes anything in it.
isHeaderLine :: Text -> Bool
isHeaderLine s = any (`T.isPrefixOf` s) ["import ", "{-#", "-- cabal:"]

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
    headers = [l | l <- T.lines src, isHeaderLine (T.strip l)]

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
