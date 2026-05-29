{-# LANGUAGE OverloadedStrings #-}

{- | Turn raw notebook-cell source into a compilable program shape.

A notebook cell is a sequence of GHCi statements sharing one scope, so a value
binding and the @x \<- readCsv@ it depends on can sit in any cell. A compiled
module can't mirror that with top-level declarations: a top-level binding can't
see a @main@-local @\<-@ bind. So we put everything sequential into one
@do@-block ("everything in @main@"): value bindings become @let@s, @\<-@ binds
stay, expressions stay — all in document order, so each statement sees the ones
before it. Only things that genuinely can't live in a @do@-block — imports,
pragmas, @data@\/@type@\/@class@\/@instance@, and Template Haskell splices — are
hoisted to the top level.

Crucially this works from the *raw* cell text (not the @let@-stripped
'ScriptHs.Parser.Line's), so multi-binding @let@s, @case@ layout, and
multi-clause functions keep their indentation.
-}
module Sabela.Export.Block (
    Hoisted (..),
    splitProgram,
    programActionExprs,
) where

import Data.List (intercalate)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

import ScriptHs.Render (TrailKind (..), TrailingResolver)

-- | Material that must live at the top level (can't go in a @do@-block).
data Hoisted = Hoisted
    { hPragmas :: [Text]
    , hImports :: [Text]
    , hTopDecls :: [Text]
    }
    deriving (Show, Eq)

emptyHoisted :: Hoisted
emptyHoisted = Hoisted [] [] []

data UnitKind
    = UImport
    | UPragma
    | UDirective
    | UDrop
    | UComment
    | UDataDecl
    | UTHSplice
    | UIOBind
    | ULet
    | USig
    | UBinding
    | UAction
    deriving (Eq, Show)

{- | Split raw cell sources into hoisted top-level material and an ordered list
of @do@-block statements. @\<-@ binds whose binder is in @skip@ are dropped
(used by the reactive exporter to turn widget binds into parameters). Trailing
single-line expressions are shaped by the resolver (print pure values, run IO).
-}
splitProgram :: TrailingResolver -> Set Text -> [Text] -> (Hoisted, [Text])
splitProgram resolve skip cells = go emptyHoisted [] (groupUnits (rawLines cells))
  where
    go h ss [] = (h, ss)
    -- @u@ is non-empty by 'groupUnits' (each group starts with @l : cont@),
    -- so the @hd@ pattern is total.
    go h ss (u@(hd : _) : us) = case classifyUnit u of
        UImport -> go h{hImports = hImports h ++ [T.stripStart hd]} ss us
        UPragma -> go h{hPragmas = hPragmas h ++ [unlinesU u]} ss us
        UDirective -> go h{hPragmas = hPragmas h ++ map langPragma (setExts hd)} ss us
        UDrop -> go h ss us
        UComment -> go h (ss ++ map T.stripStart u) us
        UDataDecl -> go h{hTopDecls = hTopDecls h ++ [unlinesU u]} ss us
        UTHSplice -> go h{hTopDecls = hTopDecls h ++ [unlinesU u]} ss us
        UIOBind
            | binderOf u `S.member` skip -> go h ss us
            | otherwise -> go h (ss ++ [unlinesU u]) us
        ULet -> go h (ss ++ [unlinesU u]) us
        UBinding -> go h (ss ++ [letify u]) us
        UAction -> go h (ss ++ [actionStmt resolve u]) us
        -- A type signature attaches to the binding that follows it: emit them
        -- as one @let@ (a do-block can't have a bare signature). A lone
        -- signature with no following binding is dropped.
        USig -> case us of
            (next : us')
                | classifyUnit next == UBinding -> go h (ss ++ [letify (u ++ next)]) us'
            _ -> go h ss us
    -- Unreachable given 'groupUnits' invariant; pinned for exhaustiveness.
    go h ss ([] : us) = go h ss us

{- | Every single-line trailing-action expression a 'splitProgram' would
resolve, so a caller can pre-resolve them against a live session and build a
pure 'TrailingResolver'.
-}
programActionExprs :: [Text] -> [Text]
programActionExprs cells =
    [ unlinesU u
    | u <- groupUnits (rawLines cells)
    , classifyUnit u == UAction
    , length u == 1
    ]

-- ---------------------------------------------------------------------------
-- Grouping raw lines into lead + continuation units
-- ---------------------------------------------------------------------------

rawLines :: [Text] -> [Text]
rawLines = intercalate [""] . map T.lines

groupUnits :: [Text] -> [[Text]]
groupUnits ls = case dropWhile isBlank ls of
    [] -> []
    (l : rest) -> let (cont, rest') = takeCont rest in (l : cont) : groupUnits rest'

takeCont :: [Text] -> ([Text], [Text])
takeCont [] = ([], [])
takeCont xs@(l : rest)
    | isIndentedNonBlank l = let (m, r) = takeCont rest in (l : m, r)
    | isBlank l =
        let (blanks, after) = span isBlank xs
         in case after of
                (x : _) | isIndentedNonBlank x -> let (m, r) = takeCont after in (blanks ++ m, r)
                _ -> ([], xs)
    | otherwise = ([], xs)

isBlank :: Text -> Bool
isBlank = T.null . T.strip

isIndentedNonBlank :: Text -> Bool
isIndentedNonBlank t = not (isBlank t) && (T.isPrefixOf " " t || T.isPrefixOf "\t" t)

-- ---------------------------------------------------------------------------
-- Classification
-- ---------------------------------------------------------------------------

classifyUnit :: [Text] -> UnitKind
classifyUnit [] = UAction
classifyUnit unit@(raw : _)
    | "import " `T.isPrefixOf` lead || lead == "import" = UImport
    | "{-#" `T.isPrefixOf` lead = UPragma
    | isCabalMeta lead = UDrop
    | "--" `T.isPrefixOf` lead = UComment
    | ":" `T.isPrefixOf` lead = UDirective
    | startsDeclKeyword lead = UDataDecl
    | "$(" `T.isPrefixOf` lead = UTHSplice
    | hasTopArrow lead = UIOBind
    | "let " `T.isPrefixOf` lead || lead == "let" = ULet
    | hasTopDoubleColon lead && not (any hasTopEquals unit) = USig
    | any hasTopEquals unit = UBinding
    | otherwise = UAction
  where
    lead = T.stripStart raw

isCabalMeta :: Text -> Bool
isCabalMeta t = "-- cabal:" `T.isPrefixOf` t || "--cabal:" `T.isPrefixOf` t

startsDeclKeyword :: Text -> Bool
startsDeclKeyword t =
    any
        (`T.isPrefixOf` t)
        ["data ", "newtype ", "type ", "class ", "instance ", "deriving ", "default "]

-- ---------------------------------------------------------------------------
-- Emitting do-block statements
-- ---------------------------------------------------------------------------

{- | A value/function binding becomes a @let@; continuation lines are indented
by the width of @"let "@ so they stay aligned past the binding name.
-}
letify :: [Text] -> Text
letify [] = ""
letify (l : rest) = T.intercalate "\n" (("let " <> l) : map ("    " <>) rest)

actionStmt :: TrailingResolver -> [Text] -> Text
actionStmt resolve unit =
    let expr = unlinesU unit
     in if length unit > 1
            then expr -- multi-line: assume an IO action, emit verbatim
            else case resolve expr of
                TrailPure -> "print (" <> expr <> ")"
                TrailIOShow -> "print =<< (" <> expr <> ")"
                _ -> expr

-- ---------------------------------------------------------------------------
-- GHCi directives
-- ---------------------------------------------------------------------------

setExts :: Text -> [Text]
setExts t = case T.words (T.stripStart t) of
    (cmd : rest)
        | cmd `elem` [":set", ":seti"] ->
            [ext | w <- rest, Just ext <- [T.stripPrefix "-X" w]]
    _ -> []

langPragma :: Text -> Text
langPragma ext = "{-# LANGUAGE " <> ext <> " #-}"

-- ---------------------------------------------------------------------------
-- Depth/string-aware token scanners
-- ---------------------------------------------------------------------------

-- | Does the line have a top-level (depth-0, outside strings) @\<-@?
hasTopArrow :: Text -> Bool
hasTopArrow = go (0 :: Int) . T.unpack
  where
    go _ [] = False
    go d ('<' : '-' : _) | d == 0 = True
    go d (c : cs)
        | c == '"' = go d (skipStr cs)
        | c `elem` ("([{" :: String) = go (d + 1) cs
        | c `elem` (")]}" :: String) = go (max 0 (d - 1)) cs
        | otherwise = go d cs

{- | Does the line have a top-level standalone @=@ (a binding), not @==@\/@\/=@
\/@\<=@\/@>=@ and not inside parens or strings?
-}
hasTopEquals :: Text -> Bool
hasTopEquals = go (0 :: Int) ' ' . T.unpack
  where
    go _ _ [] = False
    go d prev (c : cs)
        | c == '"' = go d '"' (skipStr cs)
        | c `elem` ("([{" :: String) = go (d + 1) c cs
        | c `elem` (")]}" :: String) = go (max 0 (d - 1)) c cs
        | c == '='
        , d == 0
        , prev `notElem` ("=/<>!" :: String)
        , nextChar cs /= Just '=' =
            True
        | otherwise = go d c cs
    nextChar (x : _) = Just x
    nextChar [] = Nothing

-- | Does the line have a top-level (depth-0, outside strings) @::@?
hasTopDoubleColon :: Text -> Bool
hasTopDoubleColon = go (0 :: Int) . T.unpack
  where
    go _ [] = False
    go d (':' : ':' : _) | d == 0 = True
    go d (c : cs)
        | c == '"' = go d (skipStr cs)
        | c `elem` ("([{" :: String) = go (d + 1) cs
        | c `elem` (")]}" :: String) = go (max 0 (d - 1)) cs
        | otherwise = go d cs

-- | The binder on the left of a top-level @\<-@.
binderOf :: [Text] -> Text
binderOf [] = ""
binderOf (l : _) = T.strip (fst (T.breakOn "<-" l))

skipStr :: String -> String
skipStr [] = []
skipStr ('\\' : _ : cs) = skipStr cs
skipStr ('"' : cs) = cs
skipStr (_ : cs) = skipStr cs

unlinesU :: [Text] -> Text
unlinesU = T.intercalate "\n"
