{-# LANGUAGE OverloadedStrings #-}

{- | Turning a stretch of notebook cells into a component module. Pragmas,
imports and package lines are hoisted to the module head; the trailing
expression of the last cell (what the notebook showed) stays behind as the
call site, so the notebook goes on calling what it used to define.
-}
module Sabela.Components.Extract (
    Extraction (..),
    extractModule,
    notebookOnlyNames,
) where

import Data.Char (isSpace)
import Data.List (nub)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.Model (Cell (..))
import Sabela.Parse (cellNames)

data Extraction = Extraction
    { exSource :: Text
    , exExports :: [Text]
    , exWarnings :: [Text]
    , exReplacement :: Text
    }
    deriving (Eq, Show)

{- | Names that exist only at the notebook prompt. A component is a real
module, so it cannot reach them; using one is reported rather than failing
later with a confusing compile error.
-}
notebookOnlyNames :: [Text]
notebookOnlyNames =
    [ "display"
    , "displayHtml"
    , "displayMarkdown"
    , "displaySvg"
    , "displayLatex"
    , "displayJson"
    , "displayImage"
    , "table"
    , "tableOf"
    , "tableOfTyped"
    , "html"
    , "md"
    , "svg"
    , "latex"
    , "json"
    , "png"
    , "jpeg"
    , "image"
    , "video"
    , "audio"
    , "mkWidget"
    , "slider"
    , "dropdown"
    , "checkbox"
    , "textInput"
    , "button"
    , "widgetGet"
    , "widgetRead"
    , "scatterSelect"
    , "chartInput"
    , "Tabular"
    , "Rich"
    , "Input"
    ]

-- | The module text for cells under a name, with what the notebook keeps.
extractModule :: Text -> Text -> [Cell] -> Extraction
extractModule modName purpose cells =
    Extraction
        { exSource = moduleText
        , exExports = exports
        , exWarnings = warnings
        , exReplacement = replacement
        }
  where
    parts = map (classify . cellSource) cells
    pragmas = nub (concatMap cpPragmas parts)
    imports = nub (concatMap cpImports parts)
    deps = nub (concatMap cpDeps parts)
    bodies =
        filter
            (not . T.null . T.strip)
            (map (T.unlines . cpBody) (initOrEmpty parts) ++ lastBody)
    (lastBody, trailing) = case reverse parts of
        [] -> ([], [])
        (lastPart : _) ->
            let (decls, expr) = splitTrailing (cpBody lastPart)
             in ([T.unlines decls | not (null decls)], expr)
    defs = S.toList (S.unions [fst (cellNames (cellSource c)) | c <- cells])
    uses = S.unions [snd (cellNames (cellSource c)) | c <- cells]
    exports = filter (not . T.isPrefixOf "_") defs
    binds = [l | p <- parts, l <- cpBody p, isBind l]
    promptNames = [n | n <- notebookOnlyNames, n `S.member` uses]
    warnings =
        [ "These lines bind with `<-` at the top level, which only works at the notebook prompt; wrap them in a function or an IO action: "
            <> T.intercalate "; " (map T.strip binds)
        | not (null binds)
        ]
            ++ [ "These names exist only in the notebook, not in a module: "
                    <> T.intercalate ", " promptNames
                    <> ". Pass values in as arguments, or keep that line in the notebook."
               | not (null promptNames)
               ]
            ++ [ "No top-level definitions were found; the component would export nothing."
               | null exports
               ]
    moduleText =
        (<> "\n") . T.stripEnd . T.unlines $
            ( pragmas
                ++ [ "{- | "
                        <> ( if T.null (T.strip purpose)
                                then "Extracted from the notebook."
                                else T.strip purpose
                           )
                        <> " -}"
                   ]
                ++ ["-- Sabela: kind=function"]
                ++ ["-- cabal: build-depends: " <> T.intercalate ", " deps | not (null deps)]
                ++ ["module " <> modName <> " ("]
                ++ ["    " <> e <> "," | e <- exports]
                ++ [") where", ""]
                ++ imports
                ++ ["" | not (null imports)]
                ++ [T.intercalate "\n" bodies]
            )
    replacement =
        T.unlines
            ( ("import " <> modName)
                : ( if null trailing
                        then ["", "-- uses: " <> T.intercalate ", " exports]
                        else "" : trailing
                  )
            )

data CellParts = CellParts
    { cpPragmas :: [Text]
    , cpImports :: [Text]
    , cpDeps :: [Text]
    , cpBody :: [Text]
    }

initOrEmpty :: [a] -> [a]
initOrEmpty [] = []
initOrEmpty xs = init xs

-- | Sort a cell's lines into what goes to the module head and what is body.
classify :: Text -> CellParts
classify src = foldr step (CellParts [] [] [] []) (T.lines src)
  where
    step l p
        | "{-# LANGUAGE" `T.isPrefixOf` T.strip l =
            p{cpPragmas = T.strip l : cpPragmas p}
        | Just r <- T.stripPrefix "-- cabal: build-depends:" (T.strip l) =
            p{cpDeps = filter (not . T.null) (map T.strip (T.splitOn "," r)) ++ cpDeps p}
        | "-- cabal:" `T.isPrefixOf` T.strip l = p
        | "-- compile" `T.isPrefixOf` T.strip l = p
        | "import " `T.isPrefixOf` l = p{cpImports = T.stripEnd l : cpImports p}
        | otherwise = p{cpBody = l : cpBody p}

{- | The last cell's trailing expression: a run of lines at the end that
starts at column zero with something that is neither a declaration nor a
bind, plus the indented lines that continue it. The notebook keeps it as
the call site.
-}
splitTrailing :: [Text] -> ([Text], [Text])
splitTrailing ls =
    let trimmed = reverse (dropWhile (T.null . T.strip) (reverse ls))
        (continuation, rest) = span indentedOrBlank (reverse trimmed)
     in case rest of
            (l : before)
                | isExprStart l -> (reverse before, l : reverse continuation)
            _ -> (trimmed, [])
  where
    indentedOrBlank l = T.null (T.strip l) || isSpace (T.head l)
    isExprStart l =
        not (T.null (T.strip l))
            && not (isSpace (T.head l))
            && not (isDecl l)
            && not (isBind l)

isDecl :: Text -> Bool
isDecl l =
    any
        (`T.isPrefixOf` l)
        ["data ", "type ", "newtype ", "class ", "instance ", "import ", "{-", "--"]
        || T.isInfixOf " :: " l
        || isDefinition l
  where
    isDefinition t = case T.words (fst (T.breakOn " = " t)) of
        (w : _)
            | not (T.null w) && " = " `T.isInfixOf` t && not (T.isPrefixOf " " t) ->
                T.all (\c -> c /= '(' && c /= '[') (T.take 1 w)
        _ -> False

isBind :: Text -> Bool
isBind l =
    not (T.isPrefixOf " " l)
        && " <- " `T.isInfixOf` l
        && not ("|" `T.isInfixOf` fst (T.breakOn " <- " l))
