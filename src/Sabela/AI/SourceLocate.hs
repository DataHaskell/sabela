{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Finds a definition inside real module source, by a ladder that never
gives up on unparseable files: ghc-lib-parser with the file's own LANGUAGE
pragmas, then a lexical column-0 scan for the CPP-ridden rest of Hackage.
-}
module Sabela.AI.SourceLocate (
    Located (..),
    DeclSlice (..),
    Outline (..),
    declSlice,
    exportsName,
    importedModules,
    moduleOutline,
    fileDynFlags,
    nearest,
) where

import Data.List (sortOn)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

import GHC.Driver.Session (
    DynFlags,
    flagSpecFlag,
    flagSpecName,
    xFlags,
    xopt_set,
 )
import qualified GHC.Hs as Hs
import GHC.Parser.Annotation (getLocA)
import GHC.Parser.Lexer (ParseResult (..))
import GHC.Types.SrcLoc (
    GenLocated (..),
    SrcSpan (..),
    srcSpanEndLine,
    srcSpanStartLine,
    unLoc,
 )
import qualified Language.Haskell.GhclibParserEx.GHC.Parser as P

import Sabela.AI.HoleRepair (editDistance)
import Sabela.AI.SourceLocate.Scan (Row (..), scannedRows)
import Sabela.Parse (parserDynFlags)
import Sabela.Parse.Ast (topLevelDefsFromDecl, topLevelSigsFromDecl)
import Sabela.Parse.Ast.Names (rdrText)

-- | How the span was found: a parse, or the lexical fallback.
data Located = Parsed | Scanned
    deriving (Eq, Show)

data DeclSlice = DeclSlice
    { dsFrom :: Int
    , dsTo :: Int
    , dsText :: Text
    , dsHow :: Located
    }
    deriving (Eq, Show)

data Outline = Outline
    { oHeader :: Text
    , oDecls :: [(Text, Int, Maybe Text)]
    , oHow :: Located
    }
    deriving (Eq, Show)

{- | The parser baseline plus the extensions the file's own LANGUAGE pragmas
name. A name 'xFlags' does not know is skipped, not an error.
-}
fileDynFlags :: Text -> DynFlags
fileDynFlags src = foldl xopt_set parserDynFlags pragmaExts
  where
    pragmaExts =
        [ flagSpecFlag fs
        | name <- pragmaNames src
        , fs <- xFlags
        , T.pack (flagSpecName fs) == name
        ]

pragmaNames :: Text -> [Text]
pragmaNames src =
    [ T.strip name
    | l <- T.lines src
    , Just body <- [T.stripPrefix "{-#" (T.strip l)]
    , let inner = T.strip (fromMaybe body (T.stripSuffix "#-}" body))
    , Just names <- [T.stripPrefix "LANGUAGE" inner]
    , name <- T.splitOn "," names
    ]

-- | The named definition's source, or the nearest names the module declares.
declSlice :: Text -> Text -> Either [Text] DeclSlice
declSlice src name = case entities src of
    (how, ents) ->
        case [e | e <- ents, name `S.member` entNames e] of
            (e : _) ->
                Right
                    DeclSlice
                        { dsFrom = entFrom e
                        , dsTo = entTo e
                        , dsText = sliceLines src (entFrom e) (entTo e)
                        , dsHow = how
                        }
            [] -> Left (nearest name (allNames ents))

moduleOutline :: Text -> Outline
moduleOutline src = case entities src of
    (how, ents) ->
        Outline
            { oHeader = headerText src
            , oDecls =
                [(entPrimary e, entFrom e, entSig e) | e <- ents]
            , oHow = how
            }

-- --- entities: sig-plus-body groups, either ladder rung -------------------

data Entity = Entity
    { entNames :: S.Set Text
    , entPrimary :: Text
    , entFrom :: Int
    , entTo :: Int
    , entSig :: Maybe Text
    }

entities :: Text -> (Located, [Entity])
entities src = case parsedRows src of
    Just rows -> (Parsed, groupRows src rows)
    Nothing -> (Scanned, groupRows src (scannedRows src))

allNames :: [Entity] -> [Text]
allNames = S.toList . S.unions . map entNames

nearest :: Text -> [Text] -> [Text]
nearest w names =
    take 5 [n | n <- sortOn (editDistance w) names, editDistance w n <= 3]

sliceLines :: Text -> Int -> Int -> Text
sliceLines src from to =
    T.unlines (take (to - from + 1) (drop (from - 1) (T.lines src)))

{- | Whether the module's export list states the name — the shape where a
name is answered for by this module but defined in one of its imports.
-}
exportsName :: Text -> Text -> Bool
exportsName src name =
    name `elem` T.split (not . identChar) (headerText src)
  where
    identChar c = c == '_' || c == '\'' || c `notElem` breakers
    breakers = " \t\n(),;[]{}" :: String

{- | The modules the file imports, lexically, so it works on files the parser
refused too.
-}
importedModules :: Text -> [Text]
importedModules src =
    [ m
    | l <- T.lines src
    , Just rest <- [T.stripPrefix "import " l]
    , (m : _) <-
        [ [ w
          | w <- T.words rest
          , w /= "qualified"
          , not ("{-" `T.isPrefixOf` w)
          ]
        ]
    ]

-- | The module header: its opening line through the one that closes it.
headerText :: Text -> Text
headerText src =
    case break (T.isPrefixOf "module") (T.lines src) of
        (_, rest@(_ : _)) ->
            let (upto, after) = break (T.isInfixOf "where") rest
             in T.unlines (upto <> take 1 after)
        _ -> ""

{- | Adjacent rows sharing a name fold into one entity, so a signature and
its equations answer as one span. A signature alone still answers.
-}
groupRows :: Text -> [Row] -> [Entity]
groupRows src = go
  where
    go [] = []
    go (r : rest) =
        let (same, others) = span (shares r) rest
            grp = r : same
         in entity grp : go others
    shares a b = not (S.null (rowNames a `S.intersection` rowNames b))
    entity grp =
        Entity
            { entNames = S.unions (map rowNames grp)
            , entPrimary =
                fromMaybe
                    (rowPrimary (head grp))
                    (listToMaybe [rowPrimary r | r <- grp, not (rowIsSig r)])
            , entFrom = minimum (map rowFrom grp)
            , entTo = maximum (map rowTo grp)
            , entSig =
                listToMaybe
                    [ T.strip (sliceLines src (rowFrom r) (rowFrom r))
                    | r <- grp
                    , rowIsSig r
                    ]
            }

-- --- rung 1: the parser ----------------------------------------------------

parsedRows :: Text -> Maybe [Row]
parsedRows src = case P.parseModule (T.unpack src) (fileDynFlags src) of
    POk _ (L _ hsMod) ->
        Just (mapMaybe row (Hs.hsmodDecls hsMod))
    PFailed _ -> Nothing
  where
    row ld = do
        (from, to) <- realSpan (getLocA ld)
        let d = unLoc ld
            defs = topLevelDefsFromDecl d
            sigs = topLevelSigsFromDecl d
            names = defs `S.union` sigs
        primary <- primaryName d `orMin` names
        pure
            Row
                { rowNames = names
                , rowPrimary = primary
                , rowIsSig = not (S.null sigs)
                , rowFrom = from
                , rowTo = to
                }
    orMin mp names = case mp of
        Just p -> Just p
        Nothing -> fst <$> S.minView names

realSpan :: SrcSpan -> Maybe (Int, Int)
realSpan = \case
    RealSrcSpan r _ -> Just (srcSpanStartLine r, srcSpanEndLine r)
    _ -> Nothing

-- | The name a decl is indexed under: the type's, not its constructors'.
primaryName :: Hs.HsDecl Hs.GhcPs -> Maybe Text
primaryName = \case
    Hs.ValD _ Hs.FunBind{Hs.fun_id = ln} -> Just (rdrText (unLoc ln))
    Hs.TyClD _ tcd -> case tcd of
        Hs.DataDecl{Hs.tcdLName = ln} -> Just (rdrText (unLoc ln))
        Hs.SynDecl{Hs.tcdLName = ln} -> Just (rdrText (unLoc ln))
        Hs.ClassDecl{Hs.tcdLName = ln} -> Just (rdrText (unLoc ln))
        Hs.FamDecl _ fd -> Just (rdrText (unLoc (Hs.fdLName fd)))
    _ -> Nothing
