{-# LANGUAGE OverloadedStrings #-}

module Sabela.Topo (
    TopoResult (..),
    buildDefMap,
    buildDepGraph,
    topoSort,
    computeTopoOrder,
    selectAffectedTopo,
    cellNames,
) where

import Data.Char (isAlpha, isAlphaNum, isAsciiUpper)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.Model (Cell (..))

data TopoResult = TopoResult
    { trOrdered :: [Cell]
    -- ^ Cells in safe execution order (no redef cells, no cycle cells)
    , trCycleIds :: S.Set Int
    -- ^ Cell IDs that are part of a circular dependency
    }

{- | Extract definitions and uses from a cell's source.

Pre-strips string literals, character literals, line comments, and block
comments so tokens inside @"..."@ / @'...'@ / @-- ...@ / @{\- ... -\}@ are not
mistakenly picked up as dependency edges. The stripping preserves line breaks
so the line-level @extractDefs@ still sees the right line numbers.

Use-extraction is **scope-aware**: function parameters on the LHS of a value
binding are treated as local and subtracted from that line's RHS uses
(and from subsequent indented continuation lines, which belong to the same
binding). Without this, @f x = ...@ in one cell and @g x = ...@ in another
would look like they "use" each other's @x@, producing spurious dependency
edges and false cycles.
-}
cellNames :: Text -> (S.Set Text, S.Set Text)
cellNames src = (defs, uses)
  where
    cleaned = stripLiteralsAndComments src
    ls = T.lines cleaned
    -- Base defs: everything `extractDefs` picks up line-by-line (patterns
    -- with `=` / `<-` on the same line, plus data/type/class/newtype).
    baseDefs = S.fromList $ concatMap extractDefs ls
    -- Plus multi-line guarded headers: an unindented "f x y" line (no `=`)
    -- whose next line is indented and contains `=` or `|`. `extractDefs`
    -- misses these because it operates on a single line at a time.
    headerDefs = S.fromList $ multiLineHeaderDefs ls
    defs = S.union baseDefs headerDefs
    uses = scanUses ls

{- | Detect top-level value bindings whose `=` lives on an indented
continuation line, e.g.

> isPrime n
>   | n < 2 = False
>   | otherwise = ...

Returns the function names from such headers. Line-pair scan with
one-line lookahead.
-}
multiLineHeaderDefs :: [Text] -> [Text]
multiLineHeaderDefs = go
  where
    go [] = []
    go [_] = []
    go (l1 : l2 : rest) = case headerOf l1 l2 of
        Just f -> f : go (l2 : rest)
        Nothing -> go (l2 : rest)
    headerOf l1 l2
        | isUnindented l1
        , not (T.null (T.strip l1))
        , noEquals l1
        , notSkippable l1
        , isIndented l2
        , hasEqualsOrGuard l2 =
            firstLowerIdentTok (T.strip l1)
        | otherwise = Nothing
    isUnindented t =
        not (T.null t) && not (T.head t == ' ' || T.head t == '\t')
    isIndented t =
        not (T.null t) && (T.head t == ' ' || T.head t == '\t')
    noEquals t =
        not (T.isInfixOf "=" t) && not (T.isInfixOf "<-" t)
    notSkippable t =
        let s = T.strip t
         in not
                ( "import " `T.isPrefixOf` s
                    || "{-#" `T.isPrefixOf` s
                    || ":" `T.isPrefixOf` s
                    || "--" `T.isPrefixOf` s
                )
    hasEqualsOrGuard t = T.isInfixOf "=" t || T.isInfixOf "|" t
    firstLowerIdentTok t = case T.words t of
        (w : _) | isLowerIdent w -> Just w
        _ -> Nothing

{- | Walk the cleaned lines tracking "current binding's params" across indented
continuations. For each line, emit its identifier uses minus any params that
belong to the enclosing top-level binding. Resets params on each new
unindented line.
-}
scanUses :: [Text] -> S.Set Text
scanUses = go S.empty S.empty
  where
    go :: S.Set Text -> S.Set Text -> [Text] -> S.Set Text
    go _params acc [] = acc
    go params acc (line : rest)
        | T.null (T.strip line) = go params acc rest
        | isIndentedLine line =
            -- Continuation of the previous top-level binding (or free
            -- indented material). Strip current params, collect the rest.
            let toks = S.fromList (extractTokens line)
                newUses = toks `S.difference` params
             in go params (S.union acc newUses) rest
        | skipLine line =
            -- Import, pragma, GHCi directive, or a lone comment: no params
            -- and no free uses to collect.
            go S.empty acc rest
        | otherwise =
            -- Unindented normal line. Either a value-binding (possibly with
            -- params) or a free expression. Reset params from prior line and
            -- compute this line's contribution.
            let (newParams, addedUses) = analyseTopLevelLine line
             in go newParams (S.union acc addedUses) rest

    isIndentedLine line =
        not (T.null line) && (T.head line == ' ' || T.head line == '\t')

    skipLine line =
        let s = T.strip line
         in "import " `T.isPrefixOf` s
                || "{-#" `T.isPrefixOf` s
                || ":" `T.isPrefixOf` s

{- | Inspect an unindented line. Returns @(params-in-scope-for-continuations,
uses-to-add)@. For @f x y = body@ lines the function body's uses are the
RHS minus the params; the returned params propagate to any indented
continuation lines. For a bare header like @f x y@ (the line before a
guarded body) we also return the params but no uses from the header itself.
For a free expression we treat all tokens as uses.
-}
analyseTopLevelLine :: Text -> (S.Set Text, S.Set Text)
analyseTopLevelLine line =
    let toks = extractTokens line
     in case T.breakOn "=" line of
            (_, "") ->
                -- No `=` on this line. Two plausible shapes: a multi-line
                -- binding header (`f x y` followed by indented guards) or a
                -- free function-call expression (`print x`). We disambiguate
                -- only structurally — if the first token is a lower-case
                -- identifier and there are subsequent tokens, assume header
                -- form and stash the rest as params; otherwise, free
                -- expression → every token is a use.
                case toks of
                    (f : ps)
                        | isLowerIdent f && not (null ps) ->
                            (S.fromList ps, S.empty)
                    _ -> (S.empty, S.fromList toks)
            (lhs, eqAndRest) ->
                let lhsToks = extractTokens lhs
                    rhsToks = extractTokens (T.drop 1 eqAndRest)
                 in case lhsToks of
                        (f : ps)
                            | isLowerIdent f ->
                                let params = S.fromList ps
                                    rhsUses = S.fromList rhsToks `S.difference` params
                                 in (params, rhsUses)
                        _ ->
                            -- Data/type/class decl or tuple/record LHS —
                            -- treat both sides as uses, no params.
                            (S.empty, S.fromList (lhsToks ++ rhsToks))

{- | Replace the content of string literals, char literals, line comments, and
block comments with spaces. Preserves line breaks and overall length so
line-oriented analyses still map back to the original source.
-}
stripLiteralsAndComments :: Text -> Text
stripLiteralsAndComments = T.pack . go . T.unpack
  where
    go [] = []
    -- block comment {- ... -}, non-nested, scan to the first matching -}
    go ('{' : '-' : rest) = ' ' : ' ' : eatBlock rest
    -- line comment -- to end of line; keep the newline
    go ('-' : '-' : rest) = ' ' : ' ' : eatLine rest
    -- string literal (handle escapes)
    go ('"' : rest) = ' ' : eatStr rest
    -- char literal 'c' or '\n' or 'x' — leave as space, but only when it
    -- actually looks like a char literal (next char not an ident char that
    -- would make it a primed identifier).
    go ('\'' : c : '\'' : rest) | c /= '\\' = ' ' : ' ' : ' ' : go rest
    go ('\'' : '\\' : _ : '\'' : rest) = ' ' : ' ' : ' ' : ' ' : go rest
    go (c : rest) = c : go rest

    eatBlock [] = []
    eatBlock ('-' : '}' : rest) = ' ' : ' ' : go rest
    eatBlock ('\n' : rest) = '\n' : eatBlock rest
    eatBlock (_ : rest) = ' ' : eatBlock rest

    eatLine [] = []
    eatLine ('\n' : rest) = '\n' : go rest
    eatLine (_ : rest) = ' ' : eatLine rest

    eatStr [] = []
    eatStr ('\\' : c : rest) = ' ' : ' ' : eatStrCont c rest
    eatStr ('"' : rest) = ' ' : go rest
    eatStr ('\n' : rest) = '\n' : eatStr rest
    eatStr (_ : rest) = ' ' : eatStr rest

    eatStrCont '\n' rest = '\n' : eatStr rest
    eatStrCont _ rest = eatStr rest

extractDefs :: Text -> [Text]
extractDefs line
    | isIndented = []
    | T.null s = []
    | T.isPrefixOf "--" s = []
    | T.isPrefixOf ":" s = []
    | T.isPrefixOf "import " s = []
    | T.isPrefixOf "{-#" s = []
    | Just rest <- stripKW "let" s = firstLowerIdent rest
    | Just rest <- stripKW "data" s = firstAnyIdent rest
    | Just rest <- stripKW "type" s = firstAnyIdent rest
    | Just rest <- stripKW "newtype" s = firstAnyIdent rest
    | Just rest <- stripKW "class" s = firstAnyIdent rest
    | otherwise = extractValueBinding s
  where
    s = T.strip line
    isIndented = not (T.null line) && (T.head line == ' ' || T.head line == '\t')

extractValueBinding :: Text -> [Text]
extractValueBinding s =
    let toks = T.words s
     in case toks of
            (w : rest)
                | isLowerIdent w
                , any (\t -> t == "=" || t == "<-") (take 8 rest) ->
                    [w]
            _ -> []

stripKW :: Text -> Text -> Maybe Text
stripKW kw t = case T.stripPrefix kw t of
    Just rest
        | T.null rest -> Nothing
        | not (isIdentChar (T.head rest)) -> Just (T.stripStart rest)
    _ -> Nothing

firstLowerIdent :: Text -> [Text]
firstLowerIdent t =
    let w = T.takeWhile isIdentChar (T.stripStart t)
     in [w | isLowerIdent w]

firstAnyIdent :: Text -> [Text]
firstAnyIdent t =
    let w = T.takeWhile isIdentChar (T.stripStart t)
     in [w | not (T.null w), isAlpha (T.head w) || T.head w == '_']

isLowerIdent :: Text -> Bool
isLowerIdent t =
    not (T.null t)
        && let c = T.head t in (isAlpha c && not (isAsciiUpper c)) || c == '_'

isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_' || c == '\''

extractTokens :: Text -> [Text]
extractTokens = filter isIdent . T.split (not . isIdentChar)
  where
    isIdent t = not (T.null t) && (isAlpha (T.head t) || T.head t == '_')

{- | Build @defMap@ = name → canonical-cell-id. Uses **last-wins** semantics:
the LAST cell (in notebook order) to define a name takes ownership. This
matches how GHCi actually handles redefinition — a second @let f = ...@ in the
same session shadows the first — and lets users iterate on definitions by
dropping a new cell below rather than deleting the old one.

The second component of the tuple (@redefMap@) is retained as @M.empty@ for
call-site compatibility, but it is always empty. Any consumer code that still
unions it into a @skipIds@ set is a no-op.
-}
buildDefMap :: [Cell] -> (M.Map Text Int, M.Map Int [Text])
buildDefMap cells = (foldl step M.empty cells, M.empty)
  where
    step defMap cell =
        let cid = cellId cell
            (defs, _) = cellNames (cellSource cell)
         in S.foldl' (\m name -> M.insert name cid m) defMap defs

{- | Build dependency graph: cell ID -> set of cell IDs it depends on.
Based on defMap: a cell depends on the cell that canonically defines each name it uses.
-}
buildDepGraph :: M.Map Text Int -> [Cell] -> M.Map Int (S.Set Int)
buildDepGraph defMap cells = M.fromList [(cellId c, depsOf c) | c <- cells]
  where
    depsOf c =
        let (_, uses) = cellNames (cellSource c)
            cid = cellId c
         in S.delete cid $
                S.fromList
                    [depCid | name <- S.toList uses, Just depCid <- [M.lookup name defMap]]

data TopoState = TopoState
    { tsFilteredDeps :: M.Map Int (S.Set Int)
    , tsRevDeps :: M.Map Int (S.Set Int)
    , tsPositions :: M.Map Int Int
    , tsCellById :: M.Map Int Cell
    }

{- | Kahn's topological sort.
Only processes the provided cells; deps to cells outside this set are ignored
(treated as already satisfied). Leftover cells with unresolved in-degree are cycles.
-}
topoSort :: [Cell] -> M.Map Int (S.Set Int) -> TopoResult
topoSort cells deps =
    let st = buildTopoState cells deps
        inDegree =
            M.fromList
                [ (cellId c, S.size (M.findWithDefault S.empty (cellId c) (tsFilteredDeps st)))
                | c <- cells
                ]
        initQueue = [cellId c | c <- cells, M.findWithDefault 0 (cellId c) inDegree == 0]
        cids = S.fromList (map cellId cells)
        (ordered, finalDeg) = runKahns st initQueue inDegree []
        cycleIds = S.fromList [cid | cid <- S.toList cids, M.findWithDefault 0 cid finalDeg > 0]
     in TopoResult{trOrdered = ordered, trCycleIds = cycleIds}

buildTopoState :: [Cell] -> M.Map Int (S.Set Int) -> TopoState
buildTopoState cells deps =
    let cids = S.fromList (map cellId cells)
        filteredDeps =
            M.fromList
                [ (cellId c, S.intersection cids (M.findWithDefault S.empty (cellId c) deps))
                | c <- cells
                ]
        revDeps =
            M.fromListWith
                S.union
                [ (dep, S.singleton cid)
                | (cid, depSet) <- M.toList filteredDeps
                , dep <- S.toList depSet
                ]
     in TopoState
            { tsFilteredDeps = filteredDeps
            , tsRevDeps = revDeps
            , tsPositions = M.fromList (zip (map cellId cells) [0 :: Int ..])
            , tsCellById = M.fromList [(cellId c, c) | c <- cells]
            }

runKahns ::
    TopoState -> [Int] -> M.Map Int Int -> [Cell] -> ([Cell], M.Map Int Int)
runKahns _ [] deg acc = (acc, deg)
runKahns st queue deg acc =
    let cid = minimumByPos queue (tsPositions st)
        queue' = filter (/= cid) queue
        dependents = S.toList (M.findWithDefault S.empty cid (tsRevDeps st))
        (newlyUnblocked, deg') = foldl unblock ([], deg) dependents
        queue'' = queue' ++ newlyUnblocked
        cell = tsCellById st M.! cid
     in runKahns st queue'' deg' (acc ++ [cell])

unblock :: ([Int], M.Map Int Int) -> Int -> ([Int], M.Map Int Int)
unblock (unblocked, d) depCid =
    let d' = M.adjust (subtract 1) depCid d
        newDeg = M.findWithDefault 0 depCid d'
     in if newDeg == 0 then (depCid : unblocked, d') else (unblocked, d')

minimumByPos :: [Int] -> M.Map Int Int -> Int
minimumByPos xs positions =
    foldl1 (\a b -> if pos a <= pos b then a else b) xs
  where
    pos x = M.findWithDefault maxBound x positions

{- | Compose buildDefMap, buildDepGraph, topoSort.
Under last-wins semantics there are no redef-excluded cells; the returned
redefMap is always empty.
-}
computeTopoOrder :: [Cell] -> (TopoResult, M.Map Int [Text])
computeTopoOrder cells =
    let (defMap, redefMap) = buildDefMap cells
        deps = buildDepGraph defMap cells
        result = topoSort cells deps
     in (result, redefMap)

{- | Find cells transitively downstream of editedCid and return a scoped
TopoResult. The edited cell itself is included. Redef filtering is a no-op
under last-wins (kept in the signature for API compat).
-}
selectAffectedTopo :: Int -> [Cell] -> (TopoResult, M.Map Int [Text])
selectAffectedTopo editedCid cells =
    let (defMap, redefMap) = buildDefMap cells
        deps = buildDepGraph defMap cells
        affected = computeAffectedSet editedCid deps
        toSort = filter (\c -> S.member (cellId c) affected) cells
        result = topoSort toSort deps
     in (result, redefMap)

computeAffectedSet :: Int -> M.Map Int (S.Set Int) -> S.Set Int
computeAffectedSet editedCid deps =
    let revDeps =
            M.fromListWith
                S.union
                [ (dep, S.singleton cid)
                | (cid, depSet) <- M.toList deps
                , dep <- S.toList depSet
                ]
     in bfsAffected (S.singleton editedCid) (S.singleton editedCid) revDeps

bfsAffected :: S.Set Int -> S.Set Int -> M.Map Int (S.Set Int) -> S.Set Int
bfsAffected visited frontier revDeps
    | S.null frontier = visited
    | otherwise =
        let next = S.unions [M.findWithDefault S.empty n revDeps | n <- S.toList frontier]
            newNext = S.difference next visited
         in bfsAffected (S.union visited newNext) newNext revDeps
