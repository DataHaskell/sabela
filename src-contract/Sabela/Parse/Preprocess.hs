{-# LANGUAGE OverloadedStrings #-}

{- | Turn a Sabela cell's GHCi-style source into a parseable module body for
'Sabela.Parse'. Cell sources are REPL fragments, not modules, so this strips
@:set@/@:type@ directives and @-- cabal:@ metadata, and rewrites statement-form
@let x = e@ and monadic @x \<- e@ into bare top-level bindings. Split out of
'Sabela.Parse' to keep that module under the size cap; 'noTopLevelIn' is also
the predicate the pre-GHC structural validator reuses.
-}
module Sabela.Parse.Preprocess (
    preprocess,
    noTopLevelIn,
) where

import qualified Data.Char as Char
import Data.Text (Text)
import qualified Data.Text as T

{- | Drop GHCi directives and cabal-metadata lines. Rewrite statement-form
@let@ and monadic @\<-@ into top-level bindings so they parse as decls.
Returns the rewritten lines, ready to be glued together as the body of a
synthetic module.
-}
preprocess :: Text -> [Text]
preprocess src = concatMap rewriteLine (T.lines src)
  where
    rewriteLine raw
        | shouldDrop trimmed = []
        | indented = [raw]
        | Just rest <- T.stripPrefix "let " raw
        , noTopLevelIn rest =
            [rest]
        | Just (binder, rhs) <- splitTopLevelArrow raw =
            [binder <> " = " <> rhs]
        | otherwise = [raw]
      where
        trimmed = T.stripStart raw
        indented = raw /= trimmed

    shouldDrop t =
        T.null t
            || ":" `T.isPrefixOf` t
            || "-- cabal:" `T.isPrefixOf` t
            || "--cabal:" `T.isPrefixOf` t

{- | A statement-form @let@ has no top-level @in@. We treat any line that
contains @ in @ at depth 0 as the regular expression-form @let ... in
...@ and leave it alone (let the parser handle it inside an expression
context if it ever ends up there).
-}
noTopLevelIn :: Text -> Bool
noTopLevelIn = go (0 :: Int) (0 :: Int) . T.unpack
  where
    -- Track paren depth and bracket depth. Stop at first top-level " in ".
    go _ _ [] = True
    go p b (' ' : 'i' : 'n' : ' ' : _) | p == 0 && b == 0 = False
    go p b ('(' : rest) = go (p + 1) b rest
    go p b (')' : rest) = go (max 0 (p - 1)) b rest
    go p b ('[' : rest) = go p (b + 1) rest
    go p b (']' : rest) = go p (max 0 (b - 1)) rest
    go p b (_ : rest) = go p b rest

{- | If the line is @ident <- rhs@ at top level, return @Just (ident, rhs)@.
We ignore @\<-@ that appears inside parens/brackets (list-comp generator,
do-block continuation, etc.).
-}
splitTopLevelArrow :: Text -> Maybe (Text, Text)
splitTopLevelArrow t =
    case findTopLevelArrow 0 0 (T.unpack t) of
        Nothing -> Nothing
        Just idx ->
            let (lhs, rhs0) = T.splitAt idx t
                rhs = T.drop 2 rhs0 -- drop "<-"
                lhsTrim = T.strip lhs
             in if isSimpleIdent lhsTrim
                    then Just (lhsTrim, T.stripStart rhs)
                    else Nothing
  where
    findTopLevelArrow :: Int -> Int -> String -> Maybe Int
    findTopLevelArrow _ _ [] = Nothing
    findTopLevelArrow p b ('<' : '-' : _) | p == 0 && b == 0 = Just 0
    findTopLevelArrow p b ('(' : rest) =
        succPos <$> findTopLevelArrow (p + 1) b rest
    findTopLevelArrow p b (')' : rest) =
        succPos <$> findTopLevelArrow (max 0 (p - 1)) b rest
    findTopLevelArrow p b ('[' : rest) =
        succPos <$> findTopLevelArrow p (b + 1) rest
    findTopLevelArrow p b (']' : rest) =
        succPos <$> findTopLevelArrow p (max 0 (b - 1)) rest
    findTopLevelArrow p b (_ : rest) = succPos <$> findTopLevelArrow p b rest

    succPos :: Int -> Int
    succPos = (+ 1)

isSimpleIdent :: Text -> Bool
isSimpleIdent t = case T.uncons t of
    Just (c, rest) ->
        (Char.isLower c || c == '_')
            && T.all isIdentChar rest
    Nothing -> False
  where
    isIdentChar c = Char.isAlphaNum c || c == '_' || c == '\''
