{-# LANGUAGE OverloadedStrings #-}

{- | Arity repair: a weak model over-applies a function (@D.col \@Double "revenue"
df@ against @col :: Text -> Expr a@) and GHC reports a function-shaped expected
type. The model named the right components; only the application is wrong, so the
compiler decides the arity and we trim — the model never re-picks.

Arity comes from probing with @undefined@ rather than parsing the head's
signature: @undefined :: a@ unifies with anything, so applying it k times asks GHC
"does this head take k arguments?" directly. There is no type parser in the tree
('Sabela.AI.Grammar.Synth.resultHead' splits on @->@ and mis-reads higher-order
arguments), so the probe is both cheaper and more correct than reading the type.
-}
module Sabela.AI.Capabilities.Edit.Assemble (
    applicationCandidates,
    probeArity,
    vacuous,
) where

import Data.Char (isAlphaNum)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.Edit.Repair (resultErrorText)
import Sabela.AI.Capabilities.Util (featureEnabled)
import Sabela.AI.Health (healthOfTypeQuery, isClean)
import Sabela.AI.HoleRepair (arityFromError)
import Sabela.AI.Spine (
    Spine (..),
    renderSpine,
    spineArity,
    splitSpine,
    trimTo,
 )
import Sabela.AI.Types (ExecutionResult (..))
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..), getHaskellSession)

{- | Most value arguments a probe will try before giving up. A head taking more
than this is not the shape this repair targets.
-}
maxProbeArity :: Int
maxProbeArity = 4

{- | Candidate sources for a misapplication: for each over-applied sub-spine in
the red source, the source with that spine trimmed to its real arity. Default ON;
set @SABELA_ARITY_FIX=0@ to disable (then this is the empty list).

Empty unless GHC reported a function-shaped expected type ('arityFromError'), so
a coercion or not-in-scope error never reaches here.
-}
applicationCandidates ::
    App -> Either Text ExecutionResult -> Text -> IO [Text]
applicationCandidates app res src = do
    enabled <- featureEnabled "SABELA_ARITY_FIX"
    mBackend <- getHaskellSession (appSessions app)
    case (enabled, mBackend, arityFromError (resultErrorText res)) of
        (True, Just backend, Just _) -> candidatesFor backend
        _ -> pure []
  where
    candidatesFor backend = do
        fixes <- mapM (trimmed backend) (overApplied src)
        pure [s | Just s <- fixes, s /= src, not (vacuous s)]
    -- One candidate per sub-spine whose real arity is below what the model applied.
    -- The group carries its parens, so the trim must re-wrap or the sub-expression
    -- would splice into the enclosing application and re-associate.
    trimmed backend (whole, sp) = do
        mArity <- headArity backend (resultErrorText res) (spHead sp)
        pure $ case mArity of
            Just n
                | n < spineArity sp ->
                    Just (T.replace whole (parenthesise (trimTo n sp)) src)
            _ -> Nothing
    parenthesise sp = "(" <> renderSpine sp <> ")"

{- | The parenthesised sub-expressions of a source that read as plain
applications, paired with their spine. The over-applied head is nested inside the
outer call (@D.sum (D.col … df) df@), so the inner groups are what we probe.
-}
overApplied :: Text -> [(Text, Spine)]
overApplied src = mapMaybe withSpine (parenGroups src)
  where
    withSpine g = (,) g <$> splitSpine (unwrap g)
    unwrap g = T.dropEnd 1 (T.drop 1 g)

{- | The balanced parenthesised groups in a source, outermost first, then the
groups nested inside each. Scans with a depth counter, honouring string literals
so a paren inside a literal never opens a group.
-}
parenGroups :: Text -> [Text]
parenGroups = go . T.unpack
  where
    go [] = []
    go ('"' : cs) = go (skipStr cs)
    go ('(' : cs) = case grab 1 [] cs of
        -- Recurse into the group too: the over-applied head can sit at any depth.
        Just (body, rest) ->
            let whole = T.pack ('(' : body)
             in whole : parenGroups (T.pack (init body)) ++ go rest
        Nothing -> []
    go (_ : cs) = go cs
    skipStr [] = []
    skipStr ('\\' : _ : cs) = skipStr cs
    skipStr ('"' : cs) = cs
    skipStr (_ : cs) = skipStr cs
    grab :: Int -> String -> String -> Maybe (String, String)
    grab _ _ [] = Nothing
    grab d acc ('"' : cs) =
        let (lit, rest) = spanStr cs
         in grab d (acc ++ '"' : lit) rest
    grab d acc ('(' : cs) = grab (d + 1) (acc ++ "(") cs
    grab d acc (')' : cs)
        | d == 1 = Just (acc ++ ")", cs)
        | otherwise = grab (d - 1) (acc ++ ")") cs
    grab d acc (c : cs) = grab d (acc ++ [c]) cs
    spanStr [] = ([], [])
    spanStr ('\\' : c : cs) = let (l, r) = spanStr cs in ('\\' : c : l, r)
    spanStr ('"' : cs) = ("\"", cs)
    spanStr (c : cs) = let (l, r) = spanStr cs in (c : l, r)

{- | The head's real arity: GHC's own word when it gives one, else the probe.

On an over-application GHC often states it outright — @"The function `D.col' is
applied to two visible arguments, but its type `…' has only one"@ — which is free
and exact. The probe is the fallback for the errors that do not say.
-}
headArity :: ST.SessionBackend -> Text -> Text -> IO (Maybe Int)
headArity backend errText hd = case statedArity errText hd of
    Just n -> pure (Just n)
    Nothing -> probeArity backend hd

{- | The arity GHC stated for @head@, from @"The function `f' is applied to two
visible arguments, but its type `…' has only one"@. GHC spells the counts as
words. 'Nothing' unless the message names THIS head, so a nested error cannot
retype the wrong function.
-}
statedArity :: Text -> Text -> Maybe Int
statedArity errText hd = do
    rest <- afterInfix ("The function `" <> hd <> "' is applied to") errText
    only <- afterInfix "has only " rest
    numberWord (T.takeWhile (/= '\n') only)
  where
    afterInfix needle t = case T.breakOn needle t of
        (_, r) | not (T.null r) -> Just (T.drop (T.length needle) r)
        _ -> Nothing

-- | GHC spells small argument counts as words.
numberWord :: Text -> Maybe Int
numberWord t = lookup (T.takeWhile (/= ' ') (T.strip t)) tbl
  where
    tbl = [("one", 1), ("two", 2), ("three", 3), ("four", 4), ("five", 5)]

{- | The value arity of @head@, by applying @undefined@ until GHC objects: the
largest @k <= 'maxProbeArity'@ for which @head undefined … undefined@ still
@:type@s clean. 'Nothing' when even the bare head does not check.
-}
probeArity :: ST.SessionBackend -> Text -> IO (Maybe Int)
probeArity backend headTok = do
    ok0 <- checks 0
    if not ok0 then pure Nothing else Just <$> climb 0
  where
    climb k
        | k >= maxProbeArity = pure k
        | otherwise = do
            ok <- checks (k + 1)
            if ok then climb (k + 1) else pure k
    checks k = do
        out <- ST.sbQueryType backend (applied k)
        pure (isClean (healthOfTypeQuery out))
    applied k = T.unwords (headTok : replicate k "undefined")

{- | Whether a candidate carries @undefined@. It is the arity probe and must never
be a fill: it inhabits every type, so a candidate carrying one would @:type@ clean
and commit a wrecked cell. A trim can only ever DROP tokens, so no other vacuous
inhabitant (@const@, @error@) can be introduced that the model did not already
write — and those have honest uses (@either (error . show) id@), so blacklisting
them here would block real repairs.
-}
vacuous :: Text -> Bool
vacuous s = "undefined" `elem` T.split (not . identChar) s
  where
    identChar c = isAlphaNum c || c `elem` ("_'." :: String)
