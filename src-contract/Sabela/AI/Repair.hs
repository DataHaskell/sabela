{- | The shared repair-search core. Both the product and eval repair paths run
the same verify-and-backtrack loop over their own candidate sources and verify
effect; this is the single combinator they adapt to.
-}
module Sabela.AI.Repair (
    firstJustM,
    interleave,
) where

{- | The first candidate whose check yields a @Just@, paired with that value;
short-circuits. The check threads an arbitrary effect, so callers plug in
compile-only, execute-based, or pure verification.
-}
firstJustM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m (Maybe (a, b))
firstJustM _ [] = pure Nothing
firstJustM check (x : xs) = do
    r <- check x
    case r of
        Just b -> pure (Just (x, b))
        Nothing -> firstJustM check xs

{- | Round-robin across candidate groups, so a small execution cap samples
different problems before a second variant of the same one.
-}
interleave :: [[a]] -> [a]
interleave xss = case [(x, rest) | (x : rest) <- xss] of
    [] -> []
    pairs -> map fst pairs ++ interleave (map snd pairs)
