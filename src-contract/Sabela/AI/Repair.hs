module Sabela.AI.Repair (
    firstJustM,
    interleave,
) where

firstJustM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m (Maybe (a, b))
firstJustM _ [] = pure Nothing
firstJustM check (x : xs) = do
    r <- check x
    case r of
        Just b -> pure (Just (x, b))
        Nothing -> firstJustM check xs

interleave :: [[a]] -> [a]
interleave xss = case [(x, rest) | (x : rest) <- xss] of
    [] -> []
    pairs -> map fst pairs ++ interleave (map snd pairs)
