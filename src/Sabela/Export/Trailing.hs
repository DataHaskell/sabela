{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Build a 'TrailingResolver' that asks a live 'SessionBackend' how
each candidate trailing expression should be emitted in a standalone
module: @IO ()@ as-is, @IO a@ wrapped with @print =<<@, pure @Show@
wrapped with @print@, or unknown (left alone).

Used by the script + literate exporters when 'Sabela.Export.Block'
classifies a unit as a trailing expression — the resolver decides how
to render it so the emitted file compiles.
-}
module Sabela.Export.Trailing (
    mkTrailingResolver,

    -- * Pieces (exposed for testing / reuse)
    resolveOne,
    typeChecks,
    isTypeError,
) where

import Control.Exception (SomeException, try)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.SessionTypes (SessionBackend (..))
import ScriptHs.Render (TrailKind (..), TrailingResolver)

mkTrailingResolver :: Maybe SessionBackend -> [Text] -> IO TrailingResolver
mkTrailingResolver Nothing _ = pure (const TrailUnknown)
mkTrailingResolver (Just sb) exprs = do
    pairs <- mapM (\e -> (,) e <$> resolveOne sb (flatten e)) exprs
    let m = M.fromList pairs
    pure (\e -> M.findWithDefault TrailUnknown e m)
  where
    flatten = T.unwords . filter (not . T.null) . map T.strip . T.lines

resolveOne :: SessionBackend -> Text -> IO TrailKind
resolveOne sb e = do
    -- All probes use only Prelude operators (always in scope in GHCi) on the
    -- exact expression we'd emit, so success means it really compiles.
    ioUnit <- typeChecks sb ("(" <> e <> ") :: IO ()")
    if ioUnit
        then pure TrailIOUnit
        else do
            ioShow <- typeChecks sb ("print =<< (" <> e <> ")")
            if ioShow
                then pure TrailIOShow
                else do
                    isIO <- typeChecks sb ("(" <> e <> ") >> return ()")
                    if isIO
                        then pure TrailIOUnit
                        else do
                            pureShow <- typeChecks sb ("print (" <> e <> ")")
                            pure (if pureShow then TrailPure else TrailUnknown)

-- | Does @:type expr@ succeed (expression is well-typed in the session)?
typeChecks :: SessionBackend -> Text -> IO Bool
typeChecks sb expr = do
    r <- try (sbQueryType sb expr) :: IO (Either SomeException Text)
    pure $ case r of
        Left _ -> False
        Right t -> not (isTypeError t)

isTypeError :: Text -> Bool
isTypeError t =
    let lc = T.toLower t
     in T.null (T.strip t)
            || any
                (`T.isInfixOf` lc)
                [ "not in scope"
                , "error:"
                , "parse error"
                , "no instance"
                , "cannot "
                , "ambiguous"
                , "couldn't match"
                , "could not"
                ]
