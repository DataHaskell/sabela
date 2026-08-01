{-# LANGUAGE OverloadedStrings #-}

{- | What the probe examined and what came of it. Every claim carries one of
three outcomes, so a run reports how many it checked as well as how many it
broke: an unchecked claim is reported as unchecked, never as a pass.
-}
module Probe.Claim (
    Claim (..),
    Outcome (..),
    Tally (..),
    checkNames,
    idleWitnesses,
    tallyOf,
    witnessChecks,
) where

import qualified Data.Map.Strict as M
import Data.Text (Text)

data Outcome
    = -- | A witness agreed with the claim.
      Backed
    | -- | A witness disagreed: the answer said something untrue.
      Broken Text
    | -- | No witness could see the thing claimed, so nothing was verified.
      Blind Text
    deriving (Eq, Show)

data Claim = Claim
    { clCheck :: Text
    , clQuery :: Text
    , clOutcome :: Outcome
    }
    deriving (Eq, Show)

data Tally = Tally
    { tyBacked :: Int
    , tyBroken :: Int
    , tyBlind :: Int
    }
    deriving (Eq, Show)

instance Semigroup Tally where
    Tally a b c <> Tally d e f = Tally (a + d) (b + e) (c + f)

instance Monoid Tally where
    mempty = Tally 0 0 0

{- | Every check the probe runs, named whether or not this grid reached it, so
a check that examined nothing is visible in the output rather than absent.
-}
checkNames :: [Text]
checkNames =
    ["schema", "eviction", "budget", "card-authority", "install", "card-export"]

{- | The checks that need a witness outside the answer. A run in which one of
these examined nothing has not verified the world at all, whatever else it
found, so it is reported and it fails.
-}
witnessChecks :: [Text]
witnessChecks = ["install", "card-export"]

tallyOf :: [Claim] -> M.Map Text Tally
tallyOf cs = M.fromListWith (<>) [(clCheck c, one (clOutcome c)) | c <- cs]
  where
    one Backed = Tally 1 0 0
    one (Broken _) = Tally 0 1 0
    one (Blind _) = Tally 0 0 1

-- | Witness checks that no claim on this grid ever reached.
idleWitnesses :: M.Map Text Tally -> [Text]
idleWitnesses t = [c | c <- witnessChecks, M.findWithDefault mempty c t == mempty]
