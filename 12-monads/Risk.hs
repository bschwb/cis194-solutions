{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Applicative
import Control.Monad.Random
import Control.Monad
import Data.List (sort)

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

dice :: Int -> Rand StdGen [DieValue]
dice n = replicateM n die

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving Show

exactSuccessProb :: Battlefield -> Double
exactSuccessProb bf = 0

-- | Runs 1000 invades and uses the results to compute a @Double@ between 0 and
-- 1 representing the estimated probability that the attacking army will
-- completely destroy the defending army.
succesProb :: Battlefield -> Rand StdGen Double
succesProb bf = replicateM 1000 (invade bf) >>= success

success :: [Battlefield] -> Rand StdGen Double
success bfs = return $ fromIntegral (length x)  / fromIntegral (length bfs)
  where x = filter ((== 0) . defenders) bfs

-- | Simulate an invasion attempt which consist of repeating battles until no
-- defenders are remaining, or fewer than two attackers.
invade :: Battlefield -> Rand StdGen Battlefield
invade bf
  | attackers bf < 2 || defenders bf <= 0 = return bf
  | otherwise = battle bf >>= invade

-- | Simulate a single battle in a game of Risk. Simulate randomly rolling the
-- appropriate number of dice and update the two armies. It is assumed that
-- each player attacks or defend with the maximum number of units they are
-- allowed.
battle :: Battlefield -> Rand StdGen Battlefield
battle bf = dice (att+def) >>= \dc ->
            return (remArmy bf (battleOutcome (att, def) dc))
            where (att, def) = getTroops bf

-- | Returns number of fighting troops in a single battle.
-- Assuming only armies with each atleast 1 troops are sent.
getTroops :: Battlefield -> (Army, Army)
getTroops (Battlefield att def) = (attTroop, defTroop)
  where attTroop = if att >= 4 then 3 else att-1
        defTroop = if def >= 2 then 2 else def

remArmy :: Battlefield -> (Army, Army) -> Battlefield
remArmy (Battlefield att def) (lostAtt, lostDef)
  = Battlefield (att - lostAtt) (def - lostDef)

-- | Assume that the first @att@ DieValues are for the attacker
-- Assume att + def = length dc
battleOutcome :: (Army, Army) -> [DieValue] -> (Army, Army)
battleOutcome (att, def) dc = compareDice sortedDice (0, 0)
  where sortedDice = fmapPair (reverse . sort) (splitAt att dc)

fmapPair :: (a -> b) -> (a, a) -> (b, b)
fmapPair f (a, b) = (f a, f b)

compareDice :: ([DieValue], [DieValue]) -> (Army, Army) -> (Army, Army)
compareDice ([], _) troops = troops
compareDice (_, []) troops = troops
compareDice (a:as, d:ds) (attL, defL)
  | a > d     = compareDice (as, ds) (attL, defL+1)
  | otherwise = compareDice (as, ds) (attL+1, defL)

