{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Data.Monoid
import Data.Tree

import Employee

main :: IO ()
main = readFile "company.txt" >>= putStrLn . computeOutput

computeOutput :: String -> String
computeOutput = formatGL . maxFun . read

formatGL :: GuestList -> String
formatGL (GL lst fun) = "Total fun: " ++ show fun ++ "\n" ++ unlines employees
  where employees = map (\(Emp {empName = name}) -> name) lst

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL al af) (GL bl bf) = GL (al++bl) (af+bf)

-- | Adds Employee to the GuestList and update cached Fun score.
glCons :: Employee -> GuestList -> GuestList
glCons emp@(Emp {empFun = ef}) (GL lst gf) = GL (emp:lst) (ef+gf)

moreFun :: GuestList -> GuestList -> GuestList
moreFun a b = if a > b then a else b

{-treeFold :: (b -> a -> b) -> b -> Tree a -> b-}
{-treeFold f init (Node {rootLabel = rl, subForest = sf})-}
  {-= f (foldl (treeFold f) init sf) rl-}

treeFold :: (a -> [b] -> b) -> b -> Tree a -> b
treeFold f init (Node {rootLabel = rl, subForest = sf})
  = f rl (map (treeFold f init) sf)

-- | First part of list is with boss.
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss bestLists = (maximumS withBossL, maximumS withoutBossL)
  where withoutBossL   = map fst bestLists
        -- ^ The new withoutBossList has sub bosses in it.

        withoutSubBoss = map snd bestLists
        withBossL      = map (glCons boss) withoutSubBoss
        -- ^ The new withBossList doesn't have sub bosses in it.

maximumS ::(Monoid a, Ord a) => [a] -> a
maximumS [] = mempty
maximumS lst = maximum lst

maxFun :: Tree Employee -> GuestList
maxFun tree = uncurry max res
  where res = treeFold nextLevel (mempty, mempty) tree

