module Main where

import BlockWorldType
import Strips

-- data Block = A | B  deriving (Show, Eq, Ord, Enum)
$(deriveBlock) 
data Object = Table | Object Block deriving (Eq, Ord, Show)

data Term = HandEmpty
          | HandHas Block
          | IsTop Block Bool
          | On Block Object
          deriving (Eq, Ord, Show)
data ActionType = Pickup Block
                | Putdown Block
                | Stack Block Block
                | Unstack Block Block
                deriving (Eq, Show)

main :: IO ()
main = do
  let startCondition = [HandEmpty, IsTop A True, IsTop B False, On A (Object B), On B Table]
  let goalCondition  = [HandEmpty, IsTop A False, IsTop B True, On B (Object A), On A Table]
  print "------------- domain ----------------"
  mapM_ print buildDomain
  let plan = strips buildDomain startCondition goalCondition
  print "------------- target ----------------"
  print startCondition
  print goalCondition
  print "------------- plan ----------------"
  mapM_ print plan
  return ()

buildDomain :: Domain
buildDomain = pickups ++ putdowns ++ stacks ++ unstacks
  where
    pickups  = map (buildAction 1 . Pickup) [A ..]
    putdowns = map (buildAction 1 . Putdown) [A ..]
    stacks   = map (buildAction 1 . uncurry Stack) perms
    unstacks = map (buildAction 1 . uncurry Unstack) perms
    perms = [(x, y) | x <- [A ..], y <- [A ..], x /= y]

buildAction :: Int -> ActionType -> Action
buildAction cost aType@(Pickup x) = Action aType (buildPre x) (buildPost x) cost
  where buildPre x  = [HandEmpty, IsTop x True, On x Table]
        buildPost x = [HandHas x, IsTop x False]
buildAction cost aType@(Putdown x) = Action aType (buildPre x) (buildPost x) cost
  where buildPre x  = [HandHas x, IsTop x False]
        buildPost x = [HandEmpty, IsTop x True, On x Table]
buildAction cost aType@(Stack x y) = Action aType (buildPre x y) (buildPost x y) cost
  where buildPre x y  = [HandHas x, IsTop x False, IsTop y True]
        buildPost x y = [HandEmpty, IsTop x True, IsTop y False, On x (Object y)]
buildAction cost aType@(Unstack x y) = Action aType (buildPre x y) (buildPost x y) cost
  where buildPre x y  = [HandEmpty, IsTop x True, IsTop y False, On x (Object y)]
        buildPost x y = [HandHas x, IsTop x False, IsTop y True]

