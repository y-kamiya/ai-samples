module Main where

import qualified Data.Map as M

data Block = A | B  deriving (Show, Eq, Ord, Enum)
data HandState = Empty | Has Block deriving Show
data Object = Hand | Table | Object Block deriving Show
data Condition = Condition {
  hand :: HandState
, isTop :: M.Map Block Bool
, on :: M.Map Block Object
} deriving Show

data ActionType = Pickup Block
                | Putdown Block
                | Stack Block Block
                | Unstack Block Block
                deriving Show

data Action = Action {
  actionType :: ActionType
, precondition :: Condition
, postcondition :: Condition
, cost :: Int
} deriving Show

type Domain = [Action]

data NodeInfo = NodeInfo { condition :: Condition
                         , action :: Action
                         , next :: NodeInfo
                         , realCost :: Int
                         , score :: Int
                         , diff :: [Condition]
                         , diffCount :: Int
                         } deriving Show

main :: IO ()
main = do
  let startCondition = Condition Empty (M.fromList [(A, True), (B, False)]) (M.fromList [(A, Object B), (B, Table)])
  let goalCondition  = Condition Empty (M.fromList [(A, False), (B, True)]) (M.fromList [(B, Object A), (A, Table)])
  mapM_ print buildDomain
  let plan = strips buildDomain startCondition goalCondition    
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
  where buildPre x  = Condition Empty (M.singleton x True) (M.singleton x Table)
        buildPost x = Condition (Has x) (M.singleton x True) (M.singleton x Hand)
buildAction cost aType@(Putdown x) = Action aType (buildPre x) (buildPost x) cost
  where buildPre x  = Condition (Has x) (M.singleton x False) (M.singleton x Hand)
        buildPost x = Condition Empty (M.singleton x True) (M.singleton x Table)
buildAction cost aType@(Stack x y) = Action aType (buildPre x y) (buildPost x y) cost
  where buildPre x y  = Condition (Has x) (M.fromList [(x,False),(y,True)]) (M.singleton x Hand)
        buildPost x y = Condition Empty (M.fromList [(x,True),(y,False)]) (M.singleton x (Object y))
buildAction cost aType@(Unstack x y) = Action aType (buildPre x y) (buildPost x y) cost
  where buildPre x y  = Condition Empty (M.fromList [(x,True),(y,False)]) (M.singleton x (Object y))
        buildPost x y = Condition (Has x) (M.fromList [(x,False),(y,True)]) (M.singleton x Hand)

strips :: Domain -> Condition -> Condition -> NodeInfo
strips domain start goal = undefined

searchPlan = undefined
