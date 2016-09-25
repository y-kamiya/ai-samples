module Main where

import qualified Data.Map as M

data Block = A | B  deriving (Show, Eq, Ord, Enum)
data Object = Table | Object Block deriving Show

data Term = HandEmpty
          | HandHas Block
          | IsTop Block Bool
          | On Block Object
          deriving Show

type Condition = [Term]

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

type Plan = [ActionType]
type Domain = [Action]

data NodeInfo = NodeInfo { condition :: Condition
                         , action    :: Action
                         , next      :: NodeInfo
                         , realCost  :: Int
                         , score     :: Int
                         , diff      :: Condition
                         , diffCount :: Int
                         } deriving Show

main :: IO ()
main = do
  let startCondition = [HandEmpty, IsTop A True, IsTop B False, On A (Object B), On B Table]
  let goalCondition  = [HandEmpty, IsTop A False, IsTop B True, On B (Object A), On A Table]
  mapM_ print buildDomain
  -- let plan = strips buildDomain startCondition goalCondition    
  -- return ()


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
        buildPost x = [HandHas x, IsTop x True]
buildAction cost aType@(Putdown x) = Action aType (buildPre x) (buildPost x) cost
  where buildPre x  = [HandHas x, IsTop x False]
        buildPost x = [HandEmpty, IsTop x True, On x Table]
buildAction cost aType@(Stack x y) = Action aType (buildPre x y) (buildPost x y) cost
  where buildPre x y  = [HandHas x, IsTop x False, IsTop y True]
        buildPost x y = [HandEmpty, IsTop x True, IsTop y False, On x (Object y)]
buildAction cost aType@(Unstack x y) = Action aType (buildPre x y) (buildPost x y) cost
  where buildPre x y  = [HandEmpty, IsTop x True, IsTop y False, On x (Object y)]
        buildPost x y = [HandHas x, IsTop x False, IsTop y True]

strips :: Domain -> Condition -> Condition -> Plan
strips domain start goal = extractPlan [] $ searchPlan domain start goal
  where
    extractPlan :: Plan -> NodeInfo -> Plan
    extractPlan plan nodeInfo
      | realCost nodeInfo == 0 = newPlan
      | otherwise = extractPlan newPlan $ next nodeInfo
      where
        newPlan = (actionType $ action nodeInfo) : plan


searchPlan :: Domain -> Condition -> Condition -> NodeInfo
searchPlan domain start goal = searchNext domain goal [] [] 
  where
    -- buildGoalNodeIfo = NodeInfo goal NoAction NoNodeInfo 0 score diff diffCount

    searchNext :: Domain -> Condition -> [NodeInfo] -> [NodeInfo] -> NodeInfo
    -- searchNext [] _ _ = NoNodeInfo
    searchNext domain (nodeInfo:rest) closeList = undefined
