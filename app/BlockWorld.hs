module Main where

import Data.List ((\\), null, union, sortBy)
import Data.Function (on)
import qualified Data.Map as M

data Block = A | B  deriving (Show, Eq, Ord, Enum)
data Object = Table | Object Block deriving (Eq, Show)

data Term = HandEmpty
          | HandHas Block
          | IsTop Block Bool
          | On Block Object
          deriving (Eq, Show)

type Condition = [Term]

data ActionType = Pickup Block
                | Putdown Block
                | Stack Block Block
                | Unstack Block Block
                deriving Show

data Action = NoAction
            | Action {
  actionType    :: ActionType
, preCondition  :: Condition
, postCondition :: Condition
, actionCost    :: Int
} deriving Show

type Plan = [ActionType]
type Domain = [Action]

data NodeInfo = NoNodeInfo
              | NodeInfo { condition :: Condition
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
searchPlan domain start goal = searchNext domain goal [goalNodeInfo] [] 
  where
    (estimateCost, conditionDiff) = getConditionDiff start goal
    goalNodeInfo = NodeInfo goal NoAction NoNodeInfo 0 estimateCost conditionDiff estimateCost

    searchNext :: Domain -> Condition -> [NodeInfo] -> [NodeInfo] -> NodeInfo
    searchNext _ [] _ _ = NoNodeInfo
    searchNext domain goal openList@(nodeInfo:rest) closeList
      | diffCount nodeInfo == 0 = nodeInfo
      | otherwise = searchNext domain goal (buildOpenList openList closeList) (nodeInfo:closeList)

    buildOpenList :: [NodeInfo] -> [NodeInfo] -> [NodeInfo] 
    buildOpenList (nodeInfo:rest) closeList = sortBy (compare `on` score) $ mergeNodes rest closeList $ getNextNodes nodeInfo 

    mergeNodes :: [NodeInfo] -> [NodeInfo] -> [NodeInfo] -> [NodeInfo]
    mergeNodes openList closeList newNodes = M.elems $ M.unionWith replaceByCondition openMap $ newNodeMap M.\\ closeMap
      where openMap    = M.fromList $ map toTuple openList
            closeMap   = M.fromList $ map toTuple closeList
            newNodeMap = M.fromList $ map toTuple newNodes
            toTuple nodeInfo = (sort $ condition nodeInfo, nodeInfo)
            replaceByCondition old new = if score old < score new then old else new

    getNextNodes :: NodeInfo -> [NodeInfo]
    getNextNodes nodeInfo = map (buildNodeInfo nodeInfo) $ filter include domain
      where include action = null $ postCondition action \\ condition nodeInfo

    buildNodeInfo :: NodeInfo -> Action -> NodeInfo
    buildNodeInfo nodeInfo action = NodeInfo newCondition action nodeInfo score rCost diff eCost
      where newCondition = snd $ getConditionDiff (condition nodeInfo) (postCondition action) `union` preCondition action
            (eCost, diff) = getConditionDiff newCondition start
            rCost = realCost nodeInfo + actionCost action
            score = rCost + eCost
            

getConditionDiff :: Condition -> Condition -> (Int, Condition)
getConditionDiff cond1 cond2 = let diff = cond2 \\ cond1 in (length diff, diff)





