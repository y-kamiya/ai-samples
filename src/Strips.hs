module Strips where

import Data.List ((\\), null, union, sort, sortBy)
import Data.Function (on)
import qualified Data.Map as M

class ActionType a

class Term a

data Action a b = NoAction
                | Action {
                    actionType    :: a
                  , preCondition  :: [b]
                  , postCondition :: [b]
                  , actionCost    :: Int
                  } deriving (Eq, Show)

data NodeInfo a b = NoNodeInfo
                  | NodeInfo {
                      realCost  :: Int
                    , score     :: Int
                    , diff      :: [b]
                    , diffCount :: Int
                    , condition :: [b]
                    , action    :: Action a b
                    , next      :: NodeInfo a b
                    } deriving (Eq, Show)


strips :: [ Action a b] -> [b] -> [b] -> [a]
strips domain start goal = extractPlan [] $ searchPlan domain start goal

extractPlan :: [a] -> NodeInfo a b -> [a]
extractPlan plan NodeInfo { action = NoAction } = reverse plan
extractPlan plan NodeInfo { action = Action { actionType = actionType}, next = next } = extractPlan (actionType:plan) next

searchPlan :: [Action a b] -> [b] -> [b] -> NodeInfo a b
searchPlan domain start goal = searchNext [goalNodeInfo] []
  where
    (estimateCost, conditionDiff) = getConditionDiff start goal
    goalNodeInfo = NodeInfo 0 estimateCost conditionDiff estimateCost goal NoAction NoNodeInfo

    searchNext :: [NodeInfo a b] -> [NodeInfo a b] -> NodeInfo a b
    searchNext [] _ = NoNodeInfo
    searchNext openList@(nodeInfo:rest) closeList
      | diffCount nodeInfo == 0 = nodeInfo
      | otherwise = searchNext (buildOpenList openList closeList) (nodeInfo:closeList)

    buildOpenList :: [NodeInfo a b] -> [NodeInfo a b] -> [NodeInfo a b]
    buildOpenList (nodeInfo:rest) closeList = sortBy (compare `on` score) $ mergeNodes rest closeList $ getNextNodes nodeInfo

    -- getNextNodes :: NodeInfo a b -> [NodeInfo a b]
    getNextNodes nodeInfo = map (buildNodeInfo nodeInfo) $ getActionCandidates domain nodeInfo

    buildNodeInfo :: NodeInfo a b -> Action a b -> NodeInfo a b
    buildNodeInfo nodeInfo action = NodeInfo score rCost diff eCost newCondition action nodeInfo
      where newCondition = (snd $ getConditionDiff (condition nodeInfo) (postCondition action)) `union` preCondition action
            (eCost, diff) = getConditionDiff newCondition start
            rCost = realCost nodeInfo + actionCost action
            score = rCost + eCost

getConditionDiff :: [b] -> [b] -> (Int, [b])
getConditionDiff dest src = let diff = dest \\ src in (length diff, diff)

mergeNodes :: [NodeInfo a b] -> [NodeInfo a b] -> [NodeInfo a b] -> [NodeInfo a b]
mergeNodes openList closeList newNodes = M.elems $ M.unionWith replaceByCondition openMap $ newNodeMap M.\\ closeMap
  where openMap    = M.fromList $ map toTuple openList
        closeMap   = M.fromList $ map toTuple closeList
        newNodeMap = M.fromList $ map toTuple newNodes
        toTuple nodeInfo = (sort $ condition nodeInfo, nodeInfo)
        replaceByCondition old new = if score old < score new then old else new

getActionCandidates :: [Action a b] -> NodeInfo a b -> [Action a b]
getActionCandidates domain nodeInfo = filter include domain
  where include action = null $ postCondition action \\ condition nodeInfo



