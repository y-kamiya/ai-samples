module Main where

import BlockWorldLib

main :: IO ()
main = do
  let startCondition = [HandEmpty, IsTop A True, IsTop B False, On A (Object B), On B Table]
  let goalCondition  = [HandEmpty, IsTop A False, IsTop B True, On B (Object A), On A Table]
  mapM_ print buildDomain
  -- let plan = strips buildDomain startCondition goalCondition    
  print "------------- plan ----------------"

  let (estimateCost, conditionDiff) = getConditionDiff startCondition goalCondition
      goalNodeInfo = NodeInfo goalCondition NoAction NoNodeInfo 0 estimateCost conditionDiff estimateCost
      (nodeInfo:rest) = getNextNodes buildDomain startCondition goalNodeInfo 
      (nodeInfo2:_) = getNextNodes buildDomain startCondition nodeInfo 
      a@(nodeInfo3:_) = getNextNodes buildDomain startCondition nodeInfo2
      (nodeInfo4:_) = getNextNodes buildDomain startCondition nodeInfo3
      all@(nodeInfo5:[]) = getNextNodes buildDomain startCondition nodeInfo4
  mapM_ print $ extractPlan [] nodeInfo5
  mapM_ print a
  -- mapM_ print all
  return ()

