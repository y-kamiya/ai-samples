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
      all@(nodeInfo3:_) = getNextNodes buildDomain startCondition nodeInfo2
  mapM_ print all
  return ()

