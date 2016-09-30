module Main where

import BlockWorldLib

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

