{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE RankNTypes #-}
module Htn where

import qualified Data.Map as M
import Data.List (find, null, (\\))

class (Eq a, Ord a, Show a) => Term a
class (Eq a, Ord a, Show a) => PrimitiveTask a
class (Eq a, Ord a, Show a) => CompoundTask a

data Task a b = Primitive a
              | Compound b
              | Invalid String
              deriving Show

data Domain a b c = Domain {
                      primitiveMap :: M.Map a [([c], [c])]
                    , compoundMap :: M.Map b [([c], [Task a b])]
                    }

htn :: (PrimitiveTask a, CompoundTask b, Term c) => Domain a b c -> [c] -> [Task a b] -> ([Task a b], [c])
htn domain condition tasks = htn' domain condition tasks []

htn' :: (PrimitiveTask a, CompoundTask b, Term c) => Domain a b c -> [c] -> [Task a b] -> [Task a b] -> ([Task a b], [c])
htn' _ cond [] plan = (plan, cond)
htn' domain [] _ plan = (plan ++ [Invalid "no condition"], [])
htn' domain condition (task@(Invalid _):tasks) plan = (plan ++ [task, Invalid $ "current condition: " ++ show condition], condition)
htn' domain condition (task@(Primitive pTask):tasks) plan = let newCondition = execute domain condition pTask
                                                      in  htn' domain newCondition tasks $ plan ++ [task]
htn' domain condition (task@(Compound cTask):tasks) plan = let newTasks = breakdown domain condition cTask
                                                     in  htn' domain condition (newTasks ++ tasks) plan
  
include :: (Ord a) => [a] -> [a] -> Bool
include cond1 cond2 = null $ cond2 \\ cond1

breakdown :: (PrimitiveTask a, CompoundTask b, Term c) => Domain a b c -> [c] -> b -> [Task a b]
breakdown domain condition task = case M.lookup task (compoundMap domain) of
                                     Nothing -> [Invalid $ "definition is not found for " ++ show task]
                                     Just list -> case find (\(pre, _) -> include condition pre) list of
                                                    Just (_, tasks) -> tasks
                                                    Nothing -> [Invalid $ "no condition is matched, current: " ++ show condition ++ ", task: " ++ show task]

execute :: (PrimitiveTask a, Term c) => Domain a b c -> [c] -> a -> [c]
execute domain condition task = case M.lookup task (primitiveMap domain) of
                                  Nothing -> []
                                  Just list -> case find (\(pre, _) -> include condition pre) list of
                                                  Just (pre, post) -> (condition \\ pre) ++ post
                                                  Nothing -> []

