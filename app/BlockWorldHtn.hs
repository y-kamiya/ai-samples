module Main where

-- import Htn
import qualified Data.Map as M
import Data.List (find, null, (\\))

data Block = A | B | C deriving (Show, Eq, Ord, Enum)
data Object = Table | Object Block deriving (Eq, Ord, Show)

data BWTerm = HandEmpty
            | HandHas Block
            | IsTop Block Bool
            | On Block Object
            deriving (Eq, Ord, Show)

type Condition = [BWTerm]

data PrimitiveTask = Stack Block Block
                   | Unstack Block Block
                   | Putdown Block
                   | Pickup Block
                   deriving (Eq, Ord, Show)

data CompoundTask = Move Block Object
                  | Clear Block
                  | Get Block
                  | Put Block Object
                   deriving (Eq, Ord, Show)

data Task = Primitive PrimitiveTask
          | Compound CompoundTask
          | Invalid String
          deriving Show

data Domain = Domain {
                primitiveMap :: M.Map PrimitiveTask [(Condition, Condition)]
              , compoundMap :: M.Map CompoundTask [(Condition, [Task])]
              }

instance Show Domain where
  show (Domain p c) = toStr p ++ toStr c
    where toStr :: (Show a, Show b) => M.Map a [b] -> String
          toStr = M.foldlWithKey (\str task list -> str ++ "-- " ++ show task ++ "\n" ++ unlines (map show list)) ""
                      

htn :: Domain -> Condition -> [Task] -> ([Task], Condition)
htn domain condition tasks = htn' domain condition tasks []

htn' :: Domain -> Condition -> [Task] -> [Task] -> ([Task], Condition)
htn' _ cond [] plan = (plan, cond)
htn' domain [] _ plan = (plan ++ [Invalid "no condition"], [])
htn' domain condition (task@(Invalid _):tasks) plan = (plan ++ [task, Invalid $ "current condition: " ++ show condition], condition)
htn' domain condition (task@(Primitive pTask):tasks) plan = let newCondition = execute domain condition pTask
                                                      in  htn' domain newCondition tasks $ plan ++ [task]
htn' domain condition (task@(Compound cTask):tasks) plan = let newTasks = breakdown domain condition cTask
                                                     in  htn' domain condition (newTasks ++ tasks) plan
  
include :: (Ord a) => [a] -> [a] -> Bool
include cond1 cond2 = null $ cond2 \\ cond1

breakdown :: Domain -> Condition -> CompoundTask -> [Task]
breakdown domain condition task = case M.lookup task (compoundMap domain) of
                                     Nothing -> [Invalid $ "definition is not found for " ++ show task]
                                     Just list -> case find (\(pre, _) -> include condition pre) list of
                                                    Just (_, tasks) -> tasks
                                                    Nothing -> [Invalid $ "no condition is matched, current: " ++ show condition ++ ", task: " ++ show task]

execute :: Domain -> Condition -> PrimitiveTask -> Condition
execute domain condition task = case M.lookup task (primitiveMap domain) of
                                  Nothing -> []
                                  Just list -> case find (\(pre, _) -> include condition pre) list of
                                                  Just (pre, post) -> (condition \\ pre) ++ post
                                                  Nothing -> []

main :: IO ()
main = do
  let startCondition = [HandEmpty, IsTop A True, IsTop B False, IsTop C False, On A (Object B), On B (Object C), On C Table]
  let goalCondition  = [HandEmpty, IsTop A False, IsTop B False, IsTop C True, On C (Object B), On B (Object A), On A Table]
  print "------------- domain ----------------"
  print buildDomain
  print "------------- target ----------------"
  print $ "start: " ++ show startCondition
  print $ "goal: "  ++ show goalCondition
  print "------------- plan ----------------"
  let (tasks, cond) = htn buildDomain startCondition [Compound $ Move A Table, Compound $ Move B (Object A), Compound $ Move C (Object B)]
  mapM_ print tasks
  print cond
  return ()

buildDomain :: Domain
buildDomain = Domain buildPrimitiveMap buildCompoundMap
  where
    buildPrimitiveMap = M.fromList $ stacks ++ unstacks ++ putdowns ++ pickups
    pickups  = map (buildDomainPrimitive . Pickup) [A ..]
    putdowns = map (buildDomainPrimitive . Putdown) [A ..]
    stacks   = map (buildDomainPrimitive . uncurry Stack) perms
    unstacks = map (buildDomainPrimitive . uncurry Unstack) perms
    perms = [(x, y) | x <- [A ..], y <- [A ..], x /= y]

    buildCompoundMap = M.fromList $ moves ++ clears ++ gets ++ puts
    clears = map (buildDomainCompound . Clear) [A ..]
    gets   = map (buildDomainCompound . Get) [A ..]
    puts   = map (buildDomainCompound . uncurry Put) permsObj
    moves  = map (buildDomainCompound . uncurry Move) permsObj
    permsObj = [(x, y) | x <- [A ..], y <- Table:map Object [A ..]]

buildDomainPrimitive :: PrimitiveTask -> (PrimitiveTask, [(Condition, Condition)])
buildDomainPrimitive task@(Pickup x) = (task, [(pre, post)])
  where pre  = [HandEmpty, IsTop x True, On x Table]
        post = [HandHas x, IsTop x False]
buildDomainPrimitive task@(Putdown x) = (task, [(pre, post)])
  where pre  = [HandHas x, IsTop x False]
        post = [HandEmpty, IsTop x True, On x Table]
buildDomainPrimitive task@(Stack x y) = (task, [(pre, post)])
  where pre  = [HandHas x, IsTop x False, IsTop y True]
        post = [HandEmpty, IsTop x True, IsTop y False, On x (Object y)]
buildDomainPrimitive task@(Unstack x y) = (task, [(pre, post)])
  where pre  = [HandEmpty, IsTop x True, IsTop y False, On x (Object y)]
        post = [HandHas x, IsTop x False, IsTop y True]

buildDomainCompound :: CompoundTask -> (CompoundTask, [(Condition, [Task])])
buildDomainCompound task@(Move x Table) = (task, [ ([], [Compound (Get x), Compound (Put x Table)]) ])
buildDomainCompound task@(Move x (Object y)) = (task, 
  [ ([HandHas x, IsTop y True] , [Compound (Put x (Object y))])
  , ([HandHas x]               , [Compound (Clear y), Compound (Put x (Object y))])
  , ([IsTop x False]           , [Compound (Clear x), Compound (Move x (Object y))])
  , ([IsTop y False]           , [Compound (Clear y), Compound (Move x (Object y))])
  , ([]                        , [Compound (Get x  ), Compound (Put x (Object y))])
  ])
buildDomainCompound task@(Clear x) = (task, map clear [A ..] ++ [([], [])])
  where clear a = ([IsTop x False, On a (Object x)], [Compound (Move a Table)])
buildDomainCompound task@(Get x) = (task, map get [A ..] ++
  [ ([HandEmpty, IsTop x True, On x Table], [Primitive (Pickup x)])
  , ([HandEmpty, IsTop x False]           , [Compound (Clear x), Compound (Get x)])
  , ([]         , [Invalid $ "cant breakdown " ++ show task])
  ])
  where get a = ([HandEmpty, IsTop x True, On x (Object a)], [Primitive (Unstack x a)])
buildDomainCompound task@(Put x Table) = (task,
  [ ([HandHas x], [Primitive (Putdown x)])
  , ([]         , [Invalid $ "cant breakdown " ++ show task])
  ])
buildDomainCompound task@(Put x (Object y)) = (task,
  [ ([HandHas x, IsTop y True], [Primitive (Stack x y)])
  , ([]                       , [Invalid $ "cant breakdown " ++ show task])
  ])
