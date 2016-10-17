module Htn where

data PrimitiveTask = Stack Block Block Condition Condition
                   | UnStack Block Block Condition Condition
                   | Putdown Block Condition Condition
                   | Pickup Block Condition Condition

data CompoundTask = Move Block Object
                  | Clear Block
                  | Get Block
                  | Put Block

data Task = Primitive PrimitiveTask
          | Compound CompoundTask

type Domain = [Task]

htn :: Domain -> Condition -> [Task] -> [Task]
htn domain condition tasks = tasks

htn' :: Domain -> Condition -> [Task] -> [Task] -> [Task]
htn' _ _ [] plan = plan
htn' domain [] _ plan = plan ++ Invalid
htn' domain condition (task@(Invalid):tasks) plan = plan ++ task
htn' domain condition (task@(Primitive):tasks) plan = let newCondition = execute condition task
                                                      in  htn' domain newCondition tasks $ plan ++ task
htn' domain condition (task@(Compound):tasks) plan = let newTasks = breakdown condition task
                                                     in  htn' domain condition (newTasks ++ tasks) plan
  

breakdown :: Condition -> CompoundTask -> [Task]
breakdown condition (Move a b)
  | satisfy condition [Hand a, IsTop b True]  = [Put a b]
  | satisfy condition [Hand a]                = [Clear b, Put a b]
  | satisfy condition [IsTop a False]         = [Clear a, Move a b]
  | satisfy condition [IsTop b False] && b /= Table = [Clear b, Move a b]
  | otherwise                                 = [Get a, Put a b]

execute :: Condition -> PrimitiveTask -> Condition
execute condition (Stack x y pre post) = if satisfy pre condition
                                           then post
                                           else []





