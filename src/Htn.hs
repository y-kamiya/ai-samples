module Htn where

{-
data PrimitiveTask = Stack Block Block
                   | UnStack Block Block
                   | Putdown Block
                   | Pickup Block

data CompoundTask = Move Block Object
                  | Clear Block
                  | Get Block
                  | Put Block

data Task = Primitive PrimitiveTask
          | Compound CompoundTask
          | Invalid

data Domain = Domain {
                 primitiveMap :: M.Map PrimitiveTask [(Condition, Condition)]
               , compoundMap :: M.Map CompoundTask [(Condition, [Task])]
               }

htn :: Domain -> Condition -> [Task] -> [Task]
htn domain condition tasks = htn' domain condition tasks []

htn' :: Domain -> Condition -> [Task] -> [Task] -> [Task]
htn' _ _ [] plan = plan
htn' domain [] _ plan = plan ++ Invalid
htn' domain condition (task@(Invalid):tasks) plan = plan ++ task
htn' domain condition (task@(Primitive):tasks) plan = let newCondition = execute domain condition task
                                                      in  htn' domain newCondition tasks $ plan ++ task
htn' domain condition (task@(Compound):tasks) plan = let newTasks = breakdown domain condition task
                                                     in  htn' domain condition (newTasks ++ tasks) plan
  

breakdown :: Doamin -> Condition -> CompoundTask -> [Task]
breakdown domain condition task = case M.lookup task (compoundMap domain) of
                                     Nothing -> [Invalid]
                                     Just list -> case find (\(pre, _) -> include condition pre) list of
                                                    Just (_, tasks) -> tasks
                                                    Nothing -> [Invalid]
-- Conditionが存在しない場合が必要＝otherwise
-- Matchするconditionを探してそのpost conditionを返す

  | satisfy condition [Hand a, IsTop b True]  = [Put a b]
  | satisfy condition [Hand a]                = [Clear b, Put a b]
  | satisfy condition [IsTop a False]         = [Clear a, Move a b]
  | satisfy condition [IsTop b False] && b /= Table = [Clear b, Move a b]
  | otherwise                                 = [Get a, Put a b]

execute :: Doamin -> Condition -> PrimitiveTask -> Condition
execute domain condition task = case M.lookup task (primitiveMap domain) of
                                  Nothing -> []
                                  Just list -> case find (\(pre, _) -> include condition pre) list of
                                                  Just (_, condition) -> condition
                                                  Nothing -> []




-}
