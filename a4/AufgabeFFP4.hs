module AufgabeFFP4 where

-- Stack {{{1

data Stack a = EmptyStk | Stk a (Stack a)

push :: a -> Stack a -> Stack a
push x s = Stk x s

pop :: Stack a -> Stack a
pop (Stk _ s) = s
pop EmptyStk = error "pop from an empty stack"

top :: Stack a -> a
top (Stk x _) = x
top EmptyStk = error "top from an empty stack"

emptyStack :: Stack a
emptyStack = EmptyStk

stackEmpty :: Stack a -> Bool
stackEmpty (Stk _ _) = False
stackEmpty EmptyStk = True

-- DFS {{{1

searchDfs :: (Eq node) => (node -> [node]) -> (node -> Bool) -> node -> [node]
searchDfs succ goal x = search' (push x emptyStack)
  where
    search' s | stackEmpty s = []
              | goal (top s) = top s : search' (pop s)
	      | otherwise = let x = top s in search' (foldr push (pop s) (succ x))

-- Knapsack Problem {{{1

type Weight = Int
type Value = Int
type MaxWeight = Weight

type Object = (Weight, Value)
type Objects = [Object]
type SolKnp = [Object]

type NodeKnp = (Value, Weight, MaxWeight, [Object], SolKnp)

succKnp :: NodeKnp -> [NodeKnp]
succKnp = undefined

goalKnp :: NodeKnp -> Bool
goalKnp = undefined

knapsack :: Objects -> MaxWeight -> (SolKnp, Value)
knapsack = undefined

-- Binomial coefficients {{{1

binomDyn :: (Integer, Integer) -> Integer
binomDyn = undefined

-- vim:sts=2 sw=2 fdm=marker:
