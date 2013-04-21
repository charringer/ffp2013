module AufgabeFFP4 where
import Control.Monad
import Data.List
import Data.Array

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
succKnp (v, w, limit, objects, psol) = do
  nextObject@(nextW, nextV) <- objects
  when (w + nextW > limit) $ fail "too much weight"
  return (v + nextV, w + nextW, limit, delete nextObject objects, nextObject : psol)

goalKnp :: NodeKnp -> Bool
goalKnp (_, w, limit, _, _) = w == limit

knapsack :: Objects -> MaxWeight -> (SolKnp, Value)
knapsack objects limit = (psol, v)
  where (v, _, _, _, psol):_ = searchDfs succKnp goalKnp (0, 0, limit, objects, [])

-- Table {{{1

newtype Table a b = Tbl (Array a b)

newTable :: Ix a => (a,a) -> [(a,b)] -> Table a b
newTable bnds l = Tbl (array bnds l)

findTable :: Ix a => Table a b -> a -> b
findTable (Tbl a) i = a ! i

updTable :: Ix a => (a,b) -> Table a b -> Table a b
updTable p@(i,x) (Tbl a) = Tbl (a // [p])

-- Dynamic programming {{{1

dynamic :: Ix coord => (Table coord entry -> coord -> entry)
  -> (coord, coord) -> Table coord entry
dynamic compute bnds = t
  where t = newTable bnds (map (\coord -> (coord, compute t coord)) (range bnds))

-- Binomial coefficients {{{1

binomDyn :: (Integer, Integer) -> Integer
binomDyn (n,k) = findTable (dynamic compute ((0,0), (bnd,bnd))) (n,k)
  where bnd = max n k

	compute tbl (m,l)
	  | l == 0 = 1
	  | l == m = 1
	  | l >  m = 0
	  | otherwise = findTable tbl (m-1,l-1) + findTable tbl (m-1,l)

-- vim:sts=2 sw=2 fdm=marker:
