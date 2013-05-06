{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
module AufgabeFFP6 where
import Data.Array
import Data.List (intersperse)
import Data.List
import Data.Array
import Numeric
import Data.Char

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
		  
---

eval :: Array Int Int -> Array Int (Int -> Int -> Int) -> Int
eval nums ops = eval' nums ops (snd (bounds nums))

eval' nums ops n
  | n == fst (bounds nums) = nums ! n
  | otherwise = (ops ! (n-1)) (eval' nums ops (n-1)) (nums ! n)

yield :: Array Int Int -> Int -> [Array Int (Int -> Int -> Int)]
yield = yield_gtf

yield_bt :: Array Int Int -> Int -> [Array Int (Int -> Int -> Int)]
yield_bt nums goal = map makeFuncArray (searchDfs succYield goalYield (-1,nums,goal))

zeros = '0' : zeros

baseFourString n = showIntAtBase 4 intToDigit n ""

makeNumberString n l = (take (l-(length s)) zeros) ++ s
	where s = baseFourString n

numberToFunc c
	| c == '0' = (+)
	| c == '1' = (-)
	| c == '2' = (*)
	| c == '3' = div
	
succYield (n,nums, goal)
	| n == -1 = [(n, nums, goal) | n<-[0..4^(h-l)-1]]
	| otherwise =[]
	where (l,h) = bounds nums

a = (array (1,3) [(1,1),(2,2),(3,3)])

makeFuncArray (n,nums, _) = (array (l,h-1)(zip (indices nums) (map numberToFunc (makeNumberString n (h-l)))))
	where (l,h) = bounds nums

goalYield (n,nums,goal)
	| n == -1 = False
	| otherwise = goal == eval nums (makeFuncArray (n,nums,goal))
	
yield_gtf :: Array Int Int -> Int -> [Array Int (Int -> Int -> Int)]
yield_gtf nums res = filt res $ transform nums $ generate nums

generate nums = do
  ops <- mapM (const [(+), (-), (*), div]) (init (indices nums))
  let (l,h) = bounds nums
  return $ array (l,h-1) (zip (indices nums) ops)

transform nums = map (\ops -> (ops, eval nums ops))

filt res = map fst . filter ((res ==) . snd)

showFun f
  | 1 `f` 2 ==  3 = "plus"
  | 1 `f` 2 == -1 = "minus"
  | 1 `f` 2 ==  2 = "times"
  | 1 `f` 2 ==  0 = "div"

-- hugs' Data.List is so ancient that it doesn't include intercalate
intercalate xs xss = concat (intersperse xs xss)

instance Show (Array Int (Int->Int->Int)) where
  show = intercalate "-" . map showFun . elems
  

