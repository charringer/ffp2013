{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
module AufgabeFFP6 where
import Data.Array

eval :: Array Int Int -> Array Int (Int -> Int -> Int) -> Int
eval nums ops = eval' nums ops (snd (bounds nums))

eval' nums ops n
  | n == fst (bounds nums) = nums ! n
  | otherwise = (ops ! (n-1)) (eval' nums ops (n-1)) (nums ! n)

yield :: Array Int Int -> Int -> [Array Int (Int -> Int -> Int)]
yield = yield_gtf

yield_bt :: Array Int Int -> Int -> [Array Int (Int -> Int -> Int)]
yield_bt = yield_gtf -- FIXME!!!

yield_gtf :: Array Int Int -> Int -> [Array Int (Int -> Int -> Int)]
yield_gtf nums res = filt res $ transform nums $ generate nums

generate nums = do
  ops <- mapM (const [(+), (-), (*), div]) (indices nums)
  return $ array (bounds nums) (zip (indices nums) ops)

transform nums = map (\ops -> (ops, eval nums ops))

filt res = map fst . filter ((res ==) . snd)

instance Show (Array Int (Int->Int->Int)) where
  show = undefined
