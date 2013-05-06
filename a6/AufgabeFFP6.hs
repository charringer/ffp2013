{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
module AufgabeFFP6 where
import Data.Array

eval :: Array Int Int -> Array Int (Int -> Int -> Int) -> Int
eval = undefined

yield :: Array Int Int -> Int -> [Array Int (Int -> Int -> Int)]
yield = yield_gtf

yield_bt :: Array Int Int -> Int -> [Array Int (Int -> Int -> Int)]
yield_bt = undefined

yield_gtf :: Array Int Int -> Int -> [Array Int (Int -> Int -> Int)]
yield_gtf = undefined

instance Show (Array Int (Int->Int->Int)) where
  show = undefined
