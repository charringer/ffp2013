module AufgabeFFP2 where

-- part 1: prime pairs
pps :: [(Integer,Integer)]
pps = (1,1) : pps

-- part 2: pow
powFast :: Int -> Integer
powFast 0 = 1
powFast n = 1

-- part 3: exp
f :: Int -> Int -> Float
f _ _ = 1.0

fMT :: Int -> Int -> Float
fMT _ _ = 1.0

-- part 4: Gödel numbers
gz :: Integer -> Integer
gz _ = 1

gzs :: [Integer]
gzs =  1 : gzs

-- vim:sts=2:sw=2
