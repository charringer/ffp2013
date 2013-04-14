module AufgabeFFP3 where

-- part 1

type Weight      = Int
type Value       = Int
type Item        = (Weight,Value)
type Items       = [Item]
type Load        = [Item]
type Loads       = [Load]
type LoadWghtVal = (Load,Weight,Value)
type MaxWeight   = Weight

generator   :: Items -> Loads
transformer :: Loads -> [LoadWghtVal]
filter      :: MaxWeight -> [LoadWghtVal] -> [LoadWghtVal]
selector    :: [LoadWghtVal] -> [LoadWghtVal]

generator _ = []
transformer _ = []
filter _ _ = []
selector _ = []

selector1 :: [LoadWghtVal] -> [LoadWghtVal]
selector1 _ = []

selector2 :: [LoadWghtVal] -> [LoadWghtVal]
selector2 _ = []

-- part 2
binom :: (Integer,Integer) -> Integer
binom (n,k)
  | k==0 || n==k = 1
  | otherwise    = binom (n-1,k-1) + binom (n-1,k)

-- part 2.a by stream programming
binomS :: (Integer,Integer) -> Integer
binomS _ = 0

-- part 2.b by memoization
binomM :: (Integer,Integer) -> Integer
binomM _ = 0



-- vim:sts=2:sw=2
