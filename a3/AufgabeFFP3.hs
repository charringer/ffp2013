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

-- the last element of the powerset is the empty set which is apparently not a valid
-- choice of elements judging from the example outputs
generator :: Items -> Loads
generator x = droplast (generator_ x [])
	where droplast y = (reverse.tail.reverse) y

generator_ [] included_items = [included_items]
generator_ (x:xs) included_items = (generator_ xs (included_items ++ [x])) ++ (generator_ xs included_items)

transformer :: Loads -> [LoadWghtVal]
transformer x = map trans_sum x
	where trans_sum y = (y, sum (map fst y), sum (map snd y))
	
filter :: MaxWeight -> [LoadWghtVal] -> [LoadWghtVal]
filter n x = [y | y<-x, (getWeight y) <= n]

getWeight (a,b,c) = b
getValue (a,b,c) = c
	
-- get the loads with the biggest value
selector1 :: [LoadWghtVal] -> [LoadWghtVal]
selector1 [] = []
selector1 (x:xs) = selector1_ xs [x] (getValue x)

selector1_ [] best_loads _ = best_loads
selector1_ (x:xs) best_loads best_value
	| value > best_value  = selector1_ xs [x] value
	| value == best_value = selector1_ xs (x:best_loads) best_value
	| otherwise           = selector1_ xs best_loads best_value
		where value = getValue x
	
-- get the loads with the smalles weight	
selector3 :: [LoadWghtVal] -> [LoadWghtVal]
selector3 [] = []
selector3 (x:xs) = selector3_ xs [x] (getWeight x)

selector3_ [] best_loads _ = best_loads
selector3_ (x:xs) best_loads best_value
	| value < best_value  = selector3_ xs [x] value
	| value == best_value = selector3_ xs (x:best_loads) best_value
	| otherwise           = selector3_ xs best_loads best_value
		where value = getWeight x
		
selector2 :: [LoadWghtVal] -> [LoadWghtVal]
selector2 = (selector3.selector1)

-- part 2
binom :: (Integer,Integer) -> Integer
binom (n,k)
  | k==0 || n==k = 1
  | otherwise    = binom (n-1,k-1) + binom (n-1,k)

-- part 2.a by stream programming
pd :: [[Integer]]
pd = [1] : map nextRow pd
	where nextRow r = zipWith (+) (0:r) (r ++ [0])

binomS :: (Integer,Integer) -> Integer
binomS (n,k) = pd!!n_!!k_
	where
		n_ = fromIntegral n
		k_ = fromIntegral k
		
-- part 2.b by memoization
binomM :: (Integer,Integer) -> Integer
binomM (n,k) = binomTable!!n_!!k_
	where
		n_ = fromIntegral n
		k_ = fromIntegral k
		
binomTable = [ [binomFromTable (n,k) | k<-[0..n]] | n<-[0..]]
	where
		binomFromTable (n,k)
			| k==0 || n==k = 1
			| otherwise = binomTable!!(n-1)!!(k-1) + binomTable!!(n-1)!!(k) 

-- vim:sts=2:sw=2
