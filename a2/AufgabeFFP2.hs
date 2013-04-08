module AufgabeFFP2 where

-- part 1: prime pairs
primes :: [Integer]
primes = sieve [2..]

sieve :: [Integer] -> [Integer]
sieve (x:xs) = x : sieve [y | y <- xs, mod y x > 0]

doubleprimes :: [Integer]
doubleprimes = [primes!!n | n <- [0..], (primes!!n) + 2 == primes!!(n+1)]

pps :: [(Integer,Integer)]
pps = map makepair doubleprimes
	where makepair n = (n, n+2)

-- part 2: pow
pow :: Int -> Integer
pow 0 = 1
pow n = pow (n-1) + pow(n-1)

powFast :: Int -> Integer
powFast n = powMemo!!n

powMemo :: [Integer]
powMemo = [powFromMemo n | n <- [0..]]
	where powFromMemo m
		| m == 0    = 1
		| otherwise = powMemo!!(m-1) + powMemo!!(m-1)

-- part 3: exp
f :: Int -> Int -> Float
f _ _ = 1.0

fMT :: Int -> Int -> Float
fMT _ _ = 1.0

-- part 4: Gödel numbers
-- professional casting: int -> string -> char -> string -> int
-- also gz 1 = gz 10, etc. would make more sense if digits were reversed...
gz :: Integer -> Integer
gz n
	| n < 0     = 0
	| otherwise = foldr (*) 1 (zipWith (^) primes (map charToInt (show n)))
		where charToInt c = read [c] :: Int
	
gzs :: [Integer]
gzs = map gz [1..]

-- vim:sts=2:sw=2
