pof2s :: [Integer]
pof2s = 1 : map (2*) pof2s

pd :: [[Integer]]
pd = [1] : map nextRow pd
  where nextRow r = zipWith (+) (0:r) (r ++ [0])

fibdiag :: Integer -> [Integer]
fibdiag n = reverse $ drop (fromInteger $ n `quot` 2) $  zipWith (!!) pd (reverse [0..(fromInteger (n-1))])

fibdiags :: [[Integer]]
fibdiags = map fibdiag [1..]

fibspd :: [Integer]
fibspd = map sum fibdiags
