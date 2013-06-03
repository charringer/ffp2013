module AufgabeFFP9 where
import Data.List
import Test.QuickCheck
import Data.Char

type Text = String
type Word = String
type First = Int
type Last  = Int

initToIndex word is = [ (i - length word, i) | i <- is ]

filterOverlaps ((a,b) : (c,d) : idxs)
  | b > c = filterOverlaps ((a,b) : idxs)
  | otherwise = (a,b) : filterOverlaps ((c,d) : idxs)
filterOverlaps idxs = idxs

modifyIndices :: [(First,Last)] -> [(First,Last)]
modifyIndices = map (\(i,j) -> (i,j-1))

endsToPairs word = modifyIndices . filterOverlaps . initToIndex word

occS :: Text -> Word -> [(First,Last)]
occS txt word = endsToPairs word $ map length $ filter (isSuffixOf word) $ inits txt

occI :: Text -> Word -> [(First,Last)]
occI txt word = endsToPairs word $ test m $ scanl step (0, []) txt
  where
    test j [] =	[]
    test j ((n, sx) : nxs)
      | i == m      = n : test k (drop (k-1) nxs)
      | m - k <= i  = test k (drop (k-1) nxs)
      | otherwise   = test m (drop (k-1) nxs)
      where i' = llcp sw (take j sx)
	    i = if i' == j then m else i'
	    k = shift sw i

    (sw, m) = (reverse word, length word)

    shift sw i = head [ k | k <- [1..m], llcp sw (drop k sw) == min i (m-k) ]

step (n, sx) x = (n+1, x:sx)

llcp xs [] = 0
llcp [] ys = 0
llcp (x:xs) (y:ys) = if x == y then 1 + llcp xs ys else 0

-- I'm really starting to get fed up with these ancient hugs libraries.
-- And we can't even use CPP to make this work with GHC...
instance Arbitrary Char where
  arbitrary     = choose ('a', 'b')
  coarbitrary c = variant (ord c `rem` 2)

prop_coincide :: Text -> Word -> Bool
prop_coincide _ [] = True
prop_coincide txt word = occS txt word == occI txt word
