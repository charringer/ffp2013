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

endsToPairs word = filterOverlaps . initToIndex word

occS :: Text -> Word -> [(First,Last)]
occS txt word = endsToPairs word $ map length $ filter (isSuffixOf word) $ inits txt

occI :: Text -> Word -> [(First,Last)]
occI _ _ = []

-- I'm really starting to get fed up with these ancient hugs libraries.
-- And we can't even use CPP to make this work with GHC...
instance Arbitrary Char where
  arbitrary     = choose ('a', 'b')
  coarbitrary c = variant (ord c `rem` 2)

prop_coincide :: Text -> Word -> Bool
prop_coincide txt word = occS txt word == occI txt word
