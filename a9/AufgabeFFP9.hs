module AufgabeFFP9 where
import Data.List

type Text = String
type Word = String
type First = Int
type Last  = Int

initToIndex word is = [ (length i - length word, length i) | i <- is ]

filterOverlaps ((a,b) : (c,d) : idxs)
  | b > c = filterOverlaps ((a,b) : idxs)
  | otherwise = (a,b) : filterOverlaps ((c,d) : idxs)
filterOverlaps idxs = idxs

occS :: Text -> Word -> [(First,Last)]
occS txt word = filterOverlaps $ initToIndex word $ filter (isSuffixOf word) $ inits txt

occI :: Text -> Word -> [(First,Last)]
occI _ _ = []

prop_coincide :: Text -> Word -> Bool
prop_coincide txt word = occS txt word == occI txt word
