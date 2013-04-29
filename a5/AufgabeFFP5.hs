module AufgabeFFP5 where

import Data.Array
import Data.List

valueaddedlist :: (Ix i) => Array i Int -> [(i, Int, Int, Int, Int)]
valueaddedlist a = scanl step (init a) (tail $ assocs a)
  where
    init a = (i0, x0, 0, 0, x0)
      where
	i0 = fst $ bounds a
	x0 = a ! i0
    step (i,sum',_,sms',cms') (j,x) = (j,sum,sum',sms,cms)
      where
	-- plain old sum
	sum = sum' + x
	-- shifted minimal sum (from first until the previous)
	sms = min sms' sum'
	-- current maximal sum (for segment that ends here)
	cms = sum - sms

mas :: Array Int Int -> Int
mas = maximum . (map tocms) . valueaddedlist
  where
    tocms (_,_,_,_,cms) = cms

amas :: Array Int Int -> [(Int,Int)]
amas a = sort . (concatMap fromto) . (filter best) $ val
  where
    mas_a = mas a
    best (_,_,_,_,cms) = cms == mas_a
    val = valueaddedlist a
    fromto (end,_,_,sms,_) =
      (map mktuple) . (filter smsmatches) $ val
	where
	  smsmatches (_,_,sum',_,_) = sms == sum'
	  mktuple (start,_,_,_,_) = (start,end)

lmas :: Array Int Int -> (Int,Int)
lmas a = head . (filter longest) $ amas_a
  where
    amas_a = amas a
    len (start,end) = end - start
    maxlen = maximum . (map len) $ amas_a
    longest = (==maxlen) . len

minIndex :: (Ix a, Show a) => Array a b -> (b -> Bool) -> a
minIndex array _ = head $ indices array
