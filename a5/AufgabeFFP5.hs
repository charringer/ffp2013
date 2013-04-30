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

divideAndConquer :: (p -> Bool) -> (p -> s) -> (p -> [p]) -> (p -> [s] -> s) -> p -> s
divideAndConquer indiv solve divide combine initPb = dAC initPb
  where
    dAC pb
      | indiv pb = solve pb
      | otherwise = combine pb (map dAC (divide pb))

a@(Just _) ||| _ = a
Nothing ||| b = b

mi_indiv ls = length ls <= 1
mi_solve [(i,b)] = if b then Just i else Nothing
mi_divide (l:ls) = [[l], ls]
mi_combine _ = foldr (|||) Nothing

minIndex :: (Ix a, Show a) => Array a b -> (b -> Bool) -> a
minIndex array wf = case minidx of { Just a -> a; Nothing -> error "No matching index" }
  where minidx = divideAndConquer mi_indiv mi_solve mi_divide mi_combine [ (i, wf (array ! i)) | i <- indices array ]
