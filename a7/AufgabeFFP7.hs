module AufgabeFFP7 where

import Data.Array
import Data.List

-- lolwut, was istn das für a api???!?!?

data Color  = Black | White deriving (Show, Eq)
data Digit  = One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Blank deriving (Enum, Eq, Ord, Show)
type Str8ts = Array (Int,Int) (Color,Digit) --deriving Show

type ColorOut  = Char -- Nur ’b’ fuer schwarz und ’w’ fuer weiss
                      -- werden benutzt.
type DigitOut  = Int  -- Nur 1,2,3,4,5,6,7,8,9,0 werden benutzt;
                      -- 0 wird dabei als Platzhalter fuer ’blank’ benutzt.
type Str8tsOut = Array (Int,Int) (ColorOut,DigitOut)

-- impl

type Cell = (Color,Digit)
type Matrix a = [[a]]

rows = id
cols [xs] = map (:[]) xs
cols (xs:xss) = zipWith (:) xs (cols xss)

choices :: Matrix Cell -> Matrix [Cell]
choices = map (map choice)

digits = [One, Two, Three, Four, Five, Six, Seven, Eight, Nine]

choice (White, Blank) = [ (White, d) | d <- digits ]
choice c = [c]

expand = cp . map cp

cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs:xss) = [x:ys | x <- xs, ys <- cp xss]

nodups (x:xs) = x `notElem` xs && nodups xs
nodups [] = True

nodupcells = nodups . filter (/= Blank) . map snd

isstreet :: [Digit] -> Bool
isstreet xs = sorted == [head sorted .. last sorted]
  where sorted = sort xs

iscellstreet = isstreet . map snd

compartments = filter ((== White) . fst . head) . groupBy (\a b -> fst a == fst b)

valid :: Matrix Cell -> Bool
valid m = all nodupcells (rows m) && all nodupcells (cols m) &&
	  all iscellstreet (concatMap compartments (rows m)) &&
	  all iscellstreet (concatMap compartments (cols m))

-- glue

naiveStr8ts :: Str8ts -> Str8tsOut
naiveStr8ts _ = array ((0,0),(-1,-1)) []

fastStr8ts :: Str8ts -> Str8tsOut
fastStr8ts _ = array ((0,0),(-1,-1)) []
