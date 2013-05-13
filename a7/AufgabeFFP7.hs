module AufgabeFFP7 where

import Data.Array

-- lolwut, was istn das für a api???!?!?

data Color  = Black | White deriving Show
data Digit  = One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Blank deriving Show
type Str8ts = Array (Int,Int) (Color,Digit) --deriving Show

type ColorOut  = Char -- Nur ’b’ fuer schwarz und ’w’ fuer weiss
                      -- werden benutzt.
type DigitOut  = Int  -- Nur 1,2,3,4,5,6,7,8,9,0 werden benutzt;
                      -- 0 wird dabei als Platzhalter fuer ’blank’ benutzt.
type Str8tsOut = Array (Int,Int) (ColorOut,DigitOut)

swap (a, b) = (b, a)

tr a = ixmap (bounds a) swap

naiveStr8ts :: Str8ts -> Str8tsOut
naiveStr8ts _ = array ((0,0),(-1,-1)) []

fastStr8ts :: Str8ts -> Str8tsOut
fastStr8ts _ = array ((0,0),(-1,-1)) []
