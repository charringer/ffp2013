import AufgabeFFP7

import Test.HUnit
import Data.Array
import Control.Exception

-- fixtures

c2color 'w' = White
c2color 'b' = Black

c2digit '1' = One
c2digit '2' = Two
c2digit '3' = Three
c2digit '4' = Four
c2digit '5' = Five
c2digit '6' = Six
c2digit '7' = Seven
c2digit '8' = Eight
c2digit '9' = Nine
c2digit ' ' = Blank

s2pair [a,b] = (c2color b, c2digit a)

matrix2array m =
  array ((1,1),(9,9)) [ ((i,j), s2pair x) | (i,xs) <- zip [1..] m, (j,x) <- zip [1..] xs ]

s1 :: Str8ts
s1 = matrix2array [
  [" w", " w", " b", "6b", " w", "5b", " w", "2w", " b"],
  ["9w", " w", "7w", " w", "8w", " w", " w", " w", " w"],
  ["2b", "5w", " w", " b", " b", " w", " w", "1b", "7w"],
  [" b", " w", "8w", " b", " w", "3w", " b", " w", "5w"],
  [" w", " w", "9b", " w", " w", " w", " b", " w", " w"],
  [" w", "8w", " b", " w", "3w", " b", " w", " w", " b"],
  [" w", " b", "2w", " w", " b", " b", " w", " w", "3b"],
  [" w", " w", " w", " w", "4w", " w", "5w", " w", " w"],
  [" b", " w", "1w", "7b", " w", " w", "4b", "9w", "8w"]]

-- tests

tests = TestList

  ["fast 1" ~: ('w',9) ~=? fastStr8ts s1 ! (2,1)

  ]

main = do
  runTestTT tests
