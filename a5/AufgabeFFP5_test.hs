{-# LANGUAGE TemplateHaskell #-}

-- run with hugs:
-- $ runghc -cpp THISFILE
-- or with ghc:
-- $ runhugs -F"cpp -P -traditional" THISFILE

import Test.HUnit
import Data.Array
import Control.Exception

#if defined(__GLASGOW_HASKELL__)
import Test.HUnit.Tools
#endif

import AufgabeFFP5

-- helpers

#if defined(__GLASGOW_HASKELL__)
instance Eq ErrorCall where
  x == y = (show x) == (show y)

assertError msg ex f =
  assertRaises msg (ErrorCall ex) $ evaluate f
#endif

-- fixtures

a :: Array Int Int
b :: Array Int Int
c :: Array Int Int
d :: Array Week String

a = array (1,9) [(1,3),(2,(-5)),(3,0),(4,9),
	  (5,2),(6,(-1)),(7,2),(8,(-5)),(9,1)]
b = array (1,9) [(1,3),(2,(-1)),(3,(-2)),(4,9),
	  (5,2),(6,(-1)),(7,2),(8,0),(9,(-1))]
c = array (1,5) [(1,2),(2,3),(3,(-10)),(4,1),(5,4)]

data Week = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Eq,Ord,Ix,Show)

d = array (Tue,Sat) [(Wed,"work"),(Thu,"study"),(Tue,"study"),
                     (Fri,"chill"),(Sat,"relax")]

-- tests
 
tests = TestList

  ["mas a"  ~: 12 ~=? (mas a)
  ,"mas b"  ~: 12 ~=? (mas b)
  ,"mas c"  ~: 5 ~=? (mas c)

  ,"amas a" ~: [(3,7),(4,7)]             ~=? (amas a)
  ,"amas b" ~: [(1,7),(1,8),(4,7),(4,8)] ~=? (amas b)

  ,"lmas a" ~: (3,7) ~=? (lmas a)
  ,"lmas b" ~: (1,8) ~=? (lmas b)
  ,"lmas c" ~: (1,2) ~=? (lmas c)
  
  ,"minIndex a (>5)"   ~: 4 ~=? (minIndex a (>5))
  ,"minIndex a (<0)"   ~: 2 ~=? (minIndex a (<0))
  ,"minIndex a (even)" ~: 3 ~=? (minIndex a (even))
  ,"minIndex b (odd)"  ~: 1 ~=? (minIndex b (odd))
#if defined(__GLASGOW_HASKELL__)
  ,"minIndex b (>100)" ~: assertError "" "No matching data" (minIndex b (>100))
#endif
  
  ,"minIndex d (==\"relax\")" ~: Sat ~=? (minIndex d (=="relax"))
  ,"minIndex d (==\"work\")"  ~: Wed ~=? (minIndex d (=="work" ))
  ,"minIndex d (==\"chill\")" ~: Fri ~=? (minIndex d (=="chill"))
  ,"minIndex d (/=\"chill\")" ~: Tue ~=? (minIndex d (/="chill"))
#if defined(__GLASGOW_HASKELL__)
  ,"minIndex d (==\"swim\")"  ~: assertError "" "No matching index" (minIndex d (=="swim"))
#endif

  ]

main = do
  runTestTT tests
