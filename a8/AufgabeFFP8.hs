module AufgabeFFP8 where

type TargetValue = Integer
type Digit  = Integer  -- ausschliesslich Werte 1,2,..,9
type Digits = [Digit]

digits = [1..9] :: Digits

data Operator = P | T deriving (Eq,Show)  -- P fuer plus, T fuer times
data Expr = Opd Digit               -- hier jede natuerliche Zahl zulaessig
          | Opr Operator Expr Expr deriving (Eq,Show)

mkTV :: Digits -> TargetValue -> [Expr]
mkTV dgts tv =
  filterExpr tv $
  multDgts $ concatMap genGrps $
  filterComp tv $
  compDgts $ genGrps dgts

-- genCpsd [1,2,3,4] ->> [[1,2,3,4],[12,3,4],[123,4],[1234],[1,23,4],...]

genGrps :: Digits -> [[Digits]]
genGrps []   = [[]]
genGrps dgts =
  concatMap grpFst [1..len]
    where
      len = length dgts
      grpFst i = allGrpngs (take i dgts) (drop i dgts)
      allGrpngs head tail = map (\x -> [head] ++ x) $ genGrps tail

compDgts :: [[Digits]] -> [Digits]
compDgts =
  map (map (sumUp . reverse))
    where
      sumUp []     = 0
      sumUp (d:ds) = d + 10*(sumUp ds)

filterComp :: TargetValue -> [Digits] -> [Digits]
filterComp tv = filter ((<= tv) . sum)

multDgts :: [[Digits]] -> [Expr]
multDgts =
  map (add . (map multiply))
    where
      multiply [a]        = Opd a
      multiply (a:b:tail) = Opr T (Opd a) (multiply (b:tail))
      add [a]        = a
      add (a:b:tail) = Opr P a (add (b:tail))

filterExpr :: TargetValue -> [Expr] -> [Expr]
filterExpr tv =
  filter ((==tv) . evalExpr)
  
evalExpr :: Expr -> Integer
evalExpr (Opd d) = d
evalExpr (Opr P a b) = (evalExpr a) + (evalExpr b)
evalExpr (Opr T a b) = (evalExpr a) * (evalExpr b)
