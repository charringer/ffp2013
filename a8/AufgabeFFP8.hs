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
  filterExprEquals tv $
  multiplyGroups $ concatMap genGroups $
  filterSumMax tv $
  joinDigitGroups $ genGroups dgts

genGroups :: Digits -> [[Digits]]
genGroups []   = [[]]
genGroups dgts =
  concatMap grpFst [1..len]
    where
      len = length dgts
      grpFst i = allGrpngs (take i dgts) (drop i dgts)
      allGrpngs head tail = map (\x -> [head] ++ x) $ genGroups tail

-- convert grouped lists of digits to lists of joined digits
joinDigitGroups :: [[Digits]] -> [Digits]
joinDigitGroups =
  map (map (sumUp . reverse))
    where
      sumUp []     = 0
      sumUp (d:ds) = d + 10*(sumUp ds)

-- throw away all 'Digits' (lists of digits) which smallest possible
-- combination exceeds a target value. we found out that this is the sum of all
-- elements ≠ 1, because 1 can be multiplied without rising the result.
filterSumMax :: TargetValue -> [Digits] -> [Digits]
filterSumMax tv = filter ((<= tv) . sum . (filter (==1)))

multiplyGroups :: [[Digits]] -> [Expr]
multiplyGroups =
  map (sum . (map multiply))
    where
      multiply [a]        = Opd a
      multiply (a:b:tail) = Opr T (Opd a) (multiply (b:tail))
      sum [a]        = a
      sum (a:b:tail) = Opr P a (sum (b:tail))

filterExprEquals :: TargetValue -> [Expr] -> [Expr]
filterExprEquals tv =
  filter ((==tv) . evalExpr)
  
evalExpr :: Expr -> Integer
evalExpr (Opd d) = d
evalExpr (Opr P a b) = (evalExpr a) + (evalExpr b)
evalExpr (Opr T a b) = (evalExpr a) * (evalExpr b)
