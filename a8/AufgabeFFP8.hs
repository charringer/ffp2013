module AufgabeFFP8 where

type TargetValue = Integer
type Digit  = Integer  -- ausschliesslich Werte 1,2,..,9
type Digits = [Digit]

digits = [1..9] :: Digits

data Operator = P | T deriving (Eq,Show)  -- P fuer plus, T fuer times
data Expr = Opd Digit               -- hier jede natuerliche Zahl zulaessig
          | Opr Operator Expr Expr deriving (Eq,Show)

mkTV :: Digits -> TargetValue -> [Expr]
mkTV _ _ = []
