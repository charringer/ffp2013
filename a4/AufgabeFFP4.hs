module AufgabeFFP4 where

-- Knapsack Problem {{{1

type Weight = Int
type Value = Int
type MaxWeight = Weight

type Object = (Weight, Value)
type Objects = [Object]
type SolKnp = [Object]

type NodeKnp = (Value, Weight, MaxWeight, [Object], SolKnp)

succKnp :: NodeKnp -> [NodeKnp]
succKnp = undefined

goalKnp :: NodeKnp -> Bool
goalKnp = undefined

knapsack :: Objects -> MaxWeight -> (SolKnp, Value)
knapsack = undefined

-- Binomial coefficients {{{1

binomDyn :: (Integer, Integer) -> Integer
binomDyn = undefined

-- vim:sts=2 sw=2 fdm=marker:
