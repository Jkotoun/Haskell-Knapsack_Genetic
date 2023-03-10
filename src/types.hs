module Types (Knapsack (Knapsack), weight, cost, Item (Item), maxWeight, minCost, items) where

data Item = Item
  { weight :: Int,
    cost :: Int
  }
  deriving (Show, Eq)

data Knapsack = Knapsack
  { maxWeight :: Int,
    minCost :: Int,
    items :: [Item]
  }
  deriving (Show, Eq)
