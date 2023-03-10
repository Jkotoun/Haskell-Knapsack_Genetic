
module BruteforceKnapsack
(
    bruteforce
)where
import Types
import Data.List (subsequences)


-- find solution using bruteforce solution (try all combinations, O(2^n))
bruteforce :: Knapsack -> IO (Either Bool [Int] )
bruteforce (Knapsack maxWeight minCost items) = case bestSolution (0, Nothing) maxWeight minCost (subsequences items) of
  Nothing -> return $ Left False
  Just solution -> return $ Right $ solutionBitmap items solution

-- get maybe best solution (nothing if solution does not exist)
bestSolution :: (Int, Maybe [Item]) -> Int -> Int -> [[Item]] -> Maybe [Item]
bestSolution best _ _ [] = snd best
bestSolution (currentBestCost, solutionItems) maxWeight minCost (x : xs) =
  let isValidAndBetterSolution = isValidAndBetter currentBestCost maxWeight minCost x
   in if fst isValidAndBetterSolution
        then bestSolution (snd isValidAndBetterSolution, Just x) maxWeight minCost xs
        else bestSolution (currentBestCost, solutionItems) maxWeight minCost xs

-- check if given solution is valid and is better than current best found
isValidAndBetter :: Int -> Int -> Int -> [Item] -> (Bool, Int)
isValidAndBetter currentBest maxWeight minCost items =
  let (totalWeight, totalCost) = foldl (\(weightSum, costSum) (Item weight cost) -> (weightSum + weight, costSum + cost)) (0, 0) items
   in (totalWeight <= maxWeight && totalCost >= minCost && totalCost > currentBest, totalCost)

-- map found solution to bitmap
solutionBitmap :: [Item] -> [Item] -> [Int]
solutionBitmap allItems solutionItems = map (\item -> if item `elem` solutionItems then 1 else 0) allItems

