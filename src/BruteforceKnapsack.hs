
module BruteforceKnapsack
(
    bruteforce
)where
import Types
import Data.List (subsequences)


-- find solution using bruteforce solution (try all combinations, O(2^n))
bruteforce :: Knapsack -> IO (Either Bool [Int] )
bruteforce (Knapsack maximumWeight minimalCost itemsList) = case bestSolution (0, Nothing) maximumWeight minimalCost (subsequences itemsList) of
  Nothing -> return $ Left False
  Just solution -> return $ Right $ solutionBitmap itemsList solution

-- get maybe best solution (nothing if solution does not exist)
bestSolution :: (Int, Maybe [Item]) -> Int -> Int -> [[Item]] -> Maybe [Item]
bestSolution best _ _ [] = snd best
bestSolution (currentBestCost, solutionItems) maximalWeight minimalCost (x : xs) =
  let isValidAndBetterSolution = isValidAndBetter currentBestCost maximalWeight minimalCost x
   in if fst isValidAndBetterSolution
        then bestSolution (snd isValidAndBetterSolution, Just x) maximalWeight minimalCost xs
        else bestSolution (currentBestCost, solutionItems) maximalWeight minimalCost xs

-- check if given solution is valid and is better than current best found
isValidAndBetter :: Int -> Int -> Int -> [Item] -> (Bool, Int)
isValidAndBetter currentBest maximalWeight minimalCost itemsList =
  let (totalWeight, totalCost) = foldl (\(weightSum, costSum) (Item weight cost) -> (weightSum + weight, costSum + cost)) (0, 0) itemsList
   in (totalWeight <= maximalWeight && totalCost >= minimalCost && totalCost > currentBest, totalCost)

-- map found solution to bitmap
solutionBitmap :: [Item] -> [Item] -> [Int]
solutionBitmap allItems solutionItems = map (\item -> if item `elem` solutionItems then 1 else 0) allItems

