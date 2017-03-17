module HW3 where

import Data.List (findIndices)

advancedFilter :: ((a, a, a) -> Bool) -> [a] -> [Int]
advancedFilter pred (x:y:xs) = map (+ 1) $ findIndices pred $ zip3 (x:y:xs) (y:xs) xs

prodIndices :: [Integer] -> [Int]
prodIndices list = map (+ negate 1) $ advancedFilter (\(p, c, n) -> c == p * n) ([1] ++ list ++ [1])

neighbours :: [Integer] -> [Int]
neighbours = advancedFilter (\(p, c, n) -> c == n - p)

maximums :: [Integer] -> [Integer]
maximums (x:y:xs) = map (\(f, s, t) -> s) $ filter (\(p, c, n) -> p < c && c > n) $ zip3 (x:y:xs) (y:xs) xs

preHigher :: Ord a => [a] -> [Int]
preHigher (x:xs) = findIndices (uncurry (<)) $ zip (x:xs) xs 
