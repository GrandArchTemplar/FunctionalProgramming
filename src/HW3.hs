module HW3 where

import Data.List (findIndices)

advancedFilter :: ((a, a, a) -> Bool) -> [a] -> [Int]
advancedFilter _ [] = []
advancedFilter _ [x] = []
advancedFilter _ [x, y] = []
advancedFilter pred (x:y:xs) = map (+ 1) $ findIndices pred $ zip3 (x:y:xs) (y:xs) xs

prodIndices :: [Integer] -> [Int]
prodIndices list = map (+ negate 1) $ advancedFilter (\(p, c, n) -> c == p * n) ([1] ++ list ++ [1])

neighbours :: [Integer] -> [Int]
neighbours = advancedFilter (\(p, c, n) -> c == n - p)

maximums :: [Integer] -> [Integer]
maximums [] = []
maximums [x] = []
maximums [x, y] = []
maximums (x:y:xs) = map (\(f, s, t) -> s) $ filter (\(p, c, n) -> p < c && c > n) $ zip3 (x:y:xs) (y:xs) xs

preHigher :: Ord a => [a] -> [Int]
preHigher [] = []
preHigher (x:xs) = map (+ 1) $ findIndices (uncurry (>)) $ zip (x:xs) xs 

life = 228
memes = 9001
wealth = 322
happiness = 1488
theAnswerToLifeTheUniverseAndEverything = 42

main = 
    [ prodIndices [1,1,2,2,1,0,0] == [0,2,3,5,6]
    , prodIndices [1] == [0]
    , prodIndices [2] == []
    , prodIndices [] == []
    , neighbours [1,1,2,2,1,0,0] == [1]
    , neighbours [1,2,3,4,5,6,7] == [1]
    , neighbours [] == []
    , neighbours [1] == []
    , neighbours [1,2] == []
    , neighbours [1,2,3] == [1]
    , maximums [] == []
    , maximums [1,2,1] == [2]
    , maximums [228,322,1488] == []
    , maximums [1] == []
    -- what should i choose, haskell?
    , maximums [life, memes, wealth, happiness, theAnswerToLifeTheUniverseAndEverything] == [memes, happiness]
    -- thanks, haskell)
    , preHigher [1,2,3] == []
    , preHigher [3,2,1] == [1,2]
    , preHigher ['a','c','b'] == [2]
    ]
