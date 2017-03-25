module CW5 where

inc2List = map (+2)

dec2List = map (flip (-) 2)
--dec2List' :: [Integer] -> [Integer]
dec2List' = zipWith ((-)  2) (0 <$ [1..])
dec2List'' = map (+ negate 2)

fifth = (!! 4)

sumLists :: [[a]] -> Int 
sumLists = sum . map length 

type Set a = a -> Bool

mT = (> 3)
lT = (< 10)

intersect :: Set a ->  Set a -> Set a
intersect p1 p2 = \arg -> p1 arg && p2 arg

union :: Set a ->  Set a -> Set a
union p1 p2 = \arg -> p1 arg || p2 arg

add :: Eq a => Set a ->  a  -> Set a
add p arg = union p (== arg)

