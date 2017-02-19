import Data.Char

main :: IO()
main = print $ 1

sum' :: (Num a) => [a] -> a
sum' list = helper list (fromInteger 0) where
  helper :: (Num a) => [a] -> a -> a
  helper [] ans = ans
  helper (x:xs) ans = helper xs (ans + x)

isGood :: String -> Bool
isGood = not . any isSpace

counter :: [String] -> Int
counter = length . filter isGood

insert :: (Ord a) => a -> [a] -> [a]
insert x ordList = helper' x (span (<x) ordList) where
  helper' :: (Ord a) => a -> ([a], [a]) -> [a]
  helper' x (l, ge) = l ++ (x:ge)
sorter :: (Ord a) => [a] -> [a]
sorter unO = helper'' unO [] where
  helper'' :: (Ord a) => [a] -> [a] -> [a]
  helper'' [] ans = ans
  helper'' (x:xs) ans = helper'' xs (insert x ans)
