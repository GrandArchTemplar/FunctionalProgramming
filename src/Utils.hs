module Utils where

--simple assertion
assert :: (Show r, Eq r) => r -> r -> String -> String
assert ac ex msg | ac == ex = msg ++ " OK"
                 | otherwise = "Error in " ++ msg ++ ". Expected: " ++ (show ex)
                                           ++ " but found:" ++ (show ac)

--test unary function
unaryTest :: (Show e, Eq e) => (a -> e) -> [(a, e, String)] -> String
unaryTest f heap = unlines $ map (\(ac, ex, msg) -> assert (f ac) ex msg) heap

