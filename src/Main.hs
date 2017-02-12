module Main where

import HW1

main :: IO()
main = putStr . unlines $ [test1'1, test1'2, test1'3,
  test2'1, test2'2, test2'3, --test2'4,
    test3'1, test3'2, test3'3, test3'4, test3'5, test3'6 , test3'7,
      test4'1, test4'2, test4'3, test4'4]
