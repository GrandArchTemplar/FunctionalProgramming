module Main where

import HW1
import HW2
import Utils

main :: IO()
main = putStr . unlines $ [
                            unaryTest test11Func test11Data, unaryTest test12Func test12Data, 
                            unaryTest test21Func test21Data, unaryTest test22Func test22Data,
                            unaryTest test31Func test31Data, unaryTest test32Func test32Data,
                            unaryTest test41Func test41Data, unaryTest test42Func test42Data
                          ]
