module HW2 where

import Text.Regex
import Data.List
import Utils

--blackMagic вспомогательная функция позволяющая решать задачу поиска наибольшей подпоследовательности, соотвествующей предикату направо:
--для любых i < j в этой последовательности верно, что p i j = True
--асимптотика: Θ(n), где n -- длина списка 
blackMagic :: (a -> a -> Bool) -> Int -> Int -> [a] -> [a] -> [a] -> [a]
blackMagic pred maxL curL ans acc [] | maxL > curL = reverse ans
                                     | otherwise = reverse acc
blackMagic pred maxL 0 ans [] (x:xs) = blackMagic pred maxL 1 ans [x] xs     
blackMagic pred maxL curL ans (end:acc) (x:xs) | pred end x = blackMagic pred maxL (curL + 1) ans (x:end:acc) xs
                                               | curL > maxL = blackMagic pred curL 0 (end:acc) [] (x:xs)
                                               | otherwise = blackMagic pred maxL 0 ans [] (x:xs) 

-- Написать функцию increasing :: Ord a => [a] -> [a], которая в списке значений находит неубывающий отрезок максимальной длины. 
-- Например, в списке [1,3,4,3,6,7,3] таким отрезком может быть [1,3,4] или [3,6,7] (можно выдать любой из этих двух), а в списке [5,4,2,1]
-- результатом может быть любой из одноэлементных списков [5], [4], [2] или [1].
increasing :: Ord a => [a] -> [a]
increasing = blackMagic (<=) 0 0 [] []

-- parseInt -- функция выделяющая числа из строки.
-- принимает регэксп разделителя и саму строку 
-- ВНИМАНИЕ! Не работает, если разделиьедб неверный
parserInt :: String -> String -> [Integer]
parserInt del str = map read $ filter (not . null) $ splitRegex (mkRegex del) $ str

-- В заданной строке символов будем считать числом произвольную последовательность цифр, слева и справа от которой не находится цифра. 
-- Написать функцию sumNumbers :: String -> Integer, которая вычисляет сумму всех “чисел” в заданной строке. 
-- Например, для аргумента "0012 3xaxa-1000" результатом должно быть число 1015 (12 + 3 + 1000)
sumNumbers :: String -> Integer 
sumNumbers = sum . parserInt "[^0-9]"


-- Написать функцию segmEquals :: Eq a => [a] -> [a], которая в списке значений находит отрезок максимальной длины подряд стоящих равных значений. 
-- Например, в списке [1,3,3,6,7,3] таким отрезком будет [3,3], а в списке [5,5,5,1,1,1] результатом может быть любой из двух 
-- списков [5,5,5] или [1,1,1].
segmEquals :: Eq a => [a] -> [a]
segmEquals = blackMagic (==) 0 0 [] []

-- В заданной строке символов будем считать числом произвольную последовательность цифр, возможно, со знаком ‘+’ или ‘-’ перед ней, 
-- слева и справа от которой не находится ни цифра, ни знак ‘+’ или ‘-’. Написать функцию amountOfNumbers :: String -> Int, 
-- которая вычисляет количество всех “чисел” в заданной строке. Например, для аргумента "0012 3xaxa-1000" результатом должно 
-- быть 3 (числа 0012, 3 и -1000), а для аргумента "-17+12-1-" результатом будет 0.
amountOfNumbers :: String -> Int
amountOfNumbers =  length . parserInt "[^0-9]|([-0-9]+-[0-9]*)"

-- Написать функцию maxProd :: Real a => [a] -> [a], которая в списке арифметических значений находит тройку подряд идущих элементов с 
-- максимальным произведением (можно считать, что список содержит по крайней мере 3 элемента). Например, в списке [1,3,3,6,7,3] такой 
-- тройкой будет [3,6,7], (или [6,7,3]) а в списке [-10,8,4,2,4,4,4] результатом может быть любой из двух списков [8,4,2] или [4,4,4].
maxProd :: Real a => [a] -> [a]
maxProd list = helper [] [] list where
    helper ans acc [] | product ans > product acc =  ans
                      | otherwise = acc
    helper ans [] (x:y:z:xs) = helper ans [x, y, z] xs
    helper [] acc@(acx:acy:acz:[]) (x:xs) = helper acc acc (x:xs)
    helper ans@(ax:ay:az:[]) acc@(acx:acy:acz:[]) (x:xs) | product ans > product acc = helper ans (acy:acz:x:[]) xs
                                                         | otherwise = helper acc (acy:acz:x:[]) xs  
       
-- parser -- функция, которая возвращает набор строк, полученной разделением данной строки, при помощи
-- разделителя                                                  
parser :: String -> String -> [String]
parser del = splitRegex $ mkRegex del

-- Будем считать “знаками препинания” в заданной строке следующие символы и последовательности символов: ".", ",", ";", ":", "?", "!", "...". 
-- Написать функцию signs :: String -> Int, которая подсчитывает количество знаков препинания в заданной строке. 
-- Каждые последовательные три точки считаются за один знак препинания, так что, например, в строке "......." (7 точек) 
-- имеется 3 знака препинания (два многоточия и точка). В строке "Hi, Bill! Bill?.. Oh, Bill..." всего 7 знаков препинания.
signs :: String -> Int
signs str = length . filter (not . null) $ parser "[^.,;:?!]|" $ cut' str where
    cut' [] = []
    cut' [x] = [x]
    cut' (x:y:[]) = x:y:[]
    cut' ('.':'.':'.':xs) = '.':(cut' xs)
    cut' (x:y:z:xs) = x:(cut' (y:z:xs))

-- Назовем “производной” элемента числового списка разность между следующим в списке и данным элементом. 
-- Производной последнего элемента будем считать 0. Например, список производных элементов списка [3,1,2,5,7,6] будет список [-2,1,3,2,-1,0]. 
-- Написать функцию maxDeriv :: Real a => [a] -> Int, которая в заданном списке числовых элементов находит индекс элемента с максимальной
-- “производной”. Например, в вышеприведенном списке таким индексом будет 2, поскольку элемент с этим индексом имеет максимальную производную - 3.
maxDeriv :: Real a => [a] -> Int    
maxDeriv (x:xs) = fst . maximumBy (\(_, a) -> \(_, b) -> compare a b) $ zip [0..] $ zipWith (-) (xs ++ [last xs]) $ x:xs 

-- В заданной строке символов будем считать числом произвольную последовательность цифр, слева и справа от которой не находится цифра.
-- Написать функцию maxNumber :: String -> Integer, выдающую числовое значение самого большого “числа” в строке. 
-- Например, результатом вызова функции с аргументом "0xFF55 00012 -100 19" должно быть 100. 
-- Если чисел в строке нет совсем, то это ошибка аргумента.
maxNumber :: String -> Integer
maxNumber = maximum . parserInt "[^0-9]"

test11Func :: [Integer] -> [Integer]
test11Func = increasing :: [Integer] -> [Integer]
test11Data :: [([Integer], [Integer], String)]
test11Data = [
                ([1, 2, 3]            , [1, 2, 3]   , "Test 1.1.1"),
                ([1]                  , [1]         , "Test 1.1.2"),
                ([1, 2, 3, 3, 2, 1]   , [1, 2, 3, 3], "Test 1.1.3"),
                ([3, 4, 5, 6, 1, 2, 3], [3, 4, 5, 6], "Test 1.1.4"),
                ([1, 2, 3]            , [1, 2, 3]   , "Test 1.1.5")
             ]
test12Func :: String -> Integer
test12Func = sumNumbers
test12Data :: [(String, Integer, String)]
test12Data = [
                ("100"            , 100            , "Test 1.2.1"),
                ("222222222222222", 222222222222222, "Test 1.2.2"),
                ("abc222abc222"   , 444            , "Test 1.2.3"),
                ("222-222 333"    , 777            , "Test 1.2.4"),
                ("222-222 222+222", 888            , "Test 1.2.5")
             ]
test21Func :: [Integer] -> [Integer]
test21Func = segmEquals
test21Data :: [([Integer], [Integer], String)]
test21Data = [
                ([1]         , [1]   , "Test 2.1.1"),
                ([1, 2]      , [2]   , "Test 2.1.2"),
                ([1, 2, 2]   , [2, 2], "Test 2.1.3"),
                ([1, 2, 2, 3], [2, 2], "Test 2.1.4"),
                ([1, 0, 0, 2], [0, 0], "Test 2.1.5")
             ]
test22Func :: String -> Int
test22Func = amountOfNumbers
test22Data :: [(String, Int, String)]
test22Data = [
               ("100"          , 1, "Test 2.2.1"), 
               ("100 +100"     , 2, "Test 2.2.2"), 
               ("100 +100 -100", 3, "Test 2.2.3"), 
               ("100-100"      , 0, "Test 2.2.4"), 
               ("a100"         , 1, "Test 2.2.5")
             ] 
test31Func :: [Integer] -> [Integer]
test31Func = maxProd 
test31Data :: [([Integer], [Integer], String)]    
test31Data = [
                ([1000, 1000, 1000]           , [1000, 1000, 1000]  , "Test 3.1.1"),
                ([1000, 1000, 1000, 1]        , [1000, 1000, 1000]  , "Test 3.1.2"),
                ([1, 1000, 1000, 1000]        , [1000, 1000, 1000]  , "Test 3.1.3"),
                ([1000, 1000, 1000, -4000, 1] , [1000, 1000, 1000]  , "Test 3.1.4"),
                ([1000, -1000, -1000, 1, 1, 1], [1000, -1000, -1000], "Test 3.1.5")
             ]
test32Func :: String -> Int
test32Func = signs
test32Data :: [(String, Int, String)]
test32Data = [
                ("abc"                   , 0, "Test 3.2.1"),
                ("abc."                  , 1, "Test 3.2.2"),
                (",abc."                 , 2, "Test 3.2.3"),
                ("a:;,!?.abc"            , 6, "Test 3.2.4"),
                (".......dsa...da?..?...", 9, "Test 3.2.5")
             ] 
test41Func :: [Integer] -> Int
test41Func = maxDeriv
test41Data :: [([Integer], Int, String)]
test41Data = [
                ([1,2,3]      , 1, "Test 4.1.1"),
                ([4,3,2,1]    , 3, "Test 4.1.2"),
                ([1,2,4,5]    , 1, "Test 4.1.3"),
                ([2,2,2]      , 2, "Test 4.1.4"),
                ([3,1,2,5,7,6], 2, "Test 4.1.5")
             ]
test42Func :: String -> Integer
test42Func = maxNumber
test42Data :: [(String, Integer, String)]
test42Data = [
                ("1"                   , 1                , "Test 4.2.1"),
                ("1abc2"               , 2                , "Test 4.2.2"),
                ("22222222222222222"   , 22222222222222222, "Test 4.2.3"),
                ("-1000 0012 "         , 1000             , "Test 4.2.4"),
                ("0xFF55 00012 -100 19", 100              , "Test 4.2.5")
             ]
