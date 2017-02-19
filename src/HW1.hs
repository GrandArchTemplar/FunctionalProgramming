module HW1 where
--Pavel Gilyov
--HomeTask 1
--Task 1

--Написать функцию bitOnes :: Integer -> Int, которая выдает количество единиц
--в битовом представлении заданного натурального числа. Например, число 36 имеет
--битовое представление 100100, поэтому bitOnes 36 => 2

--returns count of bits in binary representation of number
bitOnes :: Integer -> Int
bitOnes 1 = 1
bitOnes 0 = 0
bitOnes n | n < 0 = bitOnes (-n)
          | otherwise = bitOnes (div n 2) + bitOnes (mod n 2)

--Task2

--Написать функцию firstPrime :: Integer -> Integer, которая выдает наименьшее
--простое число, большее заданного натурального аргумента, последняя цифра
--которого - единица. Например, firstPrime 100 => 101, firstPrime 101 => 131.
--Программа должна работать быстро для больших значений
--аргумента (порядка 10-12-значных чисел), например,
--firstPrime 222222222222 => 222222222301.

--Return true if number is prime and otherwise return false
--Checks by divinding n by ∀ odd numbers from 3 to ⌈sqrt(n)⌉
--Deprecated (!!!)
--VERY SLOW
--Asymtotic: Θ(n)
isPrimeOld :: Integer -> Bool
isPrimeOld 2 = True
isPrimeOld n = not $ any (\x -> (mod n x) == 0) $ list where
  list :: [Integer]
  list = 2:[3,5..bound + 1] where
    bound :: Integer
    bound = round . sqrt . fromIntegral $ n

--Моre optimized algorithm to check primal is number or not
--Checks by divinding n by ∀ 6k ± 1 numbers from k = 1 to k = ⌈sqrt(n)⌉ div 6
--Asymptotic: Θ(n)
isPrime :: Integer -> Bool
isPrime 2 = True
isPrime 3 = True
isPrime n = not $ any (\x -> (mod n x) == 0) $! list where
  list = 2:3:[5, 11.. bound + 1] ++ [7, 13.. bound + 1] where
    bound :: Integer
    bound = round . sqrt . fromIntegral $ n

--Моre optimized algorithm to check primal is number or not
--Check by dividing only ∀ k from list
--if list will be consist of primals only it will be the most quick determined
--algorithm of checking on primary
--Asymptotic: Θ(m) where m -- size of list
isPrimeParametred :: Integer -> [Integer] -> Bool
isPrimeParametred n list = not $ any (\x -> (mod n x) == 0) $ list

--Returns all primals from 2 to bound
--Asymptotic: Θ(bound)
primeList :: Integer -> [Integer]
primeList bound = filter isPrime list1 ++ filter isPrime list2 where
  list1 :: [Integer]
  list1 = takeWhile (<= bound) (2:3:[5, 11..])
  list2 :: [Integer]
  list2 = takeWhile (<= bound) [7, 13..]

--Returns next prime number with ending by digit x
--Asymptotic: undefined for big primes
--and Θ(n * k) where k number of primes before n
nextPrime :: Integer -> Integer -> Integer
nextPrime n 2 | n < 2 = 2
nextPrime _ x | (mod x 2) == 0 = error "Second parameter must be a odd digit"
              | x >= 10 = error "Second parameter must be a digit"
--deal n end an x
nextPrime n x | (mod n 10) < x = nextPrime (n - (mod n 10) + x) x
              | (mod n 10) > x = nextPrime (n - (mod n 10) + x + 10) x
              | otherwise = helper (primeList (bound * 2)) (bound * 2) n where
                bound :: Integer
                bound = round . sqrt . fromIntegral $ n
                --helper have an list of all primals so check for prime isn't
                --long as can be
                helper :: [Integer] -> Integer -> Integer -> Integer
                helper l b n | isPrimeParametred n l = n
                helper l b n | b * b < n = helper (primeList (b * 2)) (b * 2) n
                helper l b n | otherwise = helper l b (n + 10)

--firstPrime return an prime which > n and ends by 1
firstPrime :: Integer -> Integer
firstPrime n | isPrime n = nextPrime (n + 1) 1
             | otherwise = nextPrime n 1
--Task3

--Число назовем палиндромом, если цифры его десятичного представления одни и
--те же при чтении числа слева направо и справа налево. Например,
--числа 0, 5, 1771 и 222 - палиндромы, а 17, 500 и 10002 - нет.
--Написать функцию palindrom :: Integer -> Bool, которая проверяет,
--является ли число палиндромом.

--returns true if input number is palindrom
palindrom :: Integer -> Bool
palindrom i | i < 0 = palindrom (-i)
            | otherwise = helper . show $ i where
  --helper returns true is input string is palindrom by definition of palindrom
  helper :: [Char] -> Bool
  helper s = s == reverse s

--Task4

--Написать функцию hasPair :: Integer -> Bool, которая проверяет, есть ли в
--десятичной записи заданного числа две подряд идущие одинаковые цифры.
--Например, hasPair 1001 => True, а hasPair 1212 => False.

hasOneSidePair :: Integer -> Bool
hasOneSidePair i | i < 10 = False
hasOneSidePair i = hasPair (div i 100) || palindrom (mod i 100)
hasPairOld :: Integer -> Bool
hasPairOld i = hasOneSidePair i ||
            hasOneSidePair (div i 10)

--returns true if input number have a pair
hasPair :: Integer -> Bool
hasPair n | n < 0 = hasPair (-n)
          | otherwise = helper 'a' 'b' (show n) where
            helper a b [] = (a == b)
            helper a b (x:xs) = (a == b) || (b == x) || helper b x xs
--simple assertion
assert :: (Show a, Eq a) => a -> a -> [Char] -> [Char]
assert a e m | a == e = m ++ " OK"
             | otherwise = "Error in " ++ m ++ ". Expected: " ++ (show e)
                                       ++ " but find:" ++ (show a)

--Test for task1
test1'1 :: [Char]
test1'1 = assert (bitOnes 36) 2 "Test1.1"
test1'2 :: [Char]
test1'2 = assert (bitOnes 0) 0 "Test1.2"
test1'3 :: [Char]
test1'3 = assert (bitOnes 31) 5 "Test1.3"
--Test for task2
test2'1 :: [Char]
test2'1 = assert (firstPrime 100) 101 "Test2.1"
test2'2 :: [Char]
test2'2 = assert (firstPrime 101) 131 "Test2.2"
test2'3 :: [Char]
test2'3 = assert (firstPrime 222222222222) 222222222301 "Test2.3"
--average time of exec ~= 80 seconds
test2'4 :: [Char]
test2'4 = assert (firstPrime 22222222222222) 22222222222261 "Test2.4"
--Test for task3
test3'1 :: [Char]
test3'1 = assert (palindrom 0) True "Test3.1"
test3'2 :: [Char]
test3'2 = assert (palindrom 5) True "Test3.2"
test3'3 :: [Char]
test3'3 = assert (palindrom 1771) True "Test3.3"
test3'4 :: [Char]
test3'4 = assert (palindrom 222) True "Test3.4"
test3'5 :: [Char]
test3'5 = assert (palindrom 17) False "Test3.5"
test3'6 :: [Char]
test3'6 = assert (palindrom 500) False "Test3.6"
test3'7 :: [Char]
test3'7 = assert (palindrom 10002) False "Test3.7"
--Test for task4
test4'1 :: [Char]
test4'1 = assert (hasPair 11) True "Test4.1"
test4'2 :: [Char]
test4'2 = assert (hasPair 2112) True "Test4.2"
test4'3 :: [Char]
test4'3 = assert (hasPair 1212) False "Test4.3"
test4'4 :: [Char]
test4'4 = assert (hasPair (-132183791341)) False "Test4.4"
