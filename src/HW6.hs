module HW6 where

--Гилев Павел. А3200. Домашнее задание номер 6.
import Data.List (find, unionBy, deleteBy)

data Map k v = Map [(k, v)] 

instance (Show k, Show v) => Show (Map k v) where
    show (Map es) = show es

instance (Eq k, Eq v) => Eq (Map k v) where
    (==) (Map list1) (Map list2) = list1 == list2

infixr 9 -->
(-->) f x = x f


get      :: Eq k => k       -> Map k v -> Maybe v
put      :: Eq k => (k, v)  -> Map k v -> Map k v
remove   :: Eq k => k       -> Map k v -> Map k v
union    :: Eq k => Map k v -> Map k v -> Map k v
keys     ::         Map k v -> [k]
values   ::         Map k v -> [v]
pairsBy  ::         (k -> Bool) -> Map k v -> [(k, v)]
valuesBy ::         (k -> Bool) -> Map k v -> [v]
removeBy ::         (k -> Bool) -> Map k v -> Map k v

hyperPred e1 e2           = fst e1 == fst e2
omegaPred p appeal        = p . appeal   
union (Map es1) (Map es2) = Map $ unionBy hyperPred es1 es2    
get k (Map es)            = fmap snd $ flip find es $ (== k) . fst
put p m                   = union m $ Map $ p:[]
remove k (Map es)         = Map $ deleteBy hyperPred (k, undefined) es
keys (Map es)             = fmap fst es
values (Map es)           = fmap snd es
pairsBy p (Map es)        = flip filter es $ omegaPred p fst  
valuesBy p (Map es)       = fmap snd $ flip filter es $ omegaPred p fst 
removeBy p m              = Map $ pairsBy p m

--Внимание! Опасный участок. Тяжёлый для восприятия код!

{-
    LAWS: 
        I . Количество вершин дерева глубины deep = 2^deep
        II. trees отсортирован по порядку возрастания глубины
    n -- количество объектов в куче/в дереве(в зависимости от контекста)
    k -- абстратный размер
-}
data BinTree e = 
    BinTree { value :: e
            , trees :: [BinTree e]
            , deep  :: Int
            } 
            deriving (Show, Eq)
type BinHeap e = [BinTree e]

merge :: Ord e => BinHeap e -> BinHeap e -> BinHeap e
add :: Ord e => e -> BinHeap e -> BinHeap e

binResolver :: x -> x -> (x -> t) -> (t -> t -> r) -> r 
hyperMerge :: Ord t => (a -> t) -> [a] -> [a] -> [a]
binResolver a b ext f = f (ext a) (ext b)
mergeTrees :: Ord e => BinTree e -> BinTree e -> BinTree e
normalizer :: Ord e => [BinTree e] -> [BinTree e]
--deep :: BinTree e -> Integer

--O(log n)
--deep t | null $ t --> trees = 0
--       | otherwise          = (1 +) $! deep $ head $ t --> trees 

--O(max(arr1.size, arr2.size))       
hyperMerge _ list [] = list
hyperMerge _ [] list = list
hyperMerge appeal (x:xs) (y:ys) | appeal x < appeal y  = x : (hyperMerge appeal    xs  $ y : ys) 
                                | otherwise            = y : (hyperMerge appeal (x:xs) $     ys) 

--O(log n)
mergeTrees t1 t2 | binResolver t1 t2 value (>) = mergeTrees t2 t1
                 | otherwise = BinTree { value = t1 --> value
                                       , trees = hyperMerge deep [t2] $ t1 --> trees
                                       , deep  = t1 --> deep + 1
                                       }
                
--O(log n)
normalizer []                          = []
normalizer [x]                         = [x]
normalizer (x:y:xs) | deep x < deep y  = x : (normalizer $ y:xs)
                    | deep x == deep y = normalizer $ mergeTrees x y : xs
                    | deep x > deep y  = normalizer $ y:x:xs
                    
merge x y = normalizer $ hyperMerge deep x y
add e t   = merge [BinTree { value = e, trees = [], deep = 0 }] t

{-ТЕСТЫ-}

nothingCheck Nothing = True
nothingCheck _       = False
emptyCheck (Map [])  = True
emptyCheck _         = False
emptyMap = Map [] :: Eq k => Map k v
testMap1 = Map [(1, 'a'), (2, 'b'), (3, 'c')]
testMap2 = Map [(4, 'd'), (-1, 'e')]
testMap3 = Map [(1, 'a'), (2, 'b'), (3, 'c'), (4, 'd')]
mapTests = [ nothingCheck $ get 5 emptyMap
           , nothingCheck $ get 5 testMap1
           , emptyCheck $ union emptyMap emptyMap
           , emptyCheck $ remove 1 emptyMap 
           , null $ values emptyMap         
           , get 2 testMap1 == Just 'b' 
           , put (1, 'z') emptyMap   == Map [(1, 'z')]
           , put (1, 'z') testMap1   == testMap1
           , put (4, 'd') testMap1   == testMap3
           , remove 4 testMap2       == Map [(-1, 'e')]
           , remove 6 testMap1       == testMap1
           , union testMap1 testMap3 == testMap3
           , union testMap1 testMap2 == Map [(1, 'a'), (2, 'b'), (3, 'c'), (4, 'd'), (-1, 'e')]
           , keys emptyMap           == []
           , keys testMap1           == [1, 2, 3]
           , keys testMap2           == [4, -1]
           , values testMap1         == ['a', 'b', 'c']
           , values testMap2         == ['d', 'e']
           ]

singleT v = BinTree { value = v, trees = [], deep = 0}
binTests = [ foldr add [] [1..100] == [BinTree {value = 1, trees = 
               [BinTree {value = 2, trees = [], deep = 0},BinTree {value = 3, trees = 
               [BinTree {value = 4, trees = [], deep = 0}], deep = 1}], deep = 2},BinTree {value = 5, trees = 
               [BinTree {value = 6, trees = [], deep = 0},BinTree {value = 7, trees = 
               [BinTree {value = 8, trees = [], deep = 0}], deep = 1},BinTree {value = 9, trees = 
               [BinTree {value = 10, trees = [], deep = 0},BinTree {value = 11, trees = 
               [BinTree {value = 12, trees = [], deep = 0}], deep = 1}], deep = 2},BinTree {value = 13, trees = 
               [BinTree {value = 14, trees = [], deep = 0},BinTree {value = 15, trees = 
               [BinTree {value = 16, trees = [], deep = 0}], deep = 1},BinTree {value = 17, trees = 
               [BinTree {value = 18, trees = [], deep = 0},BinTree {value = 19, trees = 
               [BinTree {value = 20, trees = [], deep = 0}], deep = 1}], deep = 2}], deep = 3},BinTree {value = 21, trees = 
               [BinTree {value = 22, trees = [], deep = 0},BinTree {value = 23, trees = 
               [BinTree {value = 24, trees = [], deep = 0}], deep = 1},BinTree {value = 25, trees = 
               [BinTree {value = 26, trees = [], deep = 0},BinTree {value = 27, trees = 
               [BinTree {value = 28, trees = [], deep = 0}], deep = 1}], deep = 2},BinTree {value = 29, trees = 
               [BinTree {value = 30, trees = [], deep = 0},BinTree {value = 31, trees = 
               [BinTree {value = 32, trees = [], deep = 0}], deep = 1},BinTree {value = 33, trees = 
               [BinTree {value = 34, trees = [], deep = 0},BinTree {value = 35, trees = 
               [BinTree {value = 36, trees = [], deep = 0}], deep = 1}], deep = 2}], deep = 3}], deep = 4}], deep = 5},BinTree {value = 37, trees = 
               [BinTree {value = 38, trees = [], deep = 0},BinTree {value = 39, trees = 
               [BinTree {value = 40, trees = [], deep = 0}], deep = 1},BinTree {value = 41, trees = 
               [BinTree {value = 42, trees = [], deep = 0},BinTree {value = 43, trees = 
               [BinTree {value = 44, trees = [], deep = 0}], deep = 1}], deep = 2},BinTree {value = 45, trees = 
               [BinTree {value = 46, trees = [], deep = 0},BinTree {value = 47, trees = 
               [BinTree {value = 48, trees = [], deep = 0}], deep = 1},BinTree {value = 49, trees = 
               [BinTree {value = 50, trees = [], deep = 0},BinTree {value = 51, trees = 
               [BinTree {value = 52, trees = [], deep = 0}], deep = 1}], deep = 2}], deep = 3},BinTree {value = 53, trees = 
               [BinTree {value = 54, trees = [], deep = 0},BinTree {value = 55, trees = 
               [BinTree {value = 56, trees = [], deep = 0}], deep = 1},BinTree {value = 57, trees =
               [BinTree {value = 58, trees = [], deep = 0},BinTree {value = 59, trees = 
               [BinTree {value = 60, trees = [], deep = 0}], deep = 1}], deep = 2},BinTree {value = 61, trees = 
               [BinTree {value = 62, trees = [], deep = 0},BinTree {value = 63, trees = 
               [BinTree {value = 64, trees = [], deep = 0}], deep = 1},BinTree {value = 65, trees = 
               [BinTree {value = 66, trees = [], deep = 0},BinTree {value = 67, trees = 
               [BinTree {value = 68, trees = [], deep = 0}], deep = 1}], deep = 2}], deep = 3}], deep = 4},BinTree {value = 69, trees = 
               [BinTree {value = 70, trees = [], deep = 0},BinTree {value = 71, trees = 
               [BinTree {value = 72, trees = [], deep = 0}], deep = 1},BinTree {value = 73, trees = 
               [BinTree {value = 74, trees = [], deep = 0},BinTree {value = 75, trees = 
               [BinTree {value = 76, trees = [], deep = 0}], deep = 1}], deep = 2},BinTree {value = 77, trees = 
               [BinTree {value = 78, trees = [], deep = 0},BinTree {value = 79, trees = 
               [BinTree {value = 80, trees = [], deep = 0}], deep = 1},BinTree {value = 81, trees = 
               [BinTree {value = 82, trees = [], deep = 0},BinTree {value = 83, trees = 
               [BinTree {value = 84, trees = [], deep = 0}], deep = 1}], deep = 2}], deep = 3},BinTree {value = 85, trees = 
               [BinTree {value = 86, trees = [], deep = 0},BinTree {value = 87, trees = 
               [BinTree {value = 88, trees = [], deep = 0}], deep = 1},BinTree {value = 89, trees = 
               [BinTree {value = 90, trees = [], deep = 0},BinTree {value = 91, trees = [BinTree {value = 92, trees = [], deep = 0}], deep = 1}], deep = 2},BinTree {value = 93, trees = 
               [BinTree {value = 94, trees = [], deep = 0},BinTree {value = 95, trees = 
               [BinTree {value = 96, trees = [], deep = 0}], deep = 1},BinTree {value = 97, trees = 
               [BinTree {value = 98, trees = [], deep = 0},BinTree {value = 99, trees = 
               [BinTree {value = 100, trees = [], deep = 0}], deep = 1}], deep = 2}], deep = 3}], deep = 4}], deep = 5}], deep = 6}]
            ]



