module CW4 where


data Tree a = Tree { valkey :: a, leftTree :: Tree a, rightTree :: Tree a } | Empty
    deriving (Show)

infixr 9 &
(&) f x = x f

type Folder a b = a -> b -> b

insert :: Ord a => a -> Tree a -> Tree a
insert elem Empty = Tree { valkey = elem, leftTree = Empty, rightTree = Empty }
insert elem Tree { valkey = val, leftTree = left, rightTree = right } 
    | elem < val = Tree { valkey = val, leftTree = insert elem left, rightTree = right }   
    | otherwise = Tree { valkey = val, leftTree = left, rightTree = insert elem right }
                    
create :: Ord a => [a] -> Tree a 
create list = foldr insert Empty list

flatTreeL :: Tree a -> [a]
flatTreeL Empty = []
flatTreeL Tree { valkey = val, leftTree = left, rightTree = right } = flatTreeL left ++ [val] ++ flatTreeL right

flatTreeR :: Tree a -> [a]
flatTreeR Empty = []
flatTreeR Tree { valkey = val, leftTree = left, rightTree = right } = flatTreeR right ++ [val] ++ flatTreeR left

treeFoldR :: Folder a b -> b -> Tree a -> b
--гряхный хак
--treeFoldR f acc tree = foldr f acc $ flatTreeR tree  
treeFoldR _ acc Empty = acc
treeFoldR f acc Tree { valkey = val, leftTree = left, rightTree = right } = flip (treeFoldR f) left $ flip (treeFoldR f) right $ f val acc 

emptyTree :: Ord a => Tree a
emptyTree = Empty

simpleTree :: Tree Integer
simpleTree = Tree 10 Empty Empty

notSimpleTree :: Tree Integer
notSimpleTree = Tree 10 Empty $ Tree 15 Empty Empty


