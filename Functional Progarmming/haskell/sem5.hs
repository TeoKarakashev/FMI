import           Data.Char
import           Data.List hiding (isInfixOf, isPrefixOf)

main :: IO ()
main = do
  print (inorder t2)

data Shape
  = Circle Float
  | Rectangle Float Float
  | Triangle Float Float Float
  deriving (Eq, Ord) --SHow

ss :: [Shape]
ss = [Circle 5, Rectangle 3 4, Triangle 3 4 5]

s1 :: Shape
s1 = Circle 5

s2 :: Shape
s2 = Rectangle 3 4

t1 :: BTree 
t1 = Node 5 
    (Node 2 Empty (Node 3 Empty Empty)) 
    (Node 6 Empty Empty) 

t2 :: BTree                                                       

t2 = Node 5 
    (Node 3 Empty Empty)                  
    (Node 4 
        (Node 5 Empty Empty)
        (Node 7 Empty Empty)) 

instance Show Shape where
  show (Triangle a b c) =
    "Triangle " ++ show a ++ " " ++ show b ++ " " ++ show c
  show (Circle r) = "Circle " ++ show r
  show (Rectangle a b) = "Rectangle " ++ show a ++ " " ++ show b

perimeter :: Shape -> Float
perimeter (Circle r)       = 2 * pi * r
perimeter (Rectangle a b)  = 2 * (a + b)
perimeter (Triangle a b c) = a + b + c

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle a b) = a * b
area (Triangle a b c) = sqrt (p * (p - a) * (p - b) * (p - c))
  where
    p = perimeter (Triangle a b c) / 2

sumArea :: [Shape] -> Float
sumArea []     = 0
sumArea (s:ss) = area s + sumArea ss

biggestShape :: [Shape] -> Shape
biggestShape [] = error "Empty list"
biggestShape [s] = s
biggestShape (s:ss) =
  if area s > area (biggestShape ss)
    then s
    else biggestShape ss

data BTree
  = Empty
  | Node Int BTree BTree
  deriving (Show, Eq, Ord)

size :: BTree -> Int
size Empty          = 0
size (Node _ lt rt) = 1 + size lt + size rt

height :: BTree -> Int 
height Empty = 0
height (Node _ lt rt) = 1 + max (height lt) (height rt)


sumTree :: BTree -> Int
sumTree Empty = 0
sumTree (Node x lt rt) = x + sumTree lt + sumTree rt

sumLeaves :: BTree -> Int
sumLeaves Empty = 0
sumLeaves (Node x Empty Empty) = x
sumLeaves (Node _ lt rt) = sumLeaves lt + sumLeaves rt


inorder :: BTree -> [Int]
inorder Empty = []
inorder (Node x lt rt) = inorder lt ++ [x] ++ inorder rt

