import           Data.Char

main :: IO ()
main = do
  print (fn 0.5)
  print (fn (-2))
  where fn = maximize [(\x -> x*x*x),(\x -> x+1)]

xs :: [Int]
xs = [7, 8, 9]

isPrime :: Integer -> Bool
isPrime n = [x | x <- [1 .. n], mod n x == 0] == [1, n]

primesInRange :: Integer -> Integer -> [Integer]
primesInRange a b = [x | x <- [a .. b], isPrime x]

sumDiv :: Integer -> Integer
sumDiv a = (sum [d | d <- [1 .. a], mod a d == 0])

prodSumDiv :: [Integer] -> Integer -> Integer
prodSumDiv xs k = (product [d | d <- xs, mod (sumDiv d) k == 0])

isSorted :: [Int] -> Bool
isSorted xs
  | xs == [] = True
  | tail xs == [] = True
  | (head xs) <= (head (tail xs)) = isSorted (tail xs)
  | otherwise = False

isSorted' :: [Int] -> Bool
isSorted' []         = True
isSorted' [_]        = True
isSorted' (x1:x2:xs) = x1 < x2 && isSorted' (x2 : xs)

isSorted'' :: [Int] -> Bool
isSorted'' xs = and [a < b | (a, b) <- zip xs (tail xs)]

--insert in ordered list of integers
insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (y:ys)
  | x <= y = x : y : ys
  | otherwise = y : insert x ys

insert' :: Int -> [Int] -> [Int]
insert' a [] = [a]
insert' a (x:xs) =
  if a <= x
    then a : x : xs
    else x : insert' a xs

insert'' :: Int -> [Int] -> [Int]
insert'' k xs = [x | x <- xs, x < k] ++ k : [x | x <- xs, x >= k]

insert''' :: Int -> [Int] -> [Int]
insert''' k [] = [k]
insert''' k (x:xs) =
  if k < x
    then x : k : xs
    else x : insert''' k xs

merge :: [Int] -> [Int] -> [Int]
merge [] [] = []
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x < y = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys

insertionSort :: [Int] -> [Int]
insertionSort = foldr insert []


maximize :: (Ord a, Num a) => [(a -> a)] -> (a -> a)
maximize fs = \ x -> snd (maximum [(abs fx, fx) | fx <- map ($ x) fs])

