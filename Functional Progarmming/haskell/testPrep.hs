

main :: IO ()
main = do
    print(fn 0.5)

--read ((show x) ++ (show y))

reverseNum:: Int -> Int
reverseNum n = helper n 0
    where
        helper 0 res = res
        helper n res = helper (div n 10) ((res * 10) + (mod n 10))

squareDigits :: Int -> Int
squareDigits n = if n < 0 then (-1 * helper (reverseNum (-1 * n)) 0) 
    else helper (reverseNum n) 0 
    where
        helper 0 res = res
        helper n res = 
            if res == 0 then helper (div n 10) ((mod n 10) * (mod n 10))
            else helper (div n 10) (read ((show res) ++ (show ((mod n 10) * (mod n 10))))) 

            


data BTree = Empty | Node Int BTree BTree deriving (Show, Eq, Ord)

matching :: String -> [(Int, Int)]
matching s = helper s 0 [] []
    where
        helper [] _ _ res = res
        helper (x:xs) i stack res  
            | x == '[' = helper xs (i + 1) (i:stack) res
            | x == ']' = helper xs (i + 1) (tail stack) ((head stack, i):res)
            | otherwise = helper xs (i + 1) stack res
            

findNb :: Integer -> Integer
findNb m = helper 1 0
    where 
        helper n sum
            | m == sum = n
            | m < sum = (-1)
            |otherwise = helper (n + 1) (sum + n ^ 3)


isBinarySearchTree :: BTree -> Bool
isBinarySearchTree Empty = True
isBinarySearchTree bt = helper bt (minBound :: Int) (maxBound :: Int)
    where 
        helper Empty _ _ = True
        helper (Node a lt rt) min max = 
            if a < min || a > max then False
            else helper lt min a && helper rt a max


type Point = (Double,Double) 

splitPoints :: Point -> Double -> [Point] -> ([Point],[Point])
splitPoints start radius points = helper start radius points [] []
    where 
        helper _ _ [] inC outC = (inC, outC)
        helper (x, y) r ((a, b):xs) inC outC =
            if (a - x) ^ 2 + (b - y) ^ 2 <= r ^ 2 then helper (x,y) r xs ([(a, b)] ++ inC) outC 
            else helper (x,y) r xs inC ([(a, b)] ++ outC)


dominates :: (Int -> Int) -> (Int -> Int) -> [Int] -> Bool
dominates f g [] = True
dominates f g (x:xs) = if f x >= g x then dominates f g xs else False 


data Stock = Stock String Int

stocks = [Stock "ABAR" 200, Stock "CDXE" 500, Stock "BKWR" 250, Stock "BTSQ" 890, Stock "DRTY" 600]


sumStocks :: [Stock] -> Char -> Int -> Int
sumStocks [] _ res = res
sumStocks ((Stock (x:xs) n):ys) c res = if x == c then sumStocks ys c (res + n) else sumStocks ys c res

stocklist :: [Stock] -> [Char] -> [(Char, Int)]
stocklist stocks categories = helper stocks categories []
    where 
        helper _ [] res = res
        helper stocks (x:xs) res = helper stocks xs (res ++ [(x, sumStocks stocks x 0)]) 



isAsc :: [Int] -> Bool
isAsc [] = True
isAsc [x] = True
isAsc (x:xs) = if x <= head xs then isAsc xs else False

--create a function that finds unique numbers in list

remove :: Int -> [Int] -> [Int]
remove x xs = helper x xs []
    where 
        helper _ [] res = res
        helper x (y:ys) res = if x == y then helper x ys res else helper x ys (res ++ [y])

findUnique :: [Int] -> [Int]
findUnique [] = []
findUnique (x:xs) = if x `elem` xs then findUnique (remove x xs)
 else [x] ++ findUnique xs



sumUnique :: [[Int]] -> Int
sumUnique xs = helper xs 0
    where 
        helper [] res = res
        helper (x:xs) res = helper xs (res + sum (findUnique x))


type Product = (String,Double)
type StoreAvailability = [Product]


store1 :: StoreAvailability
store1=[("bread",1),("milk",2.5),("lamb",10),("cheese",5),("butter",2.3)]


store2=[("bread",1),("cheese",2.5),("bread",1),("cheese",5),("butter",2.3)]


getAverage :: StoreAvailability -> Double
getAverage [] = 0
getAverage xs = helper xs 0 0
    where 
        helper [] sum count = sum / count
        helper ((_,x):xs) sum count = helper xs (sum + x) (count + 1)



closestToAverage :: StoreAvailability -> String
closestToAverage [] = ""
closestToAverage (x:xs) = helper xs x (getAverage (x:xs))
    where 
        helper [] (x,_) _ = x
        helper ((x, y):xs) (a, b) avg = if (abs (y - avg)) < (abs (b - avg)) then helper xs (x, y) avg 
        else helper xs (a, b) avg


-- for a given x
-- returns f(x) of the function list that f(x) has the biggest absolute value
--instaed of 0 use the given x
maximize :: (Ord a, Num a) => [(a -> a)] -> (a -> a)
maximize [] = (\x -> 0)
maximize (x:xs) = helper xs x
    where 
        helper [] res = res
        helper (x:xs) res = if (abs (x 0)) > (abs (res 0)) then helper xs x else helper xs res
fn = maximize [(\x -> x*x*x),(\x -> x+1)]