import Data.Char

main :: IO()
main = do 
    print(isCapitalized "ABVC123")
    print(isCapitalized "ABsC123")

incrementAllByMap :: [Int] -> Int -> [Int]
incrementAllByMap xs n = map (\x -> x + n) xs

incrementAllBy :: [Int] -> Int -> [Int]
incrementAllBy xs n = if xs == [] then [] 
    else (head xs + n) : incrementAllBy (tail xs) n

multiplyAllBy :: [Int] -> Int -> [Int]
multiplyAllBy xs n = 
    if xs == [] then []
    else (head xs * n) : multiplyAllBy (tail xs) n

filterSmallerThan :: [Int] -> Int -> [Int]
filterSmallerThan xs n 
    | xs == [] = []
    | head xs < n = filterSmallerThan (tail xs) n
    | otherwise = head xs : filterSmallerThan (tail xs) n

intoList :: Integer -> [Integer]
intoList n = 
    if n == 0 then [] 
    else n `mod` 10 : intoList (n `div` 10)

isAscending :: Integer -> Bool
isAscending x = helper (reverse (intoList x))
    where
        helper xs 
            | xs == [] = True
            | (tail xs) == [] = True
            | (head xs) > (head (tail xs)) = False
            | otherwise = helper (tail xs)

digits :: String -> String
digits xs 
    | xs == [] = []
    | isDigit (head xs) = head xs : digits (tail xs)
    | otherwise = digits (tail xs) 


digitsSum :: String -> Int
digitsSum str = sum [ord c - ord '0' | c<- str, isDigit c]

capitalize :: String -> String
capitalize str = [toUpper c | c<-str]

isCapitalized :: String -> Bool
isCapitalized str = str == capitalize str
    