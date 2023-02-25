import Data.Char
import Data.List hiding (isPrefixOf, isInfixOf)

main :: IO ()
main = do
  print ((repeater "abc") 5 " - ")
  print (longestSubstring "aaabccdeef")
  print (longestSubstringStr "aaabbbbccdeef")
  print (longestSubstringStr "aaabccdeeef")
  print (tighten "   abc   de f   g  ")

isImage :: [Int] -> [Int] -> Bool
isImage [] [] = True
isImage [a] [b] = True
isImage (x:xs) (y:ys)
  | x - y == head xs - head ys = isImage xs ys
  | otherwise = False

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

switchsum :: Num a => (a -> a) -> (a -> a) -> Int -> (a -> a)
switchsum f g n x = helper n 0 f f g
  where
    helper 1 sum h _ _   = sum + h x
    helper n sum h f1 g1 = helper (n - 1) (sum + h x) (g1 . h) g1 f1

repeater :: String -> (Int -> String -> String)
repeater str = helper
  where
    helper count glue = concatMap (++ glue) (replicate (count - 1) str) ++ str

nOrMoreVowels :: [String] -> Int -> [String]
nOrMoreVowels xs n = [ys | ys <- xs, length (filter isVowel ys) >= n]
  where
    isVowel c = elem (toLower c) "aeiouy"

isTriangular :: [[Int]] -> Bool
isTriangular xs = helper xs 0
  where
    helper [] _     = True
    helper (x:xs) n = all (== 0) (take n x) && helper xs (n + 1)

isPrefixOf :: String -> String -> Bool
isPrefixOf str1 str2 = (take (length str1) str2) == str1

isInfixOf :: String -> String -> Bool
isInfixOf _ "" = False
isInfixOf xs ys = isPrefixOf xs ys || isInfixOf xs (tail ys)

longestSubstring :: String -> Int
longestSubstring = maximum . (map length) . group

longestSubstringStr :: String -> String
longestSubstringStr = snd . maximum . (map (\ cs -> (length cs, cs))) . group

tighten :: String -> String
tighten = unwords . words

