main :: IO ()
main = do
  print (generate [10, 15, 25] [1, 5, 20])


generateAllListsWithStart :: Int -> Int -> [Int] -> [Int] -> [[Int]]
generateAllListsWithStart a b as bs = helper [a, b] as bs [a, b]
  where
    helper start [] _ result = [result]
    helper start _ [] result = [result]
    helper start (a:as) (b:bs) result 
      | a < b && a > (last start) = helper start as bs (result ++ (start ++ [a, b]))
      | a < b && a < (last start) = helper start as (b:bs) result
      | a > b && a > (last start) = helper start (a:as) bs result 
      | otherwise = helper start as bs result

generate :: [Int] -> [Int] -> [[Int]]
generate [] _ = []
generate _ [] = []
generate as bs = helper as bs []
  where
    helper [] _ result = result
    helper (a:as) [] result = helper as bs result
    helper (a:as) (b:bs) result =
      if a < b
        then helper (a : as) bs (result ++ generateAllListsWithStart a b as bs)
        else helper (a : as) bs result


wordLengthInList :: Int -> Bool
wordLengthInList n = n == 2 || n == 3 || n == 4 || n == 7

countWhileNewLine :: String -> Int
countWhileNewLine text = helper text 0 0
  where
    helper [] count currentWordLength =
      if wordLengthInList currentWordLength
        then (count + 1)
        else count
    helper (x:xs) count currentWordLength
      | x == '\n' =
        if wordLengthInList currentWordLength
          then (count + 1)
          else count
      | x == ' ' =
        if wordLengthInList currentWordLength
          then helper xs (count + 1) 0
          else helper xs count 0
      | otherwise = helper xs count (currentWordLength + 1)

countUniques :: String -> Int
countUniques text = helper text 0
  where
    helper [] count = count
    helper (x:xs) count
      | x == '|' = helper xs (count + countWhileNewLine xs)
      | otherwise = helper xs count
