main :: IO ()
main = do

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
