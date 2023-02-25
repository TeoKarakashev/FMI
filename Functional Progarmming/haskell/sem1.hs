main :: IO()
main = do
  print(countPalis 10 20)

mymin :: Int -> Int -> Int
mymin a b = if a < b then a else b

isInside :: Int -> Int -> Int -> Bool 
isInside x a b = if a <= x && x <= b then True else False 

myfunc :: Double -> Double -> Double
myfunc a b = (a ^ 2 + b ^ 2) / 2

myfib :: Int -> Int
myfib a = if a <= 1 then 1 else myfib(a - 1) + myfib(a - 2)

myfibIter :: Integer -> Integer
myfibIter n = helper 0 0 1
  where
    helper i prev cur =
      if i == n then cur
      else helper (i + 1) cur (prev + cur)

mygcd :: Integer -> Integer -> Integer
mygcd a b = 
  if a `mod` b == 0 then b else mygcd b (a `mod` b)
  
mymaxdivisor :: Integer ->Integer  
mymaxdivisor x = helper (x - 1)
  where
    helper iter = if x `mod` iter == 0 then iter else helper (iter - 1)

sumodd :: Integer -> Integer -> Integer
sumodd a b = helper 0 a
  where 
    helper sum iter = 
      if iter > b then sum 
      else 
        if iter `mod` 2 == 1 then helper (sum + iter) (iter + 1)
        else helper sum (iter + 1)




isPrime :: Integer -> Bool
isPrime n 
  | n == 1 = False
  | n == 2 = True
  | otherwise = helper 2
  where 
    helper iter 
      | iter == n = True
      | n `mod` iter == 0 = False
      | otherwise = helper (iter + 1) 

reverseNum :: Integer -> Integer
reverseNum n = helper n 0
  where
    helper k res =
      if k == 0 then res 
      else helper (k `div` 10) ((res * 10) + k `mod` 10)


isPali :: Integer -> Bool
isPali n = n == reverseNum n

countPalis:: Integer -> Integer -> Integer
countPalis a b 
  | a > b = 0
  | isPali a = 1 + countPalis (a + 1) b 
  | otherwise = countPalis (a + 1) b