module Chs67 where

--don't delete the import, obviously
--remember to include function types (3 points each)

sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n - 1)

euclid :: Int -> Int -> Int
euclid x y
    | x == y    = x
    | x < y     = euclid x (y - x)
    | otherwise = euclid (x - y) y

sum' :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum xs

take' :: Int -> [a] -> [a]
take' 0 _      = []
take' _ []     = []
take' n (x:xs) = x : take (n - 1) xs

last' :: [a] -> a
last' [x]    = x
last' (_:xs) = last xs

dec2int' :: [Int] -> Int
dec2int' = foldl (\x y -> 10 * x + y) 0

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g [] = []
altMap f g [x] = [f x]
altMap f g (x:y:xs) = f x : g y : altMap f g xs

--define your own function(s) to help with luhn

luhnDouble :: Int -> Int
luhnDouble n
    | 2 * n > 9 = 2 * n - 9
    | otherwise = 2 * n


luhn :: [Int] -> Bool
luhn xs =
    sum (altMap id luhnDouble (reverse xs)) `mod` 10 == 0
