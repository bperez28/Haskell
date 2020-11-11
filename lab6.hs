import Data.List
--1.)
tupleProduct :: (Num a) => [(a,a)] -> [a]
tupleProduct w = map (\(b,c) -> b*c) w

--2.)
listSum :: [Int] -> Int
listSum w = last $ scanl (+) 0 w

--3.)
applyThrice :: (Int -> Int) -> Int -> Int
applyThrice x y = x$x$x y


--4.)
isLowerCase :: Char -> Bool
isLowerCase = (`elem` ['a' .. 'z'])

--5.)
--sortedList :: [Char] -> [Char]
sortedList x = split $ quicksort x


quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [a | a <- xs, a <= x] ++ [x] ++ quicksort [a | a <- xs, a > x]

split :: String -> [String]
split [] = [""]
split (x:xs)
             | x /= head xs =  "" : rest
             | otherwise = (x : head rest) : tail rest
    where rest = split xs
