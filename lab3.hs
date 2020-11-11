--1
sum_three :: Int -> Int -> Int -> Int
sum_three x y z = x + y + z

add_strings :: String -> String -> String
add_strings c s = c ++ s

removeLowerCase :: [Char] -> [Char]
removeLowerCase s = [c | c <- s, c `elem` ['A'..'Z']]
--2
add_two :: (Num a) => a -> a -> a
add_two x y = x+y

--3
num_string_ref :: Int -> String
num_string_ref 1 = "once"
num_string_ref 2 = "Twice"
num_string_ref 3 = "Thrice"
num_string_ref x = "Don't know how to say that in English."


--4
powerTo' :: Int -> Int  -> Int
powerTo' x 0 =1
powerTo' x n = x* powerTo' x (n-1)


--5
sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:[])=x
sum' (w:x:y:z:_)= w*x*y;
sum' (x:xs) = x * sum' xs
