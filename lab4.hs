
addtolist ::  Num a =>  a -> [[a]] -> [[a]]
addtolist _ []= []
addtolist a (x:xs) = [[a]++x]++addtolist a xs

powerset :: Num a => [a] -> [[a]]
powerset []=[[]]
powerset (x:xs)=  [[x]] ++ powerset xs ++ addtolist  x [xs]
