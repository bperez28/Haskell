--1.)
addtwo :: Int -> Int -> Int
addtwo x y = x+y

addingby4 = addtwo 4

--2.)
lower :: Char -> Bool
lower = (`elem` ['a'..'z'])

--3.)
zipWithf :: (a -> s -> d -> f) -> [a] -> [s] -> [d] -> [f]
zipWithf _ [] _ _ = []
zipWithf _ _ [] _ = []
zipWithf _ _ _ [] = []
zipWithf w (x:xs) (y:ys) (z:zs) = w x y z : zipWithf w xs ys zs

--4.)
tooTuple :: (Int -> Int) -> Int -> (Int , Int)
tooTuple w x = (x, w x)

--5.)
mapf :: [Int] -> (Int -> Int) -> [(Int , Int)]
mapf x w = map (\e -> tooTuple w e) x
