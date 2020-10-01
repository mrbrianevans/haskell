-- Length of a list
len :: [a] -> Int
len [] = 0
len (x:xs) = 1 + len xs

-- Check if a list contains an item
contains :: (Eq a) => [a] -> a -> Bool
contains [] y = False
contains (x:xs) y = x == y || xs `contains` y

-- returns the largest integer in a list
largest :: [Int] -> Int
largest [x] = x
largest [x, y]
              | x>y = x
              | otherwise = y
largest (x:xs) = largest [x, largest xs]

-- zips a pair of lists together into a list of pairs
zipped :: ([a], [b]) -> [(a, b)]
zipped ([x], [y]) = [(x, y)]
zipped ((x:xs), (y:ys)) = zipped ([x], [y]) ++ zipped (xs, ys)

-- insert an integer into a sorted list
insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x [y] | x > y = [y, x] | otherwise = [x, y]
insert x (y:z:zs)
                 | y<x && x<z = y : x : z : zs
                 | x<y = x : y : z : zs
                 | otherwise = y : insert x ys where ys = z : zs

-- sort a list of integers using insertion sort
sort :: [Int] -> [Int]
sort [] = []
sort [x] = [x]
sort (x:xs) = insert x (sort xs)
