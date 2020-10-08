-- Returns True if all numbers are unique in the tuple
rule1 :: (Int, Int, Int, Int, Int, Int) -> Bool
rule1 (a, b, c, d, e, f) = uniqueList1 [a, b, c, d, e, f] == False

-- HELPER FUNCTION FOR RULE1
-- returns False if items are unique
uniqueList1 :: [Int] -> Bool
uniqueList1 [] = False
uniqueList1 (x:xs) = x `isIn` xs || uniqueList1 xs

-- HELPER FUNCTION FOR RULE1
-- returns False if item not in list
isIn :: Int -> [Int] -> Bool
isIn x [] = False
isIn x (y:ys) = x==y || x `isIn` ys


-- Returns true if numbers alternate between odds and evens
-- The way it works is by checking that the sum of a, c, e is not the same evenness as the sum of b, d, f
-- A final check is that a + c is even and b + d is even
rule2 :: (Int, Int, Int, Int, Int, Int) -> Bool
rule2 (a, b, c, d, e, f) = (((a+c+e) `mod` 2) /= ((b+d+f) `mod` 2))
                        && (((a+c) `mod` 2) + ((b+d) `mod` 2) == 0)

-- Returns True if alternate digits differ by more than two
rule3 :: (Int, Int, Int, Int, Int, Int) -> Bool
rule3 (a, b, c, d, e, f) = listRule3 [a, b, c, d, e, f]

listRule3 :: [Int] -> Bool
listRule3 [x] = True
listRule3 (x:y:ys) = x `differsByMoreThanTwo` y && listRule3 (y:ys)

differsByMoreThanTwo :: Int -> Int -> Bool
differsByMoreThanTwo x y = abs (x-y) > 2

-- returns True if the first two pairs of digits are multiples of the last pair
rule4 :: (Int, Int, Int, Int, Int, Int) -> Bool
rule4 (a, b, c, d, 0, 0) = False
rule4 (a, b, c, d, e, f) = (((10*a+b)`mod`g)+((10*c+d)`mod`g))==0 where g = 10*e+f

-- returns all possible tuples from (0,0,0,0,0,0) to (9,9,9,9,9,9)
possibles :: [(Int, Int, Int, Int, Int, Int)]
possibles = map splitter [0..999999]

-- takes a number and splits it into 6 columns of a tuple
splitter :: Int -> (Int, Int, Int, Int, Int, Int)
splitter x = convertToTuple $ splitList $ show x

convertToTuple :: [Int] -> (Int, Int, Int, Int, Int, Int)
convertToTuple [a, b, c, d, e, f] = (a, b, c, d, e, f)
convertToTuple (x:xs) = convertToTuple (0 : x : xs)

splitList :: [Char] -> [Int]
splitList [] = []
splitList (x:xs) = (ord x - ord '0') : splitList xs

-- returns true if a tuple matches the 4 criteria to be a valid solution
isSolution :: (Int, Int, Int, Int, Int, Int) -> Bool
isSolution (a, b, c, d, e, f) = rule3 (a, b, c, d, e, f)
                              && rule2 (a, b, c, d, e, f)
                              && rule1 (a, b, c, d, e, f)
                              && rule4 (a, b, c, d, e, f)

allSolutions = filter isSolution possibles

-- returns the ASCII code for number characters
ord :: Char -> Int
ord '0' = 48
ord '1' = 49
ord '2' = 50
ord '3' = 51
ord '4' = 52
ord '5' = 53
ord '6' = 54
ord '7' = 55
ord '8' = 56
ord '9' = 57



---------------------------------------------------------------



-- QUESTION 2
pretty :: [[String]] -> String
pretty [[]] = ""
pretty [(x:xs)] = x ++ "\n" ++ (pretty [xs])
pretty (x:xs) = pretty [x] ++ (pretty xs)

type Point = (Int, Int)

glider :: [Point]
glider = [(2,2), (1,3), (2,1), (0,2), (2,3)]

--visualisation
visualisation :: Int -> Int -> [[Point]] -> [[String]]
visualisation x y [] = []
visualisation x y (pnt:pnts) = oneGrid x y pnt 0 : visualisation x y pnts

oneGrid :: Int -> Int -> [Point] -> Int -> [String]
oneGrid x y [] r = []
oneGrid x y (pnts) r
  | r>y = []
  | otherwise =
    oneRow (sort [fst z | z <- pnts, snd z == r]) x : oneGrid x y pnts (r+1)

-- Takes a sorted list of ints for a single line, needs at least 1 item
oneRow :: [Int] -> Int -> String
oneRow [] a = replicate (a+1) '.'
oneRow (x:xs) a = replicate x '.' ++ "#" ++ oneRow [y-x-1|y<-xs] (a-x-1)

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

evolution :: [Point] -> [[Point]]
evolution (points) = iterate evolve points

-- this might need a custom filter function
evolve :: [Point] -> [Point]
evolve (points) = filter (isAlive points) (fillGrid 5 5)

-- returns all points in a grid of size x, y
fillGrid :: Int -> Int -> [Point]
fillGrid x y = [(a,b)|a<-[0..x],b<-[0..y]]

isAlive :: [Point] -> Point -> Bool
isAlive (points) point
  | points `contains` point = {- CELL WAS ALIVE -}
    (tps == 2 || tps == 3)
  | otherwise = {- CELL WAS DEAD -}
    tps == 3
    where tps = touching points point
contains :: [Point] -> Point -> Bool
contains [] p = False
contains (point:points) p = point==p || contains points p
-- returns the number of alive cells touching a specific cell
touching :: [Point] -> Point -> Int
touching (points) point = length (filter nextTo points)
    where nextTo p = p == (fst point-1, snd point-1)
                  || p == (fst point, snd point-1)
                  || p == (fst point+1, snd point-1)
                  || p == (fst point-1, snd point)
                  || p == (fst point+1, snd point)
                  || p == (fst point-1, snd point+1)
                  || p == (fst point, snd point+1)
                  || p == (fst point+1, snd point+1)
