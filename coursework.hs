
import Data.Char (ord)

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
isSolution (a, b, c, d, e, f) = rule1 (a, b, c, d, e, f)
                              && rule2 (a, b, c, d, e, f)
                              && rule3 (a, b, c, d, e, f)
                              && rule4 (a, b, c, d, e, f)

allSolutions = filter isSolution possibles
