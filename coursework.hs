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
rule2 (a, b, c, d, e, f) = (((a+c+e) `mod` 2) /= ((b+d+f) `mod` 2)) && (((a+c) `mod` 2) + ((b+d) `mod` 2) == 0)
