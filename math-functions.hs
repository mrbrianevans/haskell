
-- Increment function
increment :: Int -> Int
increment x = x + 1

-- Decrement function
decrement :: Int -> Int
decrement x = x - 1

-- Addition function
add :: Int -> Int -> Int
add a 0 = a
add a 1 = increment a
add 1 b = increment b
add a b = increment (add b (decrement a))

-- TODO: Subtraction function

-- Multiplication function
mult :: Int -> Int -> Int
mult a 0 = 0
mult a b = add a c where c = mult a d where d = decrement b

-- Exponent function x^n or x**n
power :: Int -> Int -> Int
power x 0 = 1
power x n = mult x y where y = power x (decrement n)

-- Exponent function using guards
powerGuard :: Int -> Int -> Int
powerGuard x n
    | n == 0 = 1
    | otherwise = x * powerGuard x (decrement n)

-- TODO: Power Tower function
