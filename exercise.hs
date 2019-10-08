-- no. 1
returnOne a = 1
getLength xs = sum (map returnOne xs)

-- no. 2
-- ans: using composition
composition f g xs = map (f . g) xs

-- factorial
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial(n-1)

-- no. 3
iter :: Int -> (a -> a) -> a -> a
iter 0 f x = x
iter n f x = (f . iter (n - 1) f) x

-- helper method
coba :: Int -> Int
coba a = a + 7

-- no. 4
--  foldr itu apa???
mystery xs = foldr (++) [] (map sing xs) where
    sing x = [x]
