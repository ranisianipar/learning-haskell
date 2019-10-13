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

-- no. 4: sum of the squares of the natural numbers 1 to n, using map and foldr
sumOfSquares n = foldr (\p q -> p + q) 0 (map square [1..n])
sumOfSquares2 n = foldr (+) 0 (map square [1..n])
square x = x*x

-- Mystery function just put xs element in new array and put the [5,6] (in param) to the right of xs
-- this works vice versa with the foldl
mystery xs = foldl (++) [5,6] (map sing xs) where
    sing x = [x]

-- no. 5: polymorphic identity function
polymorphicId x = x
-- x can be anything, that's why called polymorphic

-- greaterThan10 :: Int -> Bool
greaterThan10 x = if x > 10 
    then True
    else False

-- question: If f is of type Int -> Bool, at what instance of its most general type a ->
--           a is id used in each case?

-- no. 6: composeList
-- compose list of function
composeMe x y = x . y
composeList (x:xs) = foldl composeMe x xs
composeList [] = polymorphicId

-- function will be composed from leftest element, index move to the right (side)
composeList2 (x:xs) = foldr (\x y -> x . y) x xs
composeList2 [] = polymorphicId

-- test case: `composeList [succ, polymorphicId, square] 1` --> return 2
-- test case: `composeList2 [succ, polymorphicId, square] 1` --> return 4

-- no. 7: flip
flipMe f a b = f b a
