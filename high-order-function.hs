-- Exercises from chapter 9-10 of The Craft of Functional Programming

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



-- LIST COMPREHENSION

-- LIST COMPRE to HIGH ORDER

-- no. 1: [ x + 1 | x <- xs ]
list1 xs = map succ xs
list1A xs = map (+1) xs

-- no. 2: [ x + y | x <- xs, y <- ys ]
-- HOW TO IMPLEMENT THIS WITH MAP???
list2 (x:xs) (y:ys) = (x + y) : list2 xs ys
list2 [] [] = []
list2 (x:xs) [] = xs
list2 [] (y:ys) = ys

-- with helper function
addThem (i,j) =  i + j
list2A xs ys = map addThem (zip xs ys)

-- no helper function
list2B xs ys = map (\(i,j) -> i + j) (zip xs ys)

-- implement zipWith
list2C f xs ys = map (\(i,j) -> f i j) (zip xs ys)

-- no. 3: [ x + 2 | x <- xs, x > 3 ]
gt3 x = x > 3 
list3 xs = map (\a -> a + 2) (filter gt3 xs)

-- no helper function
list3A xs = map (\a -> a + 2) (
    filter (\a -> a > 3) xs 
    )

-- no. 4: [ x + 3 | (x, _) <- xys ]
list4 xys = map (\(i, _) -> i + 3) xys

-- with helper func.
firstPlus3 (i, _) = i + 3
list4A xys = map firstPlus3 xys


-- no. 5: [ x+4 | (x,y) <- xys, x+y < 5 ]
list5 xys = map (\(i,j) -> i + 4) (filter plusGt5 xys)

-- helper func.
plusGt5 (i,j) = (i + j) < 5

haha mxs = [ x+5 | Just x <- mxs ]

-- no. 6: [ x+5 | Just x <- mxs ]
-- test case: mxs = [Nothing, Just 1, Just 2]
helo x = if (x == Nothing)
    then False
    else True

list6 mxs = map (\ (Just a) -> a + 5) (filter helo mxs)

-- HIGH ORDER to LIST COMPRE

-- no. 1: map (+3) xs
list7 xs = [ x+3 | x <- xs]

-- no. 2: filter (>7) xs
list8 xs = [ x | x <- xs, x > 7]

-- no.3: concat (map (\x -> map (\y -> (x,y)) ys) xs)
-- belom mirip [FAILED]
list9 xs ys = [ (x,y) | x <- xs, y <- ys]

-- ???
mystery9 (x:xs) (y:ys) = map (\x -> map (\y -> (x,y)) ys) xs

-- no. 4: filter (>3) (map (\(x,y) -> x+y) xys)
list10 xys = [ x+y | (x,y) <- xys, x + y > 3 ]


phytagoras = [ (x,y,z) | x <- [1..], y <- [1..], z <- [1..], x**2 + y**2 == z **2 ]
maxList xs = foldr maxMe 0 xs
maxMe a b = if a > b then a else b