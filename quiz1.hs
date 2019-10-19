-- kpk
check x y z = if z `mod` x == 0 && z `mod` y == 0 then True else False
maxs a b xs = filter (check a b) xs
kpk x y = head (maxs x y [1..(x*y)])

-- no.2 [(x,y) | x <- [1..3], y <- [1..(2*x)]]
num2 = [(x,y) | x <- [1..3], y <- [1..(2*x)]]
-- this list will collect tuples of (x,y), which x be the pointer and value of y adjust to the x.

middle xs = (length xs) `div` 2
mergeSort [] = []
mergeSort [a] = [a]
mergeSort xs = merge (mergeSort rightList) (mergeSort leftList)
    where rightList = take (middle xs) xs
          leftList = drop (middle xs) xs

merge a [] = a
merge [] b = b
merge (a:as) (b:bs)
    | a < b = a : (merge as (b:bs))
    | otherwise = b : (merge (a:as) bs)
