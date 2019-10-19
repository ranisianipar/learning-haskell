-- Divisor
divisor n = [x | x <- [1..n], n `mod` x == 0]

-- quick sort: 1st element as a pivot
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a < x]
        biggerSorted = quicksort [b | b <- xs, b < x]   
    in smallerSorted ++ [x] ++ biggerSorted


quicksort1 [] = []
quicksort1 (x:xs) = filter (<x) xs ++ [x] ++ filter (>x) xs
