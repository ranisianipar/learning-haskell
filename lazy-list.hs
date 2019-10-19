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

-- Permutation
-- permutation [] = []
-- permutation (x:[]) = [x]
-- permutation xs = [x | x <- xs]

-- prime number: Sieve of Erastothenes
primes = sieve (2:[3,5..]) where
    sieve (p:xs) = p : sieve [ x | x <- xs, x `mod` p > 0]
