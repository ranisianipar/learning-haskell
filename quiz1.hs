-- kpk
check x y z = if z `mod` x == 0 && z `mod` y == 0 then True else False
maxs a b xs = filter (check a b) xs
kpk x y = head (maxs x y [1..(x*y)])