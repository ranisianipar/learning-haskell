data Expr = C Int | Expr :+ Expr | Expr :- Expr
            | Expr :* Expr | Expr :/ Expr
            | V String
            | Let String Expr Expr
        deriving Show

-- implement map
mapExpr :: (Expr -> Int) -> [Expr] -> [Int]
mapExpr _ [] = []
mapExpr f (x:xs) = f x : mapExpr f xs
-- test: mapExpr (\x -> evaluate (C 1 :+ x)) [C 4, C 5]
-- result = [5, 6]

evaluate :: Expr -> Int
-- recursive
evaluate (C x) = x
-- test: evaluate (C 1 :+ C 2)
-- result: 3
evaluate (e1 :+ e2) = evaluate e1 + evaluate e2
evaluate (e1 :- e2) = evaluate e1 - evaluate e2
evaluate (e1 :* e2) = evaluate e1 * evaluate e2
evaluate (e1 :/ e2) = evaluate e1 `div` evaluate e2

-- implement fold
foldExpr :: (Expr -> Expr -> Int) -> Int -> [Expr] -> Int
foldExpr f a [] = a
foldExpr f a (x:xs) = foldExpr f (f a x) xs

evaluate2Add :: Expr -> Expr -> Int
evaluate2Add x y = evaluate (x :+ y)
-- evaluate2 (C x) (C y) = evaluate (C x :+ C y)

