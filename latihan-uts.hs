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
evaluate (C x) = x
-- test: evaluate (C 1 :+ C 2)
-- result: 3
evaluate (e1 :+ e2) = evaluate e1 + evaluate e2