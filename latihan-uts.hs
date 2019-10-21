data Expr = C Float | Expr :+ Expr | Expr :- Expr | Expr :* Expr | Expr :/ Expr
            | V String | Let String Expr Expr
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

-- subst
subst :: String -> Expr -> Expr -> Expr
subst v0 e0 (V v1) = if (v0 == v1) then e0 else (V v1)
subst _ _ (C c1) = (C c1)
subst v0 e0 (e1 :+ e2) = subst v0 e0 e1 :+ subst v0 e0 e2
subst v0 e0 (e1 :- e2) = subst v0 e0 e1 :- subst v0 e0 e2
subst v0 e0 (e1 :* e2) = subst v0 e0 e1 :* subst v0 e0 e2
subst v0 e0 (e1 :/ e2) = subst v0 e0 e1 :/ subst v0 e0 e2

-- OLD SUBST
-- subst v0 e0 (Let v1 e1 e2) = Let v1 e1 (subst v0 e0 e2) -- perlu koreksi ???
-- koreksi subst
subst v0 e0 (Let v1 e1 e2) = Let v1 e1 e2


-- implement fold
foldExpr :: (Expr -> Expr -> Int) -> Int -> [Expr] -> Int
foldExpr f a [] = a
foldExpr f a (x:xs) = foldExpr f (f a x) xs

evaluate2Add :: Expr -> Expr -> Int
evaluate2Add x y = evaluate (x :+ y)
-- evaluate2 (C x) (C y) = evaluate (C x :+ C y)

