data Expr = C Float | Expr :+ Expr | Expr :- Expr | Expr :* Expr | Expr :/ Expr 
            | V String | Let String Expr Expr
            deriving Show

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



