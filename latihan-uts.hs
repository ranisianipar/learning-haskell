data Expr = C Float | Expr :+ Expr | Expr :- Expr
            | Expr :* Expr | Expr :/ Expr
            | V String
            | Let String Expr Expr
        deriving Show
