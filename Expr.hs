import Parsing

data Expr = Num Double | Var String | Sin Expr | Cos Expr | Add Expr Expr | Mul Expr Expr
instance Show Expr where
   show = showExpr

showExpr :: Expr -> String
showExpr e = case e of
    (Num i) -> show i
    (Var s) -> s
    (Sin e) -> "sin " ++ paranthesis e
    (Cos e) -> "cos " ++ paranthesis e
    (Add e1 e2) -> showExpr e1 ++ " + " ++ showExpr e2
    (Mul e1 e2) -> needParanthesis e1 ++ "*" ++ needParanthesis e2

    where paranthesis (Num i)         = showExpr (Num i)
          paranthesis (Var s)         = showExpr (Var s)
          paranthesis exp             = "(" ++ showExpr exp ++ ")"
          needParanthesis (Add e1 e2) = "(" ++ showExpr (Add e1 e2) ++ ")"

-- (5+6)*8*9 = 792
ex1 = Mul (Add (Num 5) (Num 6)) (Mul (Num 8) (Num 9))

-- sin(7+2*1)
ex2 = Sin (Add (Num 7) (Mul (Num 2) (Num 1)))

-- cos(x*4+6*2) + sin(x)
ex3 = Add (Cos (Add (Mul (Var "x") (Num 4)) (Mul (Num 6) (Num 2)))) (Sin (Var "x"))

eval :: Expr -> Double -> Double
eval (Num i) _     = i
eval (Var s) i     = i
eval (Add e1 e2) x = eval e1 x + eval e2 x
eval (Mul e1 e2) x = eval e1 x * eval e2 x
eval (Sin e)     x = sin (eval e x)
eval (Cos e)     x = cos (eval e x)

readExpr :: String -> Maybe Int
readExpr = undefined
