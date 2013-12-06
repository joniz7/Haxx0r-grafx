data Expr = Num Double | Var String | Sin Expr | Cos Expr | Add Expr Expr | Mul Expr Expr
instance Show Expr where
    show = showExpr

showExpr :: Expr -> String
showExpr (Num i) = show i
showExpr (Var s) = s
showExpr (Sin e) = "sin " ++ paranthesis e
showExpr (Cos e) = "sin " ++ paranthesis e
showExpr (Add e1 e2) = showExpr e1 ++ " + " ++ showExpr e2
showExpr (Mul e1 e2) = needParanthesis e1 ++ "*" ++ needParanthesis e2
   where paranthesis exp = "(" ++ showExpr exp ++ ")"
         paranthesis (Num i) = showExpr (Num i)
         paranthesis (Var s) = showExpr (Var s)
		 needParanthesis (Add e1 e2) = "(" ++ showExpr (Add e1 e2) ++ ")"
		 
		 