module Expr where
import Parsing
import Data.Char
import Data.Maybe
import Test.QuickCheck

data Expr = Num Double | Var String | Sin Expr | Cos Expr | Add Expr Expr | Mul Expr Expr
instance Show Expr where
   show = showExpr

showExpr :: Expr -> String
showExpr e = case e of
    (Num i) | i<0 -> "("++show i++")"
            |otherwise -> show i
    (Var s) -> s
    (Sin e) -> "sin " ++ paranthesis e
    (Cos e) -> "cos " ++ paranthesis e
    (Add e1 e2) -> showExpr e1 ++ "+" ++ showExpr e2
    (Mul e1 e2) -> needParanthesis e1 ++ "*" ++ needParanthesis e2

    where paranthesis (Num i)         = showExpr (Num i)
          paranthesis (Var s)         = showExpr (Var s)
          paranthesis (Sin e)         = showExpr (Sin e)
          paranthesis (Cos e)         = showExpr (Cos e)
          paranthesis exp             = "(" ++ showExpr exp ++ ")"
          needParanthesis (Add e1 e2) = "(" ++ showExpr e1 ++ "+" ++ showExpr e2 ++ ")"
          needParanthesis exp         = showExpr exp
          
eval :: Expr -> Double -> Double
eval (Num i) _     = i
eval (Var s) i     = i
eval (Add e1 e2) x = eval e1 x + eval e2 x
eval (Mul e1 e2) x = eval e1 x * eval e2 x
eval (Sin e)     x = sin (eval e x)
eval (Cos e)     x = cos (eval e x)

readExpr :: String -> Maybe Expr
readExpr s = let s' = filter (not.isSpace) s
             in case parse expr s' of
                     Just (e,"") -> Just e
                     _           -> Nothing
   
readAndEval :: String -> Double -> Double
readAndEval s d = eval (fromJust(readExpr s)) d
   
num :: Parser Expr
num = do
      c <- oneOrMore digit
      return (Num (read c ::Double))

dot :: Parser Char
dot = char '.'

var :: Parser Expr
var = 
      do
         char 'x'
         return (Var "x")

doub :: Parser Expr
doub = 
       func
       +++
       do
         char '('
         char '-'
         i <- doub
         char ')'
         return (Num (- (eval i 0)))
       +++
       do
         i1 <- oneOrMore digit
         d <- dot
         i2 <- oneOrMore digit
         return (Num (toDouble (i1++"."++i2)))
       +++ do 
            i1 <- oneOrMore digit
            return (Num (toDouble i1))
            
   where toDouble i = read i::Double

func :: Parser Expr
func = pSin +++ pCos
   
pSin :: Parser Expr
pSin = do
         char 's'
         char 'i'
         char 'n'
         d <- factor
         return (Sin d)

pCos :: Parser Expr
pCos = do
         char 'c'
         char 'o'
         char 's'
         d <- factor
         return (Cos d)
         
expr :: Parser Expr
expr = do
         a <- term
         char '+'
         b <- expr
         return (Add a b)
       +++
       do
         a <- term
         return a
         
term :: Parser Expr
term = do
         a <- factor
         char '*'
         b <- term
         return (Mul a b)
       +++
       do
         a <- factor
         return a
         
factor :: Parser Expr
factor = do
            char '('
            a <- expr
            char ')'
            return a
         +++
         do
            a <- doub
            return a
         +++
         do
            a <- var
            return a
            
derive :: Expr -> Expr
derive (Add e1 e2) = add (derive e1) (derive e2)
derive (Mul e1 e2) = add (mul e1 (derive e2)) (mul e2 (derive e1))
derive (Sin e)     = mul (Cos e) (derive e)
derive (Cos e)     = mul (mul (Num (-1)) (Sin e)) (derive e)
derive (Var "x")   = Num 1
derive _           = Num 0

{- Some special cases to make the functions look nicer after derivation -}
add (Num x) (Num y)     = Num (x+y)
add e (Num 0)           = e
add (Num 0) e           = e
add (Var "x") (Var "x") = mul (Num 2) (Var "x")

{- x + 2x = 3x -}
add (Var "x") (Mul (Num i) (Var "x")) = mul (Num (i+1)) (Var "x")
add (Var "x") (Mul (Var "x") (Num i)) = mul (Num (i+1)) (Var "x")
add (Mul (Num i) (Var "x")) (Var "x") = mul (Num (i+1)) (Var "x")
add (Mul (Var "x") (Num i)) (Var "x") = mul (Num (i+1)) (Var "x")

{- 2x + 3x = 5x -}
add (Mul (Num i1) (Var "x")) 
    (Mul (Num i2) (Var "x")) = mul (Num (i1+i2)) (Var "x")
add (Mul (Num i1) (Var "x")) 
    (Mul (Var "x") (Num i2)) = mul (Num (i1+i2)) (Var "x")
add (Mul (Var "x") (Num i1)) 
    (Mul (Num i2) (Var "x")) = mul (Num (i1+i2)) (Var "x")
add (Mul (Var "x") (Num i1)) 
    (Mul (Var "x") (Num i2)) = mul (Num (i1+i2)) (Var "x")

add e1 e2               = Add e1 e2

mul _ (Num 0)       = Num 0
mul (Num 0) _       = Num 0
mul e (Num 1)       = e
mul (Num 1) e       = e
mul (Num (-1)) (Num (-1)) = Num 1
mul (Num x) (Num y) = Num (x*y)
mul e1 e2           = Mul e1 e2
            
prop_showReadExpr :: Expr -> Bool
prop_showReadExpr expr | isNothing e = False
                       | otherwise = showExpr (fromJust e) == showExpr expr
        where e = readExpr(showExpr expr)

rExpr :: Int -> Gen Expr
rExpr s = frequency [(1,rNum),(s,rOp), (s,rFu)]
  where
   s' = s `div` 2
   rNum = (elements [Num x| x <-[-100.0, -99.5..100.0]])

   rOp = do 
      op <- elements [Add,Mul]
      e1 <- rExpr s'
      e2 <- rExpr s'
      return $ op e1 e2

   rFu = do
      fu <- elements [Sin, Cos]
      e <- rExpr s'
      return (fu e)
instance Arbitrary Expr where
  arbitrary = sized rExpr
    