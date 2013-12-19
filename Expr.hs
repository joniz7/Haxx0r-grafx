module Expr where
import Parsing
import Data.Char
import Data.Maybe
import Test.QuickCheck

-- Defines a data type for expression
data Expr = Num Double | Var String | Sin Expr | Cos Expr | Add Expr Expr | Mul Expr Expr
   --deriving Show
-- Make Expr an instance of Show
instance Show Expr where
   show = showExpr

-- Converts an expression to a string    
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

-- Given a value, calculates the value of the expression for that value
eval :: Expr -> Double -> Double
eval (Num i) _     = i
eval (Var s) i     = i
eval (Add e1 e2) x = eval e1 x + eval e2 x
eval (Mul e1 e2) x = eval e1 x * eval e2 x
eval (Sin e)     x = sin (eval e x)
eval (Cos e)     x = cos (eval e x)

-- Converts a string to an expression data type.
readExpr :: String -> Maybe Expr
readExpr s = let s' = filter (not.isSpace) s
             in case parse expr s' of
                     Just (e,"") -> Just e
                     _           -> Nothing

                     -- Given a string and a value, first converts a string 
-- to an expression, and then calculates its value for the given value
readAndEval :: String -> Double -> Double
readAndEval s = eval (fromJust(readExpr s))

-- Parser for whole numbers  
num :: Parser Expr
num = do
      c <- oneOrMore digit
      return (Num (read c ::Double))
-- Parse the '.' in doubles, helps when parsing numbers with decimals
dot :: Parser Char
dot = char '.'

-- Parser for variable
var :: Parser Expr
var = 
      do
         char 'x'
         return (Var "x")
-- Parser for numbers with decimals
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
-- Parser for functions
func :: Parser Expr
func = do
       a <- item
       b <- item
       c <- item
       d <- factor   
       if [a,b,c] == "sin" then return (Sin d)   
       else if [a,b,c] == "cos" then return (Cos d)
       else expr       

-- Parser for outer expressions
expr :: Parser Expr
expr = do
         a <- term
         char '+'
         b <- expr
         return (Add a b)
       +++
       term
-- Parser for terms         
term :: Parser Expr
term = do
         a <- factor
         char '*'
         b <- term
         return (Mul a b)
       +++
       factor       
-- Parser for factors         
factor :: Parser Expr
factor = do
            char '('
            a <- expr
            char ')'
            return a
         +++
         doub
         +++
         var
-- Simplifies a given expression         
simplify :: Expr -> Expr
simplify (Add a b)      = add (simplify a) (simplify b)
simplify (Mul a b)      = mul (simplify a) (simplify b)         
simplify (Sin expr)    = Sin (simplify expr)
simplify (Cos expr)    = Cos (simplify expr)
simplify expr         = expr

-- Test property for simplify-function

{- Because of inaccuracy in the Double datatype a simplified version of
   a very complex equation might result in a slightly different value.
   Therefore we check if eval of av expression is close to the eval of
   the simplified version of that expression. -}
   
prop_simplify :: Expr -> Double -> Bool
prop_simplify expr n = (eval expr n) > (eval (simplify expr) n) - (10e-3) 
                       || (eval expr n) < (eval (simplify expr) n) + (10e-3)

-- Takes an expression and derives it with respect to x
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
{-The code looks really ugly, but it makes the functions look so much nicer-}
add (Mul (Num i1) (Var "x")) 
    (Mul (Num i2) (Var "x")) = mul (Num (i1+i2)) (Var "x")
add (Mul (Num i1) (Var "x")) 
    (Mul (Var "x") (Num i2)) = mul (Num (i1+i2)) (Var "x")
add (Mul (Var "x") (Num i1)) 
    (Mul (Num i2) (Var "x")) = mul (Num (i1+i2)) (Var "x")
add (Mul (Var "x") (Num i1)) 
    (Mul (Var "x") (Num i2)) = mul (Num (i1+i2)) (Var "x")
    
add (Add (Num i1) (Var "x")) 
    (Add (Num i2) (Var "x")) = add (Num (i1+i2)) (Var "x")
add (Add (Num i1) (Var "x")) 
    (Add (Var "x") (Num i2)) = add (Num (i1+i2)) (Var "x")
add (Add (Var "x") (Num i1)) 
    (Add (Num i2) (Var "x")) = add (Num (i1+i2)) (Var "x")
add (Add (Var "x") (Num i1)) 
    (Add (Var "x") (Num i2)) = add (Num (i1+i2)) (Var "x")
    
add (Num i1) (Add e (Num i2)) = add (Num (i1+i2)) e
add (Num i1) (Add (Num i2) e) = add (Num (i1+i2)) e
add (Add e (Num i2)) (Num i1)= add (Num (i1+i2)) e
add (Add (Num i2) e) (Num i1)= add (Num (i1+i2)) e

{-
add e1 (Add e2 (Num i)) = add (Num i) (add e1 e2)
add e1 (Add (Num i) e2) = add (Num i) (add e1 e2)
add (Add e1 (Num i)) e2= add (Num i) (add e1 e2)
add (Add (Num i) e1) e2= add (Num i) (add e1 e2)
-}
add e1 e2               = Add e1 e2

{-Same deal here as above. The code might look ugly, but the functions are so
  pretty, so pretty-}
mul _ (Num 0)       = Num 0
mul (Num 0) _       = Num 0
mul e (Num 1)       = e
mul (Num 1) e       = e
mul (Num (-1)) (Num (-1)) = Num 1

mul (Num x) (Mul (Num y) e) = mul (Num (x*y)) e
mul (Num x) (Mul e (Num y)) = mul (Num (x*y)) e
mul (Mul (Num x) e) (Num y) = mul (Num (x*y)) e
mul (Mul e (Num x)) (Num y) = mul (Num (x*y)) e

mul (Num x) (Num y) = Num (x*y)

mul (Mul (Num i1) e1) 
    (Mul (Num i2) e2) = mul (Num (i1*i2)) (mul e1 e2)
mul (Mul (Num i1) e1) 
    (Mul e2 (Num i2)) = mul (Num (i1*i2)) (mul e1 e2)
mul (Mul e1 (Num i1)) 
    (Mul (Num i2) e2) = mul (Num (i1*i2)) (mul e1 e2)
mul (Mul e1 (Num i1)) 
    (Mul e2 (Num i2)) = mul (Num (i1*i2)) (mul e1 e2)

mul e1 (Mul (Num i) e2) = mul (Num i) (mul e1 e2)
mul e1 (Mul e2 (Num i)) = mul (Num i) (mul e1 e2)
mul (Mul e1 (Num i)) e2 = mul (Num i) (mul e1 e2)
mul (Mul (Num i) e1) e2 = mul (Num i) (mul e1 e2)
    
mul e1 e2 = Mul e1 e2

-- Property to check the function readExpr and showExpr            
prop_showReadExpr :: Expr -> Bool
prop_showReadExpr expr | isNothing e = False
                       | otherwise = showExpr (fromJust e) == showExpr expr
        where e = readExpr(showExpr expr)

-- An expression generator with a size parameter        
rExpr :: Int -> Gen Expr
rExpr s = frequency [(1,rNum),(s,rOp), (s,rFu)]
  where
   s' = s `div` 2
   {-We had to generate the numbers like this since we would
     otherwise get numbers like 1.2341212415e-2 which our
     program can't parse.-}
   rNum = elements [Num x| x <-[-100.0, -99.5..100.0]]

   rOp = do 
      op <- elements [Add,Mul]
      e1 <- rExpr s'
      e2 <- rExpr s'
      return $ op e1 e2

   rFu = do
      fu <- elements [Sin, Cos]
      e <- rExpr s'
      return (fu e)
      
-- Makes Expr an instance of Arbitrary      
instance Arbitrary Expr where
  arbitrary = sized rExpr
     
