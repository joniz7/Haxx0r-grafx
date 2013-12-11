module Expr where
import Parsing
import Data.Char
import Data.Maybe
import Test.QuickCheck

-- Defines a data type for expression
data Expr = Num Double | Var String | Sin Expr | Cos Expr | Add Expr Expr | Mul Expr Expr

-- Make Expr an instance of Show
instance Show Expr where
   show = showExpr

-- Converts an expression to a string    
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
readAndEval s d = eval (fromJust(readExpr s)) d

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
         return (Num ((toDouble i1)+
            ((toDouble i2)/(10^(length i2)))))
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
       if (a:b:c:[]) == "sin" then return (Sin d)   
       else if (a:b:c:[]) == "cos" then return (Cos d)
       else expr       

-- Parser for outer expressions
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
-- Parser for terms         
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
-- Parser for factors         
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
   rNum = fmap Num (arbitrary:: Gen Double)

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
     