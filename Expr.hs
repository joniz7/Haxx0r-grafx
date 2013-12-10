import Parsing
import Data.Char
import Data.Maybe
import Test.QuickCheck

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
          needParanthesis (Add e1 e2) = "(" ++ showExpr e1 ++ "+" ++ showExpr e2 ++ ")"
          needParanthesis exp         = showExpr exp

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
var = do
         char 'x'
         return (Var "x")

doub :: Parser Expr
doub = 
       func
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
            
prop_showReadExpr :: Expr -> Bool
prop_showReadExpr expr | isNothing e = False
                       | otherwise = showExpr (fromJust e) == showExpr expr
        where e = readExpr(showExpr expr)

arbExpr :: Int -> Gen Expr
arbExpr i = genExpr
 
genExpr :: Gen Expr
genExpr = elements [Num 5.0, Var "x", Add (Num 10.0) (Var "x"), Mul (Mul (Num 8.0) (Var "x")) (Num 20.0), Sin (Var "x"), Cos (Num 9.0)]

instance Arbitrary Expr where
    arbitrary = sized arbExpr 

-- <expression> ::= <term> | <term> "+" <expression>

-- <term>       ::= <factor> | <factor> "*" <term>

-- <factor>     ::= "(" <expression> ")" | <number>

