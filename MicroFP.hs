-- Add your names and student numbers to the following file. Do not change anything else, since it is parsed.
-- Student 1: Matei Bucur (S2626381)
-- Student 2: Ujjwal Dodeja (s2526220)
-- Student 3: Third student (szzzzzzz)

{-# LANGUAGE TemplateHaskell #-}

module MicroFP where

import Control.Applicative
import PComb
import BasicParsers
import Test.QuickCheck
import Test.QuickCheck.All

---------------------------
--FP3.1

{-
DEF: EDSL for the microFP language
  in order to make the Cond data type more compact, we decided to move
  the expressions that had to be compared in the body of If
  constructor, inside the Expr data type

-}

data Prog = Program [FunDef]
            deriving Show

data FunDef = Fun String [Variab] Expr
            deriving Show

data Variab = Var String  | Cons Integer
            deriving Show

data Expr = Par Variab
            | Add Expr Expr
            | Sub Expr Expr
            | Mul Expr Expr
            | If Expr Cond Expr Expr Expr
            | Call String [Expr]
            deriving Show

data Cond = Equal | Gthan  | Lthan
            deriving Show

------------Usage----------
checkSumProg = Program [Fun "mySum" [Var "x", Var "y"] (Add (Par $ Var "x") (Par $ Var "y"))]

---------------------------
--FP3.2

{-
DEF: defininitions of the functions.txt in the
microFP language
-}

fibonacci :: Prog
fibonacci = Program [ Fun "fibonacci"
                          [Cons 0]
                          (Par $ Cons 0)
                     , Fun "fibonacci"
                          [Cons 1]
                          (Par $ Cons 1)
                     , Fun "fibonacci"
                          [Var "n"]
                          (Add (Call "fibonacci" [Sub
                                            (Par $ Var "n")
                                            (Par $ Cons 1)])
                                (Call "fibonacci" [Sub
                                            (Par $ Var "n")
                                            (Par $ Cons 2)]))
                     ]

fib :: Prog
fib = Program [ Fun "fib"
                  [Var "n"]
                  (If (Par $ Var "n")
                      Lthan
                      (Par $ Cons 3)
                      (Par $ Cons 1)
                      (Add
                        (Call "fib" [Sub
                                      (Par $ Var "n")
                                      (Par $ Cons 1)])
                        (Call "fib" [Sub
                                      (Par $ Var "n")
                                      (Par $ Cons 2)])))
             ]

sum :: Prog
sum = Program [ Fun "sum"
                 [Cons 0]
                 (Par $ Cons 0)
             , Fun "sum"
                 [Var "a"]
                 (Add
                   (Call "sum" [Sub
                                 (Par $ Var "a")
                                 (Par $ Cons 1)])
                   (Par $ Var "a"))
             ]

div :: Prog
div = Program [ Fun "div"
                  [Var "x", Var "y"]
                  (If (Par $ Var "x")
                      Lthan
                      (Par $ Var "y")
                      (Par $ Cons 0)
                      (Add
                        (Par $ Cons 1)
                        (Call "div" [ Sub
                                        (Par $ Var "x")
                                        (Par $ Var "y")
                                    , Par $ Var "y"])))
              ]

add :: Prog
add = Program [Fun "add"
                [Var "x", Var "y"]
                    (Add
                        (Par $ Var "x")
                        (Par $ Var "y"))]

twice :: Prog
twice = Program [Fun "twice"
                    [Var "f", Var "x"]
                        (Call "f"
                            [Call "f"
                                [Par $ Var "x"]])]

inc :: Prog
inc = Program [Fun "inc"
                []
                    (Call "add"
                        [Par $ Cons 1])]

eleven :: Prog
eleven = Program [Fun "eleven"
                    []
                        (Call "inc"
                            [Par $ Cons 10])]

fourty :: Prog
fourty = Program [Fun "fourty"
                    []
                        (Call "twice"
                            [Par $ Var "double"
                            ,Par $ Cons 10])]

main :: Prog
main = Program [Fun "main"
                    []
                        (Call "div"
                            [Par $ Cons 999
                            ,Par $ Cons 2])]
---------------------------
--FP3.3
{-
DEF: Printer which prints all the data structures of the EDSL in a readable format
-}

class Pretty p where
    pretty :: p -> String

instance Pretty Variab where
    pretty (Var a) = a
    pretty (Cons a) = show a

instance Pretty Expr where
    pretty (Par a) = pretty a
    pretty (Add a b) = pretty a ++ " + " ++ pretty b
    pretty (Mul a b) = pretty a ++ " * " ++ pretty b
    pretty (Sub a b) = pretty a ++ " - " ++ pretty b
    pretty (If a b c d e) = "if (" ++ pretty a ++ " "
                            ++ pretty b ++ " " ++ pretty c
                            ++ ") then \n  {\n   " ++ pretty d
                            ++ "\n  } else {\n   " ++ pretty e ++ "\n  }"
    pretty (Call xs ls) =  xs ++ "(" ++ prettyList ls "," ++ ")"

instance Pretty FunDef where
    pretty (Fun xs ls x) = xs ++ " " ++ prettyList ls " " ++  " := " ++ pretty x ++ ";\n"

instance Pretty Cond where
    pretty (Equal) = "=="
    pretty (Gthan) = ">"
    pretty (Lthan) = "<"

instance Pretty Prog where
    pretty (Program xs) = prettyList xs " "

{-helper function; takes a list of type a and a delimiter and
returns a pretty list delimited by the character delimiter -}
prettyList :: Pretty a => [a] -> [Char] -> String
prettyList xs c = init $ foldr (\x y -> pretty x ++ c ++ y) "" xs

------------Unit Tests----------
prop_pCons = pretty (Cons 1) == "1"
prop_pVar = pretty (Var "x") == "x"

--Variables for testing the pretty printer
a = Par $ Cons 2
b = Par $ Cons 1
checkCond = Equal
e = Par $ Var "x"
d = Par $ Var "y"

prop_pAdd = pretty (Add a b) == "2 + 1"
prop_pSub = pretty (Sub a b) == "2 - 1"
prop_pMul = pretty (Mul a b) == "2 * 1"
prop_pPar = pretty a == "2"
prop_pCall = pretty (Call "div" [a, b]) == "div(2,1)"
prop_pEqual = pretty (Equal) == "=="

---------------------------
--FP3.4

{-
DEF: evaluator for a Program
  takes the name of the function that needs to be evaluated and its arguments
  the variable Prog is passed to all eval helper fonctions because it is needed
  when the Call needs to be evaluated

-}

eval :: Prog -> String -> [Integer] -> Integer
eval p s xs = evalFun (findFun s p) xs p

evalFun :: FunDef -> [Integer] -> Prog -> Integer
evalFun (Fun f par ex) args p  = evalExpr (bind ex (variabArrToStr par) args) p

{- Pre Condition: the function bind is run in order to replace all the unknowns
 with the actual arguments -}
evalExpr :: Expr -> Prog -> Integer
evalExpr (Par (Cons x)) f = x
evalExpr (Add e1 e2) f = evalExpr e1 f + evalExpr e2 f
evalExpr (Sub e1 e2) f = evalExpr e1 f - evalExpr e2 f
evalExpr (Mul e1 e2) f = evalExpr e1 f * evalExpr e2 f
evalExpr (If e1 op e2 e3 e4) f | c op (evalExpr e1 f) (evalExpr e2 f) = evalExpr e3 f
                               | otherwise                            = evalExpr e4 f
evalExpr (Call n xs) p = evalFun ( findFun n p )  ((\x -> evalExpr x p) <$> xs) p

c Equal = (==)
c Gthan = (>)
c Lthan = (<)

{-helper function; no pattern matching supported; the function definition can
 have as parameters only the unknown variables, which are of type String-}
variabArrToStr :: [Variab] -> [String]
variabArrToStr []     = []
variabArrToStr (x:xs) = toStr x : variabArrToStr xs
  where
    toStr (Var x) = x
    toStr (Cons x) = show x

--helper function; returns the name of the FunDef
getName :: FunDef -> String
getName (Fun n _ _) = n

{-helper function; looks up the function name in a program and
returns the first occurance of the function definition-}
findFun :: String -> Prog -> FunDef
findFun "" (Program xs) = last xs
findFun n (Program []) = error "no function found"
findFun n (Program (x:xs)) | getName x == n = x
                           | otherwise      = findFun n $ Program xs

{-helper function; replaces all the unknown variables in an expression with the provided arguments;
 it is required that the two lists are of same length -}
bind :: Expr -> [String] -> [Integer] -> Expr
bind ex _ [] = ex
bind (Par (Var s)) (x:xs) (e:es) | s == x    = Par $ Cons e
                                 | otherwise = bind (Par $ Var s) xs es
bind ex@(Par (Cons s)) xs es = ex
bind (Add e1 e2) xs es = Add (bind e1 xs es) (bind e2 xs es)
bind (Sub e1 e2) xs es = Sub (bind e1 xs es) (bind e2 xs es)
bind (Mul e1 e2) xs es = Mul (bind e1 xs es) (bind e2 xs es)
bind (Call s e1) xs es = Call s ((\x -> bind x xs es) <$> e1)
bind (If e1 c e2 e3 e4) xs es = If (bind e1 xs es) c (bind e2 xs es) (bind e3 xs es) (bind e4 xs es)

------------Tests and Usage----------
checkEval = eval fib "fib" [0]
checkEval2 = eval MicroFP.div "div" [10,0]
checkBind = bind (Call "div" [ Sub(Par $ Var "x") (Par $ Var "y"), Par $ Var "y"]) ["x","y"] [1,2]
checkFindFun = findFun "pr2"  (Program [ Fun "pr1" [] (Par $ Cons 0) , Fun "pr2"[] (Par $ Cons 1)])


prop_eval :: Integer -> Integer -> Bool
prop_eval x y = eval checkSumProg "mySum" [x,y] == (x + y)
prop_evalFib = eval fib "fib" [15] == (fibFun 15)
prop_evalFun :: Integer -> Integer -> Bool
prop_evalFun x y = evalFun multfun [x,y] (Program []) == (x * y)
prop_evalExpr = evalExpr (Sub (Par $ Cons 50) (Par $ Cons 1)) (Program [])  == 49

--testing helper function
fibFun :: Integer -> Integer
fibFun 0 = 1
fibFun 1 = 1
fibFun 2 = 1
fibFun n = fibFun (n-1) + fibFun (n-2)

--testing program
multfun = Fun "mult" [Var "x", Var "y"] (Mul (Par $ Var "x") (Par $ Var "y"))

---------------------------
--FP4.1

{-
DEF: needed parsers for microFP
-}
--to test the call part, probably not right, it misses the try that was in parsec

factor :: Parser Expr
factor = parseIf <|> (Call <$> identifier <*> (parens pars)) <|> Par <$> parseVariab <|> (parens expr)
         where pars = sep1 expr (char ',')

parseVariab :: Parser Variab
parseVariab = Cons <$> integer <|> Var <$> identifier

expr :: Parser Expr
expr = whitespace $ add <|> sub <|> term
    where
      add = Add <$> (term <* symbol "+") <*> expr
      sub = Sub <$> (term <* symbol "-") <*> expr

term :: Parser Expr
term = Mul <$> (factor <* symbol "*") <*> term <|> factor

equal :: Parser Cond        --results into the pure value of Equal only if the symbol parser is successful for "==" on the Stream
equal = (symbol "==") *> pure Equal

gthan :: Parser Cond        --results into the pure value of Gthan only if the symbol parser is successful for ">" on the Stream
gthan = (symbol ">") *> pure Gthan

sthan :: Parser Cond        --results into the pure value of Sthan only if the symbol parser is successful for "<" on the Stream
sthan = (symbol "<") *> pure Lthan

parseIf :: Parser Expr
parseIf = symbol "if" *> (If <$> (symbol "(" *>  expr) <*> cond <*> (expr <* symbol ")") <*> two <*> three)
          where two = symbol "then" *> (braces expr)
                three = symbol "else" *> (braces expr)
                cond = equal <|> sthan <|> gthan

func :: Parser FunDef
func = whitespace $ Fun <$> identifier <*> sep parseVariab (char ' ')  <*> (symbol ":=" *> expr <* symbol ";")

prog :: Parser Prog
prog = whitespace $ Program <$> sep1 func (char ' ')


--------Tests and Usages-----
--parseVariab
checkParseVariab00 = runParser (parseVariab) (Stream " 12 23")
checkParseVariab01 = runParser (parseVariab) (Stream "apple")

--factor
checkFactor00 = runParser (factor) (Stream "1")                 --checking all the possible cases in factor
checkFactor01 = runParser (factor) (Stream "fib(n-1, n+1)")
checkFactor10 = runParser (factor) (Stream "if (2==2) then {x} else {y}")
checkFactor11 = runParser (factor) (Stream "(x+1)")

--expr
checkExpr00 = runParser (expr) (Stream "1")
checkExpr01 = runParser (expr) (Stream "x+3")
checkExpr10 = runParser (expr) (Stream "x-3")

--term
checkTerm00 = runParser (term) (Stream "2*1")
checkTerm01 = runParser (term) (Stream "x*y")

--equal, gthan, sthan
checkEqual = runParser equal (Stream "==")
checkGthan = runParser gthan (Stream ">")
checkLthan = runParser sthan (Stream "<")

--parseIf
checkParseIf = runParser parseIf (Stream "if (2==2) then {x} else {y}")

--func
checkFunc = runParser func (Stream "fibonacci 0 := 0;fibonacci 1 := 1;")

--prog
checkProg = runParser prog (Stream "fibonacci 0 = 0;fibonacci 1 = 1;fibonacci n = fibonacci (n-1) + fibonacci (n-2);")

---------------------------
--FP4.2
{-
DEF: parses and translates a textual representation of microFP to
the previous EDSL
-}
compile :: String -> Prog
compile s = fst $ head $ runParser prog (Stream s)

------------Tests and Usage----------
checkCompile = compile $ pretty MicroFP.div

checkCompileFile :: IO FunDef
checkCompileFile = findFun <$> pure "fib" <*> compile <$> readFile "functions.txt"

prop_compile1  = show (compile $ pretty fib) == show fib
prop_compile2 = show (compile $ pretty fibonacci) == show fibonacci
prop_compile3 = show (compile $ pretty MicroFP.sum) == show MicroFP.sum
prop_compile4 = show (compile $ pretty MicroFP.div) == show MicroFP.div
prop_compile5 = show (compile $ pretty twice) == show twice

prop_add :: Integer -> Integer -> Bool
prop_add x y = eval (compile "add x y := x + y;") "add" [x,y] == (+) x y

prop_Fib = (==) (show $ compile "fib n := if (n < 3) then {1} else {fib (n-1) + fib (n-2)};") (show fib)

---------------------------
--FP4.3

{-
DEF: reads the program from the specified file, compiles it, and evaluates it
-}

runFile :: FilePath -> [Integer] -> IO Integer
runFile fp xs =  (eval) <$> prog <*> func <*> pure xs
  where
     func =  getName <$> (findFun <$> pure "" <*> prog) --findFun returns the last function if it receives an empty string as arg
     prog = compile <$> readFile fp

------------Tests and Usage----------

checkRun = runFile "functions.txt" []
checkRunArgs = runFile "functions.txt" [1,2,3]

-- QuickCheck: all prop_* tests
return []
check = $quickCheckAll
