-- Add your names and student numbers to the following file. Do not change anything else, since it is parsed.
-- Student 1: Matei Bucur (S2626381)
-- Student 2: Ujjwal Dodeja (s2526220)
-- Student 3: Third student (szzzzzzz)

module BasicParsers where
import Control.Applicative
import Data.Char
import Data.Monoid
import Test.QuickCheck
import PComb

---------------------------
--FP2.1

{-
DEF: Char Parser
  letter and digit are parsers which parse a single character
  letter - it only parses the first character of a Stream if it belongs to the set of all alphabets
  dig - it only parses a character if it belongs to the set of all single digit numbers
-}
letter :: Parser Char
letter = P p
        where
            p (Stream []) = []
            p (Stream xs) | isLetter (head xs) = [(head xs, Stream $ tail xs)]
                          | otherwise          =  []

dig :: Parser Char
dig = P p
      where
        p (Stream []) = []
        p (Stream xs) = if isDigit (head xs) then [(head xs, Stream (tail xs))] else []

------------Tests----------
--letter
{-
 a check for letter can be defined by calling letter with runParser and supplying a Stream with letters,
 an example of this check is given below, it results in only the first letter of the Stream being parsed
 -}
checkLetter = runParser (letter) (Stream "xys")
prop_letter = checkLetter == [('x', Stream "ys")]

--dig
{-
 a check for letter can be defined by calling digit with runParser and supplying a Stream with digits, the same as below
 an example of a check is given below, it results in only the first digit of the Stream being parsed
 -}
checkDigit = runParser (dig) (Stream "123")
prop_digit = checkDigit == [('1', Stream "23")]

---------------------------
--FP2.2

{-
DEF: Sequential Combinators
 between - takes three parsers and runs them in a sequence on the current Stream,
           only giving out the result of the second parser by ignoring the results of the other two
 whitespace - takes a parser and runs it on the input stream, also parses all the whitespeces and newline characters
           around the target
-}

between :: Parser a -> Parser b -> Parser c -> Parser b
between open par close = result
                        where result = open *> par <* close

whitespace :: Parser a -> Parser a
whitespace p = let sp = many ((char ' ') <|> (char '\n') <|> (char '\t'))
                  in  between sp p sp

------------Tests----------
--between
checkBetween = runParser (between (char '1') (char '2') (char '3')) (Stream "123")

--whitespace
checkWhitespace0 = runParser (whitespace (char 'a')) (Stream " a b ")
checkWhitespace1 = runParser (whitespace (char 'a')) (Stream "ab ")
checkWhitespace2 = runParser (whitespace (char 'a')) (Stream "  a \n b ")
prop_whitespace = all (==checkWhitespace0)  [checkWhitespace1, checkWhitespace2]

---------------------------
--FP2.3

{-
DEF: Usage of Sequential and alternative Combinators
 sep1 - takes two parsers (one separator), and parses one or many occurences of these parsers in a sequence by using 'some'
 sep - takes two parsers (one separator), and parses zero and many occurences of these parsers in a sequence by using 'many'
 option - takes a parser and an object, tries to apply the parser on the Stream and returns the given object in case of failure
-}


sep1 :: Parser a -> Parser b -> Parser [a]
sep1 p s = some ((p <* s) <|> p)

sep :: Parser a -> Parser b -> Parser [a]
sep p s = many ((p <* s) <|> p)

option :: a -> Parser a -> Parser a
option x p = p <|> par
            where par = (P (\z -> [(x, z)]))

------------Tests----------
--sep1
sep1Par = sep1 (letter) (char ',')
checkSep10 = runParser (sep1Par) (Stream "a,p,p,l,e")
checkSep11 = runParser (sep1Par) (Stream "app,le")
checkSep12 = runParser (sep1Par) (Stream "appl,e")
prop_sep1 = all (==checkSep10) [checkSep11, checkSep12]

--sep
sepPar = sep (letter) (char ',')
checkSep0 = runParser (sepPar) (Stream "a,,p,p,l,e")
checkSep1 = runParser (sepPar) (Stream "app,le,")
checkSep2 = runParser (sepPar) (Stream "ap,,,pl,e")
prop_sep = all (==checkSep0) [checkSep1, checkSep2]

--option
checkOption00 = runParser (option 'a' (char 'a')) (Stream "abc") -- the given parser is successful and resultsin the required results
checkOption01 = runParser (option 'a' (char 'b')) (Stream "abc") -- the given parses fails and results in the provided object

---------------------------
--FP2.4

{-
DEF: Usage of previously defined parsers
 string - takes a string as input, and parses it char by char on the Stream
 identifier - parses an identifier(acc to the given naming rules) surrounded by whitespaces  in the given Stream if present
 integer - parses an integer value surrounded by whitespaces in the given Stream
 symbol - takes a String, and tries to parse the entire string as one entity and gives the immediate result
 parens - takes a parser, and uses it with between to parse the target of the parser surrounded by paranthesis in the Stream
 braces - takes a parser, and uses it with between to parse the target of the parser surrounded by braces in the Stream
-}

string :: String -> Parser String
string [] = pure []
string (s:ss) = (:) <$> char s <*> string ss

lowerCase = ['a'..'z']
alphanum = lowerCase ++ ['0'..'9']

lowLetter :: Parser Char -- helper function used to parse a lower case letter
lowLetter = P p
        where
            p (Stream []) = []
            p (Stream xs) | head xs `elem` lowerCase = [(head xs, Stream $ tail xs)]
                          | otherwise          =  []

alphaNum :: Parser Char -- helper function used to parse a lower case letter or digit
alphaNum = P p
        where
            p (Stream []) = []
            p (Stream xs) | head xs `elem` alphanum = [(head xs, Stream $ tail xs)]
                          | otherwise          =  []

identifier :: Parser String --uses lowLetter and alphaNum to parse an identifier acc to the specified naming conventions
identifier = whitespace  ((:) <$> lowLetter <*> many alphaNum )

readInteger :: String -> Integer
readInteger xs = read xs :: Integer   --changes a string value into an Integer if possible

integer :: Parser Integer
integer = whitespace (readInteger <$> ((:) <$> dig <*> many dig ))

symbol :: String -> Parser String
symbol xs = whitespace $ string xs

parens :: Parser a -> Parser a
parens par = between (char '(' ) par (char ')' )

braces :: Parser a -> Parser a
braces par = between (char '{' ) par (char '}' )

------------Tests----------
--string
stringPar = string "apple"
checkString0 = runParser stringPar (Stream "applemango")
checkString1 = runParser stringPar (Stream "apple")
checkString2 = runParser stringPar (Stream "mango")

prop_string :: String -> Bool           -- can be tested using quickCheck
prop_string xs = runParser (string xs) (Stream ("" ++ xs ++ "")) == [(xs, Stream "")]

--identifier
checkIdent00 = runParser identifier (Stream "a12") --successful as naming conventions are followed
checkIdent01 = runParser identifier (Stream "ab2r") --successful as naming conventions are followed

checkIdent10 = runParser identifier (Stream "123") --fails as the naming conventions are not followed

--integer
checkInteger00 = runParser (integer) (Stream "123")
checkInteger01 = runParser (integer) (Stream "777")

--symbol
checkSymbol00 = runParser (symbol "if") (Stream "if") -- passes and gives th erequired result
checkSymbol01 = runParser (symbol "then") (Stream "the1") -- fails because the required symbol is not present in the Stream

--parens
checkParens00 = runParser (parens integer) (Stream "(123)")
checkParens01 = runParser (parens identifier) (Stream "(a123)")

prop_parens :: String -> Bool           -- can be tested using quickCheck
prop_parens x = runParser (parens (string x)) (Stream ("(" ++ x ++")")) == [(x, Stream "")]

--braces
checkBraces00 = runParser (braces integer) (Stream "{123}")
checkBraces01 = runParser (braces identifier) (Stream "{a123}")

prop_braces :: String -> Bool           -- can be tested using quickCheck
prop_braces x = runParser (braces (string x)) (Stream ("{" ++ x ++"}")) == [(x, Stream "")]

---------------------------
