-- Add your names and student numbers to the following file. Do not change anything else, since it is parsed.
-- Student 1: Matei Bucur (S2626381)
-- Student 2: Ujjwal Dodeja (s2526220)
-- Student 3: Third student (szzzzzzz)

module PComb where
import Control.Applicative
import Data.Char
import Data.Monoid
import Test.QuickCheck

-- Stream of Chars - can be extended with the location for error handling
data Stream = Stream [Char]
              deriving (Eq, Show)

---------------------------
--FP1.1

{-
DEF: parser data type
  P is the constructor
  we use record syntax, runParser extracts what is inside the data constructor
  runParser gets a Parser of type a and a Stream and returns a [] on failure or
  a tuple with the the parsing result and the remaining Stream
-}

data Parser a = P {
    runParser :: Stream -> [(a , Stream)]
}

--Unit Tests---------------
--helper function to test runParser
parOne :: Stream -> [(Int, Stream)]
parOne (Stream (x:xs)) | x == '1'  = [(1, Stream xs)]
                       | otherwise = []

-- :t P hFunc == Parser Int
prop_parserData :: String -> Bool       --can be checked using quickCheck
prop_parserData xs = runParser (P parOne) (Stream ("1" ++ xs)) == [(1,Stream xs)]   --randomly produces strings, adds one as the head of the string and then checks if the required results are achieved


---------------------------
--FP1.2
{-
DEF: functor instance of the parser defined above
-}

instance Functor Parser where
    fmap f par = P (\str -> [(f x, xs) | (x,xs) <- runParser par str])

------------Tests and Usage----------
data T = Node Int
checkFunctor = Node <$> (P parOne) -- returns a parser of type T

---------------------------
--FP1.3
{-
DEF: Char Parser
     takes a character, and parses the character on the Stream, and results into the parsed character and the reamianing stream or []
-}
char :: Char -> Parser Char
char c = P par
        where par (Stream []) = []                                              -- Returns an empty list in case of an empty Stream
              par (Stream xs) | head xs == c = [(head xs, Stream (tail xs))]    -- if the given char is parsed, returns the char and the rest of the stream
                              | otherwise    = []                               -- Also returns empty if the char is not parsed

------Tests and Usages-----

checkA = runParser (char 'a') (Stream "apple")          --Test passes and returns [('a', Stream "pple")]
checkB = runParser (char 'b') (Stream "boy")            --Test passes and returns [('b',Stream "oy")]
checkP = runParser (char 'p') (Stream "apple")          --Test fails as 'p' is not at the beginning at the Stream and the parser stops; returns []

prop_char :: Char -> Bool       --can be tested using quickCheck
prop_char xs = runParser (char xs) (Stream  (xs:"")) == [(xs, Stream "")]

---------------------------
--FP1.4

{-
DEF: Single Parser
failure - no matter what input is recieved, it results in a failure and an empty [] is returned
-}

failure :: Parser a
failure = P (\_ -> [])

------Tests and Usages-----

checkFailure00 = runParser (failure) (Stream "abc")
checkFailure01 = runParser (failure) (Stream "")

---------------------------
--FP1.5

{-
DEF: Applicative
runs two parsers in sequence; the first one is run on the original stream
the second one is only run if the first one is successfull; it is run on the remaining stream returned by the first one
-}

instance Applicative Parser where
    pure x = P (\z -> [(x, z)])
    a <*> b = P (\z -> [ (x y, n)| (x, m) <- runParser a z, (y, n) <- runParser b m])

------Tests and Usages-----

pureEx = runParser (pure 42) (Stream "apple") --gives out the same result for all the Streams, here [(42, Stream "apple")]
checkAp00 = runParser (((,) <$> (char 'a')) <*> (char 'p')) (Stream "apple")  --parses both 'a' and 'p', and returns [(('a','p'),Stream "ple")]
checkAp01 = runParser (((,) <$> (char 'd')) <*> (char 'p')) (Stream "apple")  --fails because 'd' is not parsed, and returns and empty []

---------------------------
--FP1.6

{-
DEF: Alternative
runs one of the given parsers;
if the first parser is successful, the second parser is ignored
if the first parser is unsuccessful, the second parser is tried
-}

instance Alternative Parser where
   empty = failure
   p1 <|> p2  = P p
        where p x = let rp1 = runParser p1 x in
                  case rp1 of
                     [] -> runParser p2 x
                     _  -> rp1

 ------Tests and Usages-----

checkAlt = runParser (char 'p' <|> char 'p' <|> char 'a') (Stream "apple")
checkAlt2 = runParser (char 'p' <|> char 'a') (Stream "apple")
checkAlt3 = runParser (char 'a' <|> char 'p') (Stream "apple")
checkAlt4 = runParser (char 'a' <|> char 'a') (Stream "apple")
checkAlt5 = runParser (char 'p' <|> char 'p') (Stream "apple")

---------------------------
