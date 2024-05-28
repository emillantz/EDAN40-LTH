-- Axel Froborg (ax3051fr-s) and Emil Lantz (em0377la-s)

module Parser
  ( module CoreParser,
    T,
    digit,
    digitVal,
    chars,
    letter,
    err,
    lit,
    number,
    iter,
    accept,
    require,
    token,
    spaces,
    word,
    comment,
    (-#),
    (#-),
  )
where

import CoreParser
import Data.Char
import Prelude hiding (fail, return)

infixl 7 -#, #-

type T a = Parser a

err :: String -> Parser a
err message cs = error (message ++ " near " ++ cs ++ "\n")

iter :: Parser a -> Parser [a]
iter m = m # iter m >-> cons ! return []

cons (a, b) = a : b

(-#) :: Parser a -> Parser b -> Parser b
m -# n = m # n >-> snd

(#-) :: Parser a -> Parser b -> Parser a
m #- n = m # n >-> fst

spaces :: Parser String
spaces = iter (char ? isSpace)

comment :: Parser String
comment = accept "--" -# iter (char ? (/= '\n')) -# require "\n"

whiteSpace :: Parser String
whiteSpace = iter (spaces # comment) -# spaces

token :: Parser a -> Parser a
token m = m #- whiteSpace

letter :: Parser Char
letter = char ? isAlpha

word :: Parser String
word = token (letter # iter letter >-> cons)

chars :: Int -> Parser String
chars 0 = return []
chars n = char # chars (n - 1) >-> cons

accept :: String -> Parser String
accept w = token (chars (length w)) ? (== w)

require :: String -> Parser String
require w = accept w ! err ("expecting " ++ w)

lit :: Char -> Parser Char
lit c = token char ? (== c)

digit :: Parser Char
digit = char ? isDigit

digitVal :: Parser Integer
digitVal = digit >-> digitToInt >-> fromIntegral

number' :: Integer -> Parser Integer
number' n =
  digitVal
    #> (\d -> number' (10 * n + d))
    ! return n

number :: Parser Integer
number = token (digitVal #> number')
