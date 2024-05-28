-- Axel Froborg (ax3051fr-s) and Emil Lantz (em0377la-s)

module Statement (T, parse, toString, fromString, exec) where

import Dictionary qualified
import Expr qualified
import Parser hiding (T)
import Prelude hiding (fail, return)

type T = Statement

data Statement
  = Assignment String Expr.T
  | Skip
  | Begin [Statement]
  | If Expr.T Statement Statement
  | While Expr.T Statement
  | Read String
  | Write Expr.T
  | Comment String
  deriving (Show)

assignment :: Parser Statement
assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss

skip :: Parser Statement
skip = accept "skip" # require ";" >-> const Skip

begin :: Parser Statement
begin = accept "begin" -# iter parse #- require "end" >-> Begin

ifStmt :: Parser Statement
ifStmt = accept "if" -# Expr.parse #- require "then" # parse #- require "else" # parse >-> buildIf

while :: Parser Statement
while = accept "while" -# Expr.parse #- require "do" # parse >-> uncurry While

readStmt :: Parser Statement
readStmt = accept "read" -# word #- require ";" >-> Read

write :: Parser Statement
write = accept "write" -# Expr.parse #- require ";" >-> Write

commentStmt :: Parser Statement
commentStmt = accept "--" -# iter (char ? (/= '\n')) #- require "\n" >-> Comment

buildAss (v, e) = Assignment v e

buildIf ((cond, thenStmt), elseStmt) = If cond thenStmt elseStmt

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] _ _ = []
exec (If cond thenStmts elseStmts : stmts) dict input =
  if Expr.value cond dict > 0
    then exec (thenStmts : stmts) dict input
    else exec (elseStmts : stmts) dict input
exec (Assignment var expr : stmts) dict input = exec stmts (Dictionary.insert (var, Expr.value expr dict) dict) input
exec (Skip : stmts) dict input = exec stmts dict input
exec (Begin stmts' : stmts) dict input = exec (stmts' ++ stmts) dict input
exec (While cond thenStmt : stmts) dict input =
  if Expr.value cond dict > 0
    then exec (thenStmt : While cond thenStmt : stmts) dict input
    else exec stmts dict input
exec (Read var : stmts) dict (input : inputs) = exec stmts (Dictionary.insert (var, input) dict) inputs
exec (Write expr : stmts) dict input = Expr.value expr dict : exec stmts dict input
exec (Comment _ : stmts) dict input = exec stmts dict input

indent :: Int -> String
indent n = replicate (2 * n) ' '

toString' :: Int -> Statement -> String
toString' n (Assignment var expr) = indent n ++ var ++ " := " ++ Expr.toString expr ++ ";\n"
toString' n Skip = indent n ++ "skip;\n"
toString' n (Begin stmts) = indent n ++ "begin\n" ++ concatMap (toString' (n + 1)) stmts ++ indent n ++ "end\n"
toString' n (If cond thenStmt elseStmt) = indent n ++ "if " ++ Expr.toString cond ++ " then\n" ++ toString' (n + 1) thenStmt ++ indent n ++ "else\n" ++ toString' (n + 1) elseStmt
toString' n (While cond stmt) = indent n ++ "while " ++ Expr.toString cond ++ " do\n" ++ toString' (n + 1) stmt
toString' n (Read var) = indent n ++ "read " ++ var ++ ";\n"
toString' n (Write expr) = indent n ++ "write " ++ Expr.toString expr ++ ";\n"
toString' n (Comment c) = indent n ++ "-- " ++ c ++ "\n"

instance Parse Statement where
  parse = assignment ! skip ! begin ! ifStmt ! while ! readStmt ! write ! commentStmt
  toString = toString' 0
