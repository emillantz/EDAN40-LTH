-- Axel Froborg (ax3051fr-s) and Emil Lantz (em0377la-s)

module Program (T, parse, fromString, toString, exec) where

import Dictionary qualified
import Parser hiding (T)
import Statement qualified
import Prelude hiding (fail, return)

newtype T = Program [Statement.T]

instance Parse T where
  parse = iter Statement.parse >-> Program
  toString (Program p) = concatMap Statement.toString p

exec (Program p) = Statement.exec p Dictionary.empty