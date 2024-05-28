{- Test for Program -}
module TestProgram where

import Program

p0, p1, p2, p3, p4, p5, p8 :: Program.T
p0 =
  fromString
    "\
    \-- 3\n\
    \read k;\
    \-- 16\n\
    \read n;\
    \m := 1;\
    \while n-m do\
    \  begin\
    \    if m - m/k*k then\
    \      skip;\
    \    else\
    \      write m;\
    \    m := m + 1;\
    \  end"
p1 =
  fromString
    "\
    \-- 1024\n\
    \read n;\
    \-- 2\n\
    \read b;\
    \m := 1;\
    \s := 0;\
    \p := 1;\
    \while n do\
    \  begin\
    \    q := n/b;\
    \    r := n - q*b;\
    \    write r;\
    \    s := p*r+s;\
    \    p := p*10;\
    \    n :=q;\
    \  end\
    \write s;"

s1 =
  "\
  \read n;\
  \read b;\
  \m := 1;\
  \s := 0;\
  \p := 1;\
  \while n do\
  \  begin\
  \    q := n/b;\
  \    r := n - q*b;\
  \    write r;\
  \    s := p*r+s;\
  \    p := p*10;\
  \    n :=q;\
  \  end\
  \write s;"

sp = putStr (toString p0)

sp1 = putStr (toString p1)

p2 = fromString (toString p0)

p3 = fromString (toString p1)

rp0 = Program.exec p0 [3, 16]

rp1 = Program.exec p1 [1024, 2]

rp2 = Program.exec p2 [3, 16]

rp3 = Program.exec p3 [4, 4]

s4 =
  "\
  \read a;\
  \read b;\
  \-- a comment\n\
  \s := 3;\
  \while a do\
  \  begin\
  \    c := a^s;\
  \    d := 2^a;\
  \    write c;\
  \    write d;\
  \    a := a-1;\
  \  end\
  \write a;"

p4 = fromString s4

rp4 = Program.exec p4 [4, 4]

p5 = fromString (toString p4)

rp5 = Program.exec p5 [4, 4]

s6 =
  "\
  \c := 2^3^4;\
  \write c;"

p6 = fromString s6

rp6 = Program.exec p6 []

p7 = fromString (toString p6)

rp7 = Program.exec p7 []

s8 =
  "\
  \read a;\
  \read -- input a value\n\
  \     -- into variable b:\n\
  \     b;\
  \a := 3;\
  \write a;\
  \write b;"

p8 = fromString s8

rp8 = Program.exec p8 [5, 6]