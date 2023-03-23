module IR (Instr(..), BinOp(..), Temp, Label, Prog(..), Function(..)) where

import AST (BinOp(..), Ident(..))


type Temp  = String
type Label = String

data Prog = Prog Ident [Function]
          deriving (Eq, Show)

data Function = Function Ident [Temp] [Instr]
              deriving (Eq, Show)

data Instr = MOVE Temp Temp
           | MOVEI Temp Int
           | OP BinOp Temp Temp Temp
           | OPI BinOp Temp Temp Int
           | LABEL Label
           | JUMP Label
           | COND Temp BinOp Temp Label Label
           | CALL Temp Ident [Temp]         -- t:=CALL f(temps)
           | RETURN Temp
           | READM Temp Temp                -- puts whatever is in the address in t2 into t1
           | WRITEM Temp Temp               -- writes whatever is in t2 into the address in t1
           | WRITEMB Temp Temp              -- like above but only for writing a byte for strings
           | MALLOC Temp Temp               -- alloc t1 ptr t2 num bytes | t1 = malloc t2
           deriving (Eq, Show)
