--abstract syntax tree

module AST where

type Ident = String

data Expr = Num Int
          | String String
          | Bool Bool
          | Var VarAccess
          | BinOp BinOp Expr Expr
          | UnOp UnOp Expr
          | FuncCall Ident [Expr]
          deriving Show

data VarAccess = VarSimple Ident 
               | VarIndex Ident Expr
               deriving Show

data BinOp = Add
           | Sub
           | Mult
           | Div 
           | Mod 
           | Equals
           | NotEquals
           | LesserThan
           | GreaterThan
           | LesserOrEqual
           | GreaterOrEqual
           | And
           | Or 
           deriving (Eq, Show)

data UnOp = Neg
          | Not 
          deriving Show

data Stm = Assign VarAccess Expr 
         | IfThen Expr Stm
         | IfThenElse Expr Stm Stm
         | While Expr Stm
         | For Ident Expr Expr Stm
         | Break
         | ProcStm Ident [Expr]
         | CompStm [Stm]
         deriving Show

data ConstDef = ConstDef Ident Int
              deriving Show  

data VarDef = VarDef Ident Type
            deriving Show

data Type = Type BasicType 
          | TypeA ArrayType
          | TypeFunc [Type] BasicType
          | TypeProc [Type]
          deriving (Eq, Show)

data BasicType = TypeInt | TypeBool | TypeString
               deriving (Eq, Show)

data ArrayType = TypeArray Const Const BasicType
               deriving (Eq, Show)

data Const = ConstNum Int
           | ConstId Ident
           deriving (Eq, Show)

data Proc = Proc ProcHeader ProcBody
          deriving Show

data ProcHeader = HeaderProc Ident [Param]
                | HeaderFunc Ident [Param] BasicType
                deriving Show

data ProcBody = ProcBody [VarDef] [Stm]
              deriving Show

data Param = Param Ident Type
           deriving Show

data Program = Program ProgHeader ProgBody
             deriving Show

data ProgHeader = ProgId Ident
                deriving Show

data ProgBody = ProgBody [ConstDef] [Proc] [VarDef] [Stm]
              deriving Show
