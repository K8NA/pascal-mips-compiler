{
module Parser where
import Lexer
import AST
}

%name parse
%tokentype { Token }
%error { parseError }

--priorities here
%nonassoc '>' '>=' '<' '<=' '<>' '='
%left '+' '-' or
%left '*' '/' '%' and
%left NEG not

%token

program     { PROGRAM }
function    { FUNCTION }
procedure   { PROCEDURE }
const       { CONST }
var         { VAR }
begin       { BEGIN }
end         { END }
if          { IF }
then        { THEN }
else        { ELSE }
while       { WHILE }
do          { DO }
for         { FOR }
to          { TO }
true        { TRUE }
false       { FALSE }
of          { OF }
break       { BREAK }
','         { COMMA }
'.'         { PERIOD }
';'         { SEMICOLON }
':'         { COLON }
'('         { LPAREN }
')'         { RPAREN }
'['         { LBRACKET }
']'         { RBRACKET }
'+'         { PLUS }
'-'         { MINUS }
'*'         { TIMES }
'/'         { DIV }
'%'         { MOD }
'='         { EQUALS }
'<>'        { NOT_EQUALS }
'<'         { LESS_THAN }
'<='        { LESS_OR_EQUAL }
'>'         { GREATER_THAN }
'>='        { GREATER_OR_EQUAL }
and         { AND }
or          { OR }
not         { NOT }
':='        { ASSIGN }
id          { ID $$ }
string      { STRING $$ }
num         { NUM $$ }
integer_t   { TYPE_INTEGER }
string_t    { TYPE_STRING }
bool_t      { TYPE_BOOLEAN }
array_t     { TYPE_ARRAY }



%%

Program : ProgramHeader ProgramBody '.'                                    { Program $1 $2 }

ConstDecls : const ConstDefSeq                                             { $2 }
           |                                                               { [] } 

VarDecls : var VarDefSeq                                                   { $2 }
         |                                                                 { [] }

ConstDef : id '=' num ';'                                                  { ConstDef $1 $3 }

ConstDefSeq : ConstDef ConstDefSeq                                         { $1 : $2}
            | ConstDef                                                     { [$1] }
            
VarDef : id ':' Type ';'                                                   { VarDef $1 $3 }

VarDefSeq : VarDef VarDefSeq                                               { $1 : $2 }
          | VarDef                                                         { [$1] }

Type : BasicType                                                           { Type $1 }  
     | ArrayType                                                           { TypeA $1 }

BasicType : integer_t                                                      { TypeInt }
          | bool_t                                                         { TypeBool }
          | string_t                                                       { TypeString }

ArrayType : array_t '[' Constant '.' '.' Constant ']' of BasicType         { TypeArray $3 $6 $9 }

Constant : num                                                             { ConstNum $1 }
         | id                                                              { ConstId $1 }


Expr : num                                                                 { Num $1 }
     | string                                                              { String $1}
     | true                                                                { Bool True }
     | false                                                               { Bool False }
     | VarAccess                                                           { Var $1 }
     | Expr '+' Expr                                                       { BinOp Add $1 $3 }
     | Expr '-' Expr                                                       { BinOp Sub $1 $3 }
     | Expr '*' Expr                                                       { BinOp Mult $1 $3 }
     | Expr '/' Expr                                                       { BinOp Div $1 $3 }
     | Expr '%' Expr                                                       { BinOp Mod $1 $3 }
     | Expr '=' Expr                                                       { BinOp Equals $1 $3 }
     | Expr '<>' Expr                                                      { BinOp NotEquals $1 $3 }
     | Expr '<' Expr                                                       { BinOp LesserThan $1 $3 }
     | Expr '>' Expr                                                       { BinOp GreaterThan $1 $3 }
     | Expr '<=' Expr                                                      { BinOp LesserOrEqual $1 $3 }
     | Expr '>=' Expr                                                      { BinOp GreaterOrEqual $1 $3 }
     | Expr and Expr                                                       { BinOp And $1 $3 }
     | Expr or Expr                                                        { BinOp Or $1 $3 }
     | '-' Expr %prec NEG                                                  { UnOp Neg $2 }
     | not Expr                                                            { UnOp Not $2 }                                               
     | '(' Expr ')'                                                        { $2 }
     | id '(' ExprList ')'                                                 { FuncCall $1 $3 }

VarAccess : id                                                             { VarSimple $1 }
          | id '[' Expr ']'                                                { VarIndex $1 $3 }

Unop : '-' %prec NEG                                                       { Neg }
     | not                                                                 { Not }    

Binop : '+'                                                                { Add }    
      | '-'                                                                { Sub }
      | '*'                                                                { Mult }
      | '/'                                                                { Div }
      | '%'                                                                { Mod }
      | '='                                                                { Equals }
      | '<>'                                                               { NotEquals }
      | '<'                                                                { LesserThan }
      | '>'                                                                { GreaterThan }
      | '<='                                                               { LesserOrEqual }
      | '>='                                                               { GreaterOrEqual }
      | and                                                                { And }
      | or                                                                 { Or }

ExprList : ExprList1                                                       { $1 }
         |                                                                 { [] }

ExprList1 : Expr ',' ExprList1                                             { $1 : $3 }
          | Expr                                                           { [$1] }


Stm : AssignStm                                                            { $1 }
    | IfStm                                                                { $1 }
    | WhileStm                                                             { $1 }
    | ForStm                                                               { $1 }
    | BreakStm                                                             { $1 }
    | ProcStm                                                              { $1 }
    | CompoundStm                                                          { CompStm $1 }

AssignStm : VarAccess ':=' Expr                                            { Assign $1 $3 }

IfStm : if Expr then Stm                                                   { IfThen $2 $4 }
      | if Expr then Stm else Stm                                          { IfThenElse $2 $4 $6 }

WhileStm : while Expr do Stm                                               { While $2 $4 }

ForStm : for id ':=' Expr to Expr do Stm                                   { For $2 $4 $6 $8 }

BreakStm : break                                                           { Break }

ProcStm : id '(' ExprList ')'                                              { ProcStm $1 $3 }

CompoundStm : begin StmList end                                            { $2 }

StmList : Stm ';' StmList                                                  { $1 : $3 }
        | Stm                                                              { [$1] }



Proc : ProcHeader ProcBody ';'                                             { Proc $1 $2 }

ProcHeader : procedure id '(' ParamList ')' ';'                            { HeaderProc $2 $4 }
           | function id '(' ParamList ')' ':' BasicType ';'               { HeaderFunc $2 $4 $7 }

ProcBody : VarDecls CompoundStm                                            { ProcBody $1 $2 }

ParamList : ParamList1                                                     { $1 }
          |                                                                { [] }

ParamList1 : Param ';' ParamList1                                          { $1 : $3 }
           | Param                                                         { [$1] }

Param : id ':' Type                                                        { Param $1 $3 }


ProgramHeader : program id ';'                                             { ProgId $2 }

ProgramBody : ConstDecls ProcDecls VarDecls CompoundStm                    { ProgBody $1 $2 $3 $4 }

ProcDecls : Proc ProcDecls                                                 { $1 : $2 }
          |                                                                { [] }

{
parseError :: [Token] -> a
parseError toks = error "parse error"
}
