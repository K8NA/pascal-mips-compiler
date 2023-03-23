{

module Lexer where
import Data.Char

}

%wrapper "basic"

-- Regex Expressions
$white = [\ \t\n\r\v]
$digit = [0-9]
$alpha = [_a-zA-Z]
$anyandnl = [\n.]

tokens :-

$white ; --ignore white space

--comments
"(*" (($anyandnl # \* ) | ( \* $anyandnl # \) ) )* "*)" ;

--making all reserved words case insensitive

[Pp][Rr][Oo][Gg][Rr][Aa][Mm]              { \_ -> PROGRAM }
[Ff][Uu][Nn][Cc][Tt][Ii][Oo][Nn]          { \_ -> FUNCTION }
[Pp][Rr][Oo][Cc][Ee][Dd][Uu][Rr][Ee]      { \_ -> PROCEDURE }
[Cc][Oo][Nn][Ss][Tt]                      { \_ -> CONST }
[Vv][Aa][Rr]                              { \_ -> VAR }
[Bb][Ee][Gg][Ii][Nn]                      { \_ -> BEGIN }
[Ee][Nn][Dd]                              { \_ -> END }
[Ii][Ff]                                  { \_ -> IF}
[Tt][Hh][Ee][Nn]                          { \_ -> THEN }
[Ee][Ll][Ss][Ee]                          { \_ -> ELSE }
[Ww][Hh][Ii][Ll][Ee]                      { \_ -> WHILE }
[Dd][Oo]                                  { \_ -> DO }
[Ff][Oo][Rr]                              { \_ -> FOR}
[Tt][Oo]                                  { \_ -> TO }
[Tt][Rr][Uu][Ee]                          { \_ -> TRUE }
[Ff][Aa][Ll][Ss][Ee]                      { \_ -> FALSE }
[Dd][Ii][Vv]                              { \_ -> DIV }
[Mm][Oo][Dd]                              { \_ -> MOD }
[Ii][Nn][Tt][Ee][Gg][Ee][Rr]              { \_ -> TYPE_INTEGER }
[Bb][Oo][Oo][Ll][Ee][Aa][Nn]              { \_ -> TYPE_BOOLEAN }
[Ss][Tt][Rr][Ii][Nn][Gg]                  { \_ -> TYPE_STRING }
[Aa][Rr][Rr][Aa][Yy]                      { \_ -> TYPE_ARRAY }
[Oo][Ff]                                  { \_ -> OF }
[Bb][Rr][Ee][Aa][Kk]                      { \_ -> BREAK }

--punctuation
","     { \_ -> COMMA }
"."     { \_ -> PERIOD }
";"     { \_ -> SEMICOLON }
":"     { \_ -> COLON }
"("     { \_ -> LPAREN }
")"     { \_ -> RPAREN }
"["     { \_ -> LBRACKET }
"]"     { \_ -> RBRACKET }

--operators along with div and mod
"+"     { \_ -> PLUS }
"-"     { \_ -> MINUS }
"*"     { \_ -> TIMES }
"="     { \_ -> EQUALS }
"<>"    { \_ -> NOT_EQUALS }
"<"     { \_ -> LESS_THAN }
"<="    { \_ -> LESS_OR_EQUAL }
">"     { \_ -> GREATER_THAN }
">="    { \_ -> GREATER_OR_EQUAL }
"and"   { \_ -> AND }
"or"    { \_ -> OR }
"not"   { \_ -> NOT }
":="    { \_ -> ASSIGN }

--numeral [always an integer]
$digit+                 { \s -> NUM (read s)}
--string
\'[. # \']*\'                  { \s -> STRING (tail (init s)) } --this removes the '
--identifiers
$alpha($alpha|$digit)*  { \s -> ID (mylower s) } --saves all letters as lowercase so x=X

{
data Token
    = PROGRAM
    | FUNCTION
    | PROCEDURE
    | CONST
    | VAR
    | BEGIN
    | END
    | IF
    | THEN
    | ELSE
    | WHILE
    | DO
    | FOR
    | TO
    | TRUE
    | FALSE
    | DIV
    | MOD
    | TYPE_INTEGER
    | TYPE_BOOLEAN
    | TYPE_STRING
    | TYPE_ARRAY
    | OF
    | BREAK
    | COMMA
    | PERIOD
    | SEMICOLON
    | COLON
    | LPAREN
    | RPAREN
    | LBRACKET
    | RBRACKET
    | PLUS
    | MINUS
    | TIMES
    | EQUALS
    | NOT_EQUALS
    | LESS_THAN
    | LESS_OR_EQUAL
    | GREATER_THAN
    | GREATER_OR_EQUAL
    | AND
    | OR
    | NOT
    | ASSIGN
    | NUM Int
    | STRING String
    | ID String
    deriving (Eq, Show)

mylower :: String -> String
mylower = map toLower
}
