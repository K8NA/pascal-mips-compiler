module Main where

import Lexer
import Parser
import Interm
import TypeCheck
import Control.Monad.State
import PrettyPrint
import IR
import CodeGen
import System.Environment

main = do
  [input] <- getArgs
  txt <- readFile input
  let p = parse $ alexScanTokens txt

  if checkProgram p then
    do
        let (Prog id interm, c) = runState (transProgram p) (0,0)
        printFunctions interm
        let asm = genProg interm
        writeFile (input ++ ".mips") (unlines asm)
  else error "type error"
