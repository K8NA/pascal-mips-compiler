module CodeGen where

import IR
import AST
import Data.Map as Map

genInstrs :: [Instr] -> [String]
genInstrs [] = []
genInstrs ((MOVE t0 t1) : rest) 
    = ["move $" ++ (tTemp t0) ++ ", $" ++ (tTemp t1)]      ++ (genInstrs rest)
genInstrs ((MOVEI t0 n) : rest) 
    = ["addiu $" ++ (tTemp t0) ++ ", $zero, " ++ (show n)] ++ (genInstrs rest)

genInstrs ((OP Add dest t0 t1) : rest) 
    = ["addu $"  ++ (tTemp dest) ++ ", $" ++ (tTemp t0) ++ ", $" ++ (tTemp t1)] 
       ++ (genInstrs rest)
genInstrs ((OPI Add dest t0 n) : rest)
    = ["addiu $" ++ (tTemp dest) ++ ", $" ++ (tTemp t0) ++ ", "  ++ (show n)]   
       ++ (genInstrs rest)

genInstrs ((OP Sub dest t0 t1) : rest) 
    = ["subu $"  ++ (tTemp dest) ++ ", $" ++ (tTemp t0) ++ ", $" ++ (tTemp t1)] 
       ++ (genInstrs rest)
genInstrs ((OPI Sub dest t0 n) : rest) 
    = ["subiu $" ++ (tTemp dest) ++ ", $" ++ (tTemp t0) ++ ", "  ++ (show n)]   
       ++ (genInstrs rest)

genInstrs ((OP Mult dest t0 t1) : rest) 
    = ["mul $" ++ (tTemp dest) ++ ", $" ++ (tTemp t0) ++ ", $" ++ (tTemp t1)] 
       ++ (genInstrs rest)
genInstrs ((OPI Mult dest t0 n) : rest) 
    = ["mul $" ++ (tTemp dest) ++ ", $" ++ (tTemp t0) ++ ", "  ++ (show n)]   
       ++ (genInstrs rest)

genInstrs ((OP Div dest t0 t1) : rest) 
    = ["div $" ++ (tTemp dest) ++ ", $" ++ (tTemp t0) ++ ", $" ++ (tTemp t1)] 
      ++ (genInstrs rest)
genInstrs ((OPI Div dest t0 n) : rest) 
    = ["div $" ++ (tTemp dest) ++ ", $" ++ (tTemp t0) ++ ", "  ++ (show n)]   
      ++ (genInstrs rest)

genInstrs ((OP Mod dest t0 t1) : rest)
    = ["div $" ++ (tTemp t0) ++ ", $" ++ (tTemp t1), "mfhi $" ++ (tTemp dest)] 
      ++ (genInstrs rest)
genInstrs ((OPI Mod dest t0 n) : rest) 
    = ["div $" ++ (tTemp t0) ++ ", "  ++ (show n),   "mfhi $" ++ (tTemp dest)] 
      ++ (genInstrs rest)

genInstrs ((LABEL l) : rest)
    = [l ++ ": "] ++ (genInstrs rest)
genInstrs ((JUMP l)  : rest) 
    = ["j " ++ l] ++ (genInstrs rest)

genInstrs ((COND t0 Equals t1 lt lf) :((LABEL l) : rest))
    | lf == l = ["beq $" ++ (tTemp t0) ++ ", $" ++ (tTemp t1) ++ " " ++ lt, lf ++ ":"]
                 ++ (genInstrs rest)
    | lt == l = ["bne $" ++ (tTemp t0) ++ ", $" ++ (tTemp t1) ++ " " ++ lf, lt ++ ":"] 
                 ++ (genInstrs rest)
genInstrs ((COND t0 Equals t1 lt lf) : rest)
    = ["beq $" ++ (tTemp t0) ++ ", $" ++ (tTemp t1) ++ ", " ++ lt, "j " ++ lf] 
       ++ (genInstrs rest)

genInstrs ((COND t0 LesserThan t1 lt lf) : ((LABEL l) : rest))
    | lf == l = ["blt $" ++ (tTemp t0) ++ ", $" ++ (tTemp t1) ++ ", " ++ lt, lf ++ ":"]
                 ++ (genInstrs rest)
    | lt == l = ["bge $" ++ (tTemp t0) ++ ", $" ++ (tTemp t1) ++ ", " ++ lf, lt ++ ":"]
                 ++ (genInstrs rest)
genInstrs ((COND t0 LesserThan t1 lt lf) : rest)
    = ["blt $" ++ (tTemp t0) ++ ", $" ++ (tTemp t1) ++ ", " ++ lt, "j " ++ lf] 
       ++ (genInstrs rest)

genInstrs ((COND t0 GreaterThan t1 lt lf) : rest)           -- t0 > t1 => t1 < t0
    = genInstrs ((COND t1 LesserThan t0 lt lf) : rest)                                                         
genInstrs ((COND t0 GreaterOrEqual t1 lt lf) : rest)        -- t0 >= t1 => !(t0 < t1)
    = genInstrs ((COND t0 LesserThan t1 lf lt) : rest)
genInstrs ((COND t0 LesserOrEqual t1 lt lf) : rest )        -- t0 <= t1 => !(t0 > t1) => !(t1 < t0)
    = genInstrs ((COND t1 LesserThan t0 lf lt) : rest)

genInstrs ((COND t0 NotEquals t1 lt lf) : rest)
    = ["bne $" ++ t0 ++ ", $" ++ t1 ++ ", " ++ lt ++ ", " ++ lf]
       ++ (genInstrs rest)

genInstrs ((CALL t0 "writeint" [t1]) : rest)
    = ["li $v0, 1","add $a0, $" ++ (tTemp t1) ++ ", $zero","syscall", --print number
       "li $v0, 11","li $a0, 32","syscall"]                           --print a space
       ++ (genInstrs rest)
genInstrs ((CALL t0 "readint" []) : rest)
    = ["li $v0, 5","syscall","add $" ++ (tTemp t0) ++ ", $v0, $zero"]
       ++ (genInstrs rest)
genInstrs ((CALL t0 "writestr" [t1]) : rest)
    = ["li $v0, 4","la $a0, ($" ++ t1 ++ ")","syscall"]
       ++ (genInstrs rest)
--writestr not implemented

genInstrs ((CALL t0 funid args) : rest)
    = (callCode t0 funid args) ++ (genInstrs rest)
--aux func

genInstrs ((RETURN t0) : rest)
    = ["move $v0, $" ++ (tTemp t0),
       "move $sp, $fp",
       "lw $ra, -8($sp)",
       "lw $fp, -4($sp)",
       "jr $ra"]

genInstrs ((READM t0 t1)  : rest) 
    = ["lw $" ++ (tTemp t0) ++ ", 0($" ++ (tTemp t1) ++ ")"]
       ++ (genInstrs rest)
genInstrs ((WRITEM t0 t1) : rest) 
    = ["sw $" ++ (tTemp t1) ++ ", 0($" ++ (tTemp t0) ++ ")"]
       ++ (genInstrs rest)
genInstrs ((WRITEMB t0 t1) : rest)
    = ["sb $" ++ (tTemp t1) ++ ", 0($" ++ (tTemp t0) ++ ")"]
       ++ (genInstrs rest)

genInstrs ((MALLOC t0 t1) : rest) 
    = ["sub $sp, $sp, $" ++ (tTemp t1),
       "move $" ++ (tTemp t0) ++ ", $sp"] 
       ++ (genInstrs rest)

callCode :: Temp -> Ident -> [Temp] -> [String]
callCode t0 id args 
    = saveregs ++ storeargs ++ growstack ++ jumplink ++ shrinkstack ++ restoreregs ++ saveresult
    where saveregs   = ["sw $t" ++ (show i) ++ ", " ++ (show (-4*(i+1)) ++ "($sp)")
                        | i <- [0..9]] ++
                       ["sw $s" ++ (show i) ++ ", " ++ (show (-4*(i+11)) ++ "($sp)")
                        | i <- [0..7]] ++
                       ["addiu $sp, $sp, -72"]
          storeargs   = ["sw $" ++ (args!!((length args)-i-1)) ++ ", " ++ show(-4*(1+i)) ++ "($sp)"
                         | i <- [0..(length args)-1]]
          growstack   = ["addiu $sp, $sp, " ++ (show (-4*(length args)))]
          jumplink    = ["jal " ++ id]
          shrinkstack = ["addiu $sp, $sp, " ++ (show ( 4*(length args)))]
          restoreregs = ["addiu $sp, $sp, 72"] ++
                        ["lw $t" ++ (show i) ++ ", " ++ (show (-4*(1+i)) ++ "($sp)")
                         | i <- [0..9]] ++
                        ["lw $s" ++ (show i) ++ ", " ++ (show (-4*(i+11)) ++ "($sp)")
                         | i <- [0..7]]
          saveresult  = ["move $" ++ (tTemp t0) ++ ", $v0"]
--not sure how to save temps or if we should

storeArgs :: [Temp] -> [String]
storeArgs args = ["sw $" ++ (args!!((length args)-i-1)) ++ ", " ++ show(-4*(1+i)) ++ "($sp)"
                   | i <- [0..(length args)-1]]



genFunc :: Function -> [String]
genFunc (Function "main" [] instrs) = ["main:"] ++ (genInstrs instrs)
genFunc (Function id args instrs)
    = labelid ++ savefpra ++ setupfpsp ++ loadargs ++ funcode
    where labelid   = [id ++ ":"]
          savefpra  = ["sw $fp, -4($sp)","sw $ra, -8($sp)"]
          setupfpsp = ["move $fp, $sp","addiu $sp, $sp, " ++ show (-4*((length args)+2))] --space for args plus old fp sp
          loadargs  = ["lw $t" ++ (show (i+1)) ++ ", " ++ (show (4*i)) ++ "($fp)" 
                      | i <- [0..((length args)-1)]]  -- using temps as args
          funcode   = (genInstrs instrs)

genFuncs :: [Function] -> [String]
genFuncs [] = []
genFuncs (f:fs) = (genFunc f) ++ (genFuncs fs)

genProg :: [Function] -> [String]
genProg funs = opener ++ (genFuncs funs) ++ closer
             where opener = ["j main"]
                   closer = ["li $v0, 10","syscall"]

tTemp :: Temp -> String
tTemp t = m!t
        where m = Map.fromList ([("t" ++ (show i),"t" ++ (show i)) | i <- [0..9]] ++ [("t" ++ (show (i+10)), "s" ++ (show i)) | i <- [0..7]])

