module Interm where

import           AST
import           IR
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Char
import           Control.Monad.State

type Table = Map Ident TableEntry

data TableEntry = Temp Temp           --temp
                | Const Int           -- const
                | Array Int Int Temp
                deriving Show
-- translate an expression
transExpr :: Table -> Expr -> Ident -> State Count [Instr]
transExpr tabl (Var (VarSimple var)) dest
  = case Map.lookup var tabl of
  Just (Temp temp) -> return [MOVE dest temp]
  Just (Const const) -> return [MOVEI dest const]
  Just (Array _ _ temp) -> return [MOVE dest temp] --this is necessary for parameters
  Nothing -> error "undefined variable"

transExpr tabl (Var (VarIndex var expr)) dest
  = case Map.lookup var tabl of
    Just (Array i1 i2 base) -> do addr <- newTemp
                                  code1 <- transIndex tabl i1 base expr addr
                                  popTemp 1
                                  return (code1++[READM dest addr])
    Just _  -> error "bad array access"
    Nothing -> error "undefined array"

transExpr tabl (Num n) dest
  = return [MOVEI dest n]

transExpr tabl (String str) dest =
  do t0 <- newTemp
     t1 <- newTemp
     t2 <- newTemp
     let sizecomputation = [MOVEI t1 ((length str)+1)] --1 byte per char + null terminator
     let alloc = [MALLOC t0 t1]
     let prep = [MOVE t1 t0]
     savestring <- saveString t1 t2 str
     let point = [MOVE dest t0] 
     popTemp 2
     return (sizecomputation ++ alloc ++ prep ++ savestring ++ point)

transExpr tabl (Bool bool) dest =
  case bool of
    True -> return [MOVEI dest 1]
    False -> return [MOVEI dest 0]

transExpr tabl (BinOp rel e1 e2) dest
  | rel == LesserThan || rel == LesserOrEqual || rel == Equals || rel == GreaterThan
  || rel == GreaterOrEqual || rel == NotEquals || rel == And || rel == Or =
      do ltrue <- newLabel
         lfalse <- newLabel
         lend <- newLabel
         code1 <- transCond tabl (BinOp rel e1 e2) ltrue lfalse
         return (code1 ++ [LABEL ltrue, MOVEI dest 1, JUMP lend, LABEL lfalse, MOVEI dest 0, LABEL lend])

transExpr tabl (BinOp op e1 e2) dest =
  do temp1 <- newTemp
     temp2 <- newTemp
     code1 <- transExpr tabl e1 temp1
     code2 <- transExpr tabl e2 temp2
     popTemp 2
     return (code1 ++ code2 ++ [OP op dest temp1 temp2])


transExpr tabl (UnOp Neg e1) dest =
  do temp1 <- newTemp
     code1 <- transExpr tabl e1 temp1
     popTemp 1
     return (code1 ++ [OPI Mult dest temp1 (-1)])

transExpr tabl (UnOp Not e1) dest =
  do ltrue <- newLabel
     lfalse <- newLabel
     lend <- newLabel
     code <- transCond tabl (UnOp Not e1) ltrue lfalse
     return (code ++ [LABEL ltrue, MOVEI dest 1, JUMP lend, LABEL lfalse, MOVEI dest 0, LABEL lend])

transExpr tabl (FuncCall "writeint" [e1]) dest =
  do t0 <- newTemp
     code <- transExpr tabl e1 t0
     popTemp 1
     return ([CALL dest "writeint" [t0]])

transExpr tabl (FuncCall "readint" []) dest =
  return ([CALL dest "readint" []])

transExpr tabl (FuncCall "writestr" [str1]) dest =
  do t0 <- newTemp 
     code <- transExpr tabl str1 t0
     popTemp 1
     return ([CALL dest "writestr" [t0]])

transExpr tabl (FuncCall id args) dest =
  do (code, temps) <- transArgs tabl args
     popTemp (length args)
     return (code ++ [CALL dest id temps])

transArgs tabl args = worker args
  where
    worker []  = return ([], [])
    worker (exp:exps)
      = do temp <- newTemp
           code <- transExpr tabl exp temp
           (code', temps') <- worker exps
           return (code++code', temp:temps')

saveString :: Temp -> Temp -> String -> State Count [Instr]
saveString t1 t2 "" = 
  do let holdzero = [MOVEI t2 0] --null terminator
     return (holdzero ++ [WRITEMB t1 t2])
saveString t1 t2(ch:str) = 
  do let holdchar = [MOVEI t2 (ord ch)]
     let storecode = [WRITEMB t1 t2]
     let movetemp = [OPI Add t1 t1 1] --next char
     code <- saveString t1 t2 str
     return (holdchar ++ storecode ++ movetemp ++ code) 

--translate a statement
transStm :: Table -> Stm -> Maybe Label -> State Count [Instr]
transStm tabl (Assign (VarSimple var) expr) lbreak
  = case Map.lookup var tabl of
      Nothing -> error "undefined variable"
      Just (Temp dest) -> do temp <- newTemp
                             code <- transExpr tabl expr temp
                             popTemp 1
                             return (code ++ [MOVE dest temp])
      Just t -> error "bad var in Assign Stm"

transStm tabl (Assign (VarIndex var indexpr) expr) lbreak
  = case Map.lookup var tabl of
    Nothing -> error "undefined array"
    Just (Array i1 i2 base) -> do addr <- newTemp
                                  t1 <- newTemp
                                  code0 <- transIndex tabl i1 base indexpr addr
                                  code1 <- transExpr tabl expr t1
                                  popTemp 2
                                  return (code0 ++ code1 ++ [WRITEM addr t1])
    Just t -> error "bad array in Assign Stm"

transStm tabl (IfThen cond stm1) lbreak =
  do ltrue  <- newLabel
     lfalse <- newLabel
     code1  <- transCond tabl cond ltrue lfalse
     code2  <- transStm tabl stm1 lbreak
     return (code1 ++ [LABEL ltrue] ++
             code2 ++ [LABEL lfalse])

transStm tabl (IfThenElse cond stm1 stm2) lbreak =
  do ltrue  <- newLabel
     lfalse <- newLabel
     lend  <- newLabel
     code0 <- transCond tabl cond ltrue lfalse
     code1 <- transStm tabl stm1 lbreak
     code2 <- transStm tabl stm2 lbreak
     return (code0 ++ [LABEL ltrue] ++ code1 ++
             [JUMP lend, LABEL lfalse] ++ code2 ++ [LABEL lend])

transStm tabl (While cond stm) lbreak =
  do lbody <- newLabel
     lend  <- newLabel
     lcond <- newLabel
     code1 <- transStm tabl stm (Just lend)
     code2 <- transCond tabl cond lbody lend
     return ([JUMP lcond, LABEL lbody] ++ code1 ++
             [LABEL lcond] ++ code2 ++ [LABEL lend])


transStm tabl (For var e1 e2 stm) lbreak =
  do t0 <- newTemp
     t1 <- newTemp
     t2 <- newTemp
     lloop <- newLabel
     lend <- newLabel
     lcond <- newLabel
     let tabl' = Map.insert var (Temp t0) tabl
     code1 <- transExpr tabl' e1 t1
     code2 <- transExpr tabl' e2 t2
     code3 <- transStm tabl' stm (Just lend)
     popTemp 3
     return (code1 ++ code2 ++ [MOVE t0 t1, JUMP lcond, LABEL lloop] ++
             code3 ++ [OPI Add t0 t0 1] ++ code2 ++
             [LABEL lcond, COND t0 LesserOrEqual t2 lloop lend, LABEL lend])

transStm tabl (ProcStm id args) lbreak =
  do t0 <- newTemp
     (code, temps) <- transArgs tabl args
     popTemp ((length args) + 1)
     return (code ++ [CALL t0 id temps])

transStm tabl (CompStm []) lbreak =
  do return []

transStm tabl (CompStm (stm:stms)) lbreak =
  do code1 <- transStm tabl stm lbreak
     code2 <- transStm tabl (CompStm stms) lbreak
     return (code1 ++ code2)

transStm tabl (Break) lbreak =
  case lbreak of
    Nothing -> error "Break outside of loop"
    Just l -> return [JUMP l]

--translate a condition

transCond :: Table -> Expr -> Label -> Label -> State Count [Instr]
transCond tabl (BinOp rel e1 e2) ltrue lfalse
  | rel == LesserThan || rel == LesserOrEqual || rel == Equals ||
    rel == GreaterThan || rel == GreaterOrEqual || rel == NotEquals =
      do temp1 <- newTemp
         temp2 <- newTemp
         code1 <- transExpr tabl e1 temp1
         code2 <- transExpr tabl e2 temp2
         popTemp 2
         return ( code1 ++ code2 ++
                  [COND temp1 rel temp2 ltrue lfalse] )
transCond tabl (Bool True) ltrue lfalse
  = return [JUMP ltrue]
transCond tabl (Bool False) ltrue lfalse
  = return [JUMP lfalse]
transCond tabl (UnOp Not c1) ltrue lfalse =
  do code1 <- transCond tabl c1 lfalse ltrue
     return code1
transCond tabl (BinOp And c1 c2) ltrue lfalse =
  do label2 <- newLabel
     code1 <- transCond tabl c1 label2 lfalse
     code2 <- transCond tabl c2 ltrue lfalse
     return (code1 ++ [LABEL label2] ++ code2)
transCond tabl (BinOp Or c1 c2) ltrue lfalse =
  do label2 <- newLabel
     code1 <- transCond tabl c1 ltrue label2
     code2 <- transCond tabl c2 ltrue lfalse
     return (code1 ++ [LABEL label2] ++ code2)
transCond tabl (FuncCall id exprs) ltrue lfalse =
  do t0 <- newTemp
     t1 <- newTemp
     code <- transExpr tabl (FuncCall id exprs) t0
     popTemp 2
     return (code ++ [MOVEI t1 1, COND t0 Equals t1 ltrue lfalse])
transCond tabl (Var (VarSimple var)) ltrue lfalse =
  case Map.lookup var tabl of
    Nothing -> error ("undefined variable!!" ++ var)
    Just (Temp t0) -> do t1 <- newTemp
                         popTemp 1
                         return [MOVEI t1 1, COND t1 Equals t0 ltrue lfalse]

--translate an index of an array

transIndex :: Table -> Int -> Temp -> Expr -> Temp -> State Count [Instr]
transIndex tabl firstindex base expr addr =
  do code1 <- transExpr tabl expr addr
     let offset = firstindex*4
     return (code1 ++ [OPI Mult addr addr 4, OP Add addr addr base, OPI Sub addr addr offset])

--Constants and Variables

transConstDefs :: Table -> [ConstDef] -> State Count Table
transConstDefs tabl [] = return tabl
transConstDefs tabl ((ConstDef id n):consts) =
  do let tabl' = Map.insert id (Const n) tabl
     tabl'' <- transConstDefs tabl' consts
     return tabl''

transVarDefs :: Table -> [VarDef] -> Int -> State Count ([Instr],Table, Int)
transVarDefs tabl [] count = return ([],tabl, count)
transVarDefs tabl (VarDef id ((TypeA (TypeArray c1 c2 bt))):vardefs) count =
  do t0 <- newTemp
     t1 <- newTemp
     let tabl' = defArray tabl id (TypeArray c1 c2 bt) t0
     (code1,tabl'',countf) <- transVarDefs tabl' vardefs (count+1)
     let n1 = constVal tabl c1
     let n2 = constVal tabl c2
     let codeIndex = [MOVEI t1 n2, OPI Sub t1 t1 n1, OPI Add t1 t1 1, OPI Mult t1 t1 4]
     let code2 = [MALLOC t0 t1]
     popTemp 1
     return (codeIndex++code2++code1,tabl'',countf)
transVarDefs tabl ((VarDef id ty):vardefs) count =
  do t0 <- newTemp
     let tabl' = Map.insert id (Temp t0) tabl
     (code,tabl'',countf) <- transVarDefs tabl' vardefs (count+1)
     return (code,tabl'',countf)

defArray :: Table -> Ident -> ArrayType -> Temp -> Table
defArray tabl id (TypeArray c1 c2 _) t0 = Map.insert id (Array (constVal tabl c1) (constVal tabl c2) t0) tabl

constVal :: Table -> Const -> Int
constVal tabl (ConstNum n) = n
constVal tabl (ConstId  x) =
  case Map.lookup x tabl of
    Just (Const n) -> n
    Just t -> error "bad constant"
    Nothing -> error "undefined constant"

--Procedures and Functions

transProcs :: Table -> [Proc] -> State Count [Function]
transProcs tabl [] =
  do return []
transProcs tabl (p1:ps) =
  do code1 <- transProc tabl p1
     code2 <- transProcs tabl ps
     return (code1 : code2)

transProc :: Table -> Proc -> State Count Function
transProc tabl (Proc (HeaderProc id params) (ProcBody vardefs stms)) =
  do t0 <- newTemp
     (tabl', temps, countp) <- transParams tabl params 0
     (code0,tabl'', countv) <- transVarDefs tabl' vardefs 0
     code1 <- transStm tabl'' (CompStm stms) Nothing
     popTemp (countp+countv+1)
     return (Function id temps (code0++code1++[RETURN t0])) --returning 0 just to know it ended, I don't think this is necessary, but the CALL instruction needs a temp

transProc tabl (Proc (HeaderFunc id params bt) (ProcBody vardefs stms)) =
  do t0 <- newTemp
     let tabl' = Map.insert id (Temp t0) tabl
     (tabl'', temps, countp) <- transParams tabl' params 0
     (code0,tabl''',countv) <- transVarDefs tabl'' vardefs 0
     code1 <- transStm tabl''' (CompStm stms) Nothing
     popTemp (countp+countv+1)
     return (Function id temps (code0++code1++[RETURN t0]))

transParams :: Table -> [Param] -> Int -> State Count (Table, [Temp], Int)
transParams tabl [] count = return (tabl, [], count)
transParams tabl ((Param id (TypeA (TypeArray c1 c2 _))):params) count =
  do t0 <- newTemp
     let tabl' = Map.insert id (Array (constVal tabl c1) (constVal tabl c2) t0) tabl
     (tabl'',temps,count') <- transParams tabl' params (count+1)
     return (tabl'',(t0:temps),count')
transParams tabl ((Param id t):params) count =
  do t0 <- newTemp
     let tabl' = Map.insert id (Temp t0) tabl
     (tabl'', temps, count') <- transParams tabl' params (count+1)
     return (tabl'', (t0:temps), count')


--translate a program
transProgBody :: Table -> ProgBody -> Ident -> State Count ([Function])
transProgBody tabl (ProgBody consts procs vars stms) id =
  do tabl1 <- transConstDefs tabl consts
     codeprocs <- transProcs tabl1 procs
     (code2,tabl2,c) <- transVarDefs tabl1 vars 0
     code3 <- transStm tabl2 (CompStm stms) Nothing
     let codemain = Function "main" [] (code2 ++ code3)
     return (codeprocs ++ [codemain])

transProgram :: Program -> State Count Prog
transProgram (Program (ProgId id) body) =
  do code <- transProgBody Map.empty body id
     return (Prog id code)

------------------------------------------------------------------------------

type Count = (Int,Int)  -- contadores para temporários e etiquetas

newTemp :: State Count Temp
newTemp = do (t,l) <- get; put (t+1,l); return ("t"++show t)

popTemp :: Int -> State Count ()
popTemp k = modify (\(t,l) -> (t-k,l))

newLabel :: State Count Label
newLabel = do (t,l) <- get; put (t,l+1); return ("L"++show l)
------------------------------------------------------------------------------
