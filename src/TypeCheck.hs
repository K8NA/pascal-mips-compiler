module TypeCheck where

import AST

import Data.Map(Map)
import qualified Data.Map as Map

type TypeEnv = Map Ident Type

--Expr

checkExpr :: TypeEnv -> Expr -> Type
checkExpr env (Num n) = (Type TypeInt)
checkExpr env (String s) = (Type TypeString)
checkExpr env (Bool b) = (Type TypeBool)
checkExpr env (Var (VarSimple var)) = case Map.lookup var env of
    Nothing -> error "undeclared variable [Exp]"
    Just t -> t
checkExpr env (Var (VarIndex arr i)) = case Map.lookup arr env of
    Nothing -> error "undeclared array"
    Just (TypeA (TypeArray c1 c2 t)) ->
              case (checkExpr env i) of
              (Type TypeInt) -> (Type t)
              otherwise -> error "error in array index"
-- Num to Num operations
checkExpr env (BinOp Add e1 e2) =
    let t1 = checkExpr env e1
        t2 = checkExpr env e2
    in if t1 == (Type TypeInt) && t2 == (Type TypeInt) then (Type TypeInt)
       else error "type error in +"
checkExpr env (BinOp Sub e1 e2) =
    let t1 = checkExpr env e1
        t2 = checkExpr env e2
    in if t1 == (Type TypeInt) && t2 == (Type TypeInt) then (Type TypeInt)
       else error "type error in -"
checkExpr env (BinOp Mult e1 e2) =
    let t1 = checkExpr env e1
        t2 = checkExpr env e2
    in if t1 == (Type TypeInt) && t2 == (Type TypeInt) then (Type TypeInt)
       else error "type error in *"
checkExpr env (BinOp Div e1 e2) =
    let t1 = checkExpr env e1
        t2 = checkExpr env e2
    in if t1 == (Type TypeInt) && t2 == (Type TypeInt) then (Type TypeInt)
       else error "type error in div"
checkExpr env (BinOp Mod e1 e2) =
    let t1 = checkExpr env e1
        t2 = checkExpr env e2
    in if t1 == (Type TypeInt) && t2 == (Type TypeInt) then (Type TypeInt)
       else error "type error in mod"
--Num to Bool operations
checkExpr env (BinOp Equals e1 e2) =
    let t1 = checkExpr env e1
        t2 = checkExpr env e2
    in if t1 == (Type TypeInt) && t2 == (Type TypeInt) then (Type TypeBool)
       else error "type error in ="
checkExpr env (BinOp NotEquals e1 e2) =
    let t1 = checkExpr env e1
        t2 = checkExpr env e2
    in if t1 == (Type TypeInt) && t2 == (Type TypeInt) then (Type TypeBool)
       else error "type error in <>"
checkExpr env (BinOp LesserThan e1 e2) =
    let t1 = checkExpr env e1
        t2 = checkExpr env e2
    in if t1 == (Type TypeInt) && t2 == (Type TypeInt) then (Type TypeBool)
       else error "type error in <"
checkExpr env (BinOp LesserOrEqual e1 e2) =
    let t1 = checkExpr env e1
        t2 = checkExpr env e2
    in if t1 == (Type TypeInt) && t2 == (Type TypeInt) then (Type TypeBool)
       else error "type error in <="
checkExpr env (BinOp GreaterThan e1 e2) =
    let t1 = checkExpr env e1
        t2 = checkExpr env e2
    in if t1 == (Type TypeInt) && t2 == (Type TypeInt) then (Type TypeBool)
       else error "type error in >"
checkExpr env (BinOp GreaterOrEqual e1 e2) =
    let t1 = checkExpr env e1
        t2 = checkExpr env e2
    in if t1 == (Type TypeInt) && t2 == (Type TypeInt) then (Type TypeBool)
       else error "type error in >="
--Bool to Bool operations
checkExpr env (BinOp And e1 e2) =
    let t1 = checkExpr env e1
        t2 = checkExpr env e2
    in if t1 == (Type TypeBool) && t2 == (Type TypeBool) then (Type TypeBool)
       else error "type error in and"
checkExpr env (BinOp Or e1 e2) =
    let t1 = checkExpr env e1
        t2 = checkExpr env e2
    in if t1 == (Type TypeBool) && t2 == (Type TypeBool) then (Type TypeBool)
       else error "type error in or"
--Unary operations
checkExpr env (UnOp Neg e1) =
    let t1 = checkExpr env e1
    in if t1 == (Type TypeInt) then (Type TypeInt)
       else error "type error in -"
checkExpr env (UnOp Not e1) =
    let t1 = checkExpr env e1
    in if t1 == (Type TypeBool) then (Type TypeBool)
       else error "type error in not"
checkExpr env (FuncCall f args) =
    let ts = map (checkExpr env) args
    in case Map.lookup f env of
        Just (TypeFunc ts' t) ->
            if ts == ts' then (Type t)
            else error "type error in function arguments"
        _ -> error "invalid function name"

checkStm :: TypeEnv -> Stm -> Bool
checkStm env (Assign (VarSimple var) exp) =
    case Map.lookup var env of
        Just (TypeFunc tyargs tyret) -> let (Type t2) = checkExpr env exp
                                in if tyret == t2 then True
                                           else error "test error"
        Just t1 -> let t2 = checkExpr env exp
                   in if t1 == t2 then True
                      else error "error in assignment to var"
        Nothing -> error "undeclared variable [Stm]"
checkStm env (Assign (VarIndex arr index) exp) =
    case Map.lookup arr env of
        Just (TypeA (TypeArray c1 c2 t1)) -> let (Type t2) = checkExpr env exp
                                             in if t1 == t2 then True
                                                else error "type error in assignment to array"
        Nothing -> error "undeclared array [Stm]"
checkStm env (IfThen cond stm) =
    let t1 = checkExpr env cond
    in if t1 == (Type TypeBool) then
        checkStm env stm
       else error "type error in if: conditions should be bool"
checkStm env (IfThenElse cond stm1 stm2) =
    let t1 = checkExpr env cond
    in if t1 == (Type TypeBool) then
        (checkStm env stm1) && (checkStm env stm2)
       else error "type error in if: conditions should be bool"
checkStm env (While cond stm) =
    let t1 = checkExpr env cond
    in if t1 == (Type TypeBool) then
        checkStm env stm
       else error "type error in while: conditions should be bool"
checkStm env (For var e1 e2 stm) =
    case Map.lookup var env of
        Nothing -> error "undeclared variable in for loop"
        Just t1 -> let t2 = checkExpr env e1
                       t3 = checkExpr env e2
                   in if (t1==t2 && t1==t3) then
                       checkStm env stm
                      else error "type error in for loop expressions"
checkStm env (Break) = True
checkStm env (ProcStm p exprs) =
    let ts = map (checkExpr env) exprs
    in case Map.lookup p env of
        Just (TypeProc ts') ->
            if ts == ts' then True
            else error "type error in procedure arguments"
        Nothing -> error "undefined procedure"
checkStm env (CompStm stms) =
    let ts = map (checkStm env) stms
    in (and ts)

extendEnvParams :: TypeEnv -> [Param] -> TypeEnv
extendEnvParams env [] = env
extendEnvParams env ((Param id t):rest) = extendEnvParams (Map.insert id t env) rest

extendEnvVarDefs :: TypeEnv -> [VarDef] -> TypeEnv
extendEnvVarDefs env [] = env
extendEnvVarDefs env ((VarDef id t):rest) = extendEnvVarDefs (Map.insert id t env) rest

getTypeParam :: Param -> Type
getTypeParam (Param id t) = t

checkProcBody :: TypeEnv -> ProcBody -> Bool
checkProcBody env (ProcBody vardefs stms) =
    let env' = extendEnvVarDefs env vardefs
    in (and (map (checkStm env') stms))

extendEnvConstDef :: TypeEnv -> [ConstDef] -> TypeEnv
extendEnvConstDef env [] = env
extendEnvConstDef env ((ConstDef v t):rest) = extendEnvConstDef (Map.insert v (Type TypeInt) env) rest

extendEnvProcHeader :: TypeEnv -> ProcHeader -> TypeEnv
extendEnvProcHeader env (HeaderProc name params) = 
    let tyargs = map (getTypeParam) params
        env' = Map.insert name (TypeProc tyargs) env
    in env'
extendEnvProcHeader env (HeaderFunc name params tyreturn) =
    let tyargs = map (getTypeParam) params
        env' = Map.insert name (TypeFunc tyargs tyreturn) env
    in env'

extendEnvProcs :: TypeEnv -> [Proc] -> TypeEnv
extendEnvProcs env [] = env
extendEnvProcs env ((Proc (HeaderProc name params) body):rest) =
    let tyargs = map (getTypeParam) params
        env' = extendEnvProcHeader env (HeaderProc name params)
        env'' = extendEnvParams env' params
        checkbody = checkProcBody env'' body
    in if checkbody then extendEnvProcs env' rest
       else error "type error in procedure body"
extendEnvProcs env ((Proc (HeaderFunc name params tyreturn) body):rest) =
    let tyargs = map (getTypeParam) params
        env' = extendEnvProcHeader env (HeaderFunc name params tyreturn)
        env'' = extendEnvParams env' params
        checkbody = checkProcBody env'' body
    in if checkbody then extendEnvProcs env' rest
       else error "type error in function body"

checkProgBody :: ProgBody -> Bool
checkProgBody (ProgBody constdefs procs vardefs stms) =
    let startenv = Map.fromList [("writeint",TypeProc [Type TypeInt]),("readint",(TypeFunc [] TypeInt)),("writestr", TypeProc [Type TypeString])]
        env = extendEnvConstDef startenv constdefs
        env' = extendEnvProcs env procs
        env'' = extendEnvVarDefs env' vardefs
    in (and (map (checkStm env'') stms))

checkProgram :: Program -> Bool
checkProgram (Program header body) = checkProgBody body
