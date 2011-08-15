{-# OPTIONS -XExistentialQuantification #-}
{-# OPTIONS -XScopedTypeVariables #-}
{-# OPTIONS -XGADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Rank2Types #-}
{-

SEE readme for terms of use

-}

module JSGen where

import Types
import Data.Maybe
import Data.Either
import Data.List
import Data.Char
import Control.Applicative hiding (Const)
import Control.Monad.State
import Language.HJavaScript.Syntax as JS
import Debug.Trace

data JS = JS {
                jsScope :: Scope,
                jsSequence :: Integer,          -- sequence (for loop indexes)
                jsLoopIndex :: Integer          -- current loop index
            }

type JSS a = State JS a

getJSScope :: JSS Scope
getJSScope = do
    jss <- get
    return $ jsScope jss

saveJSState :: JSS JS
saveJSState = get

restoreJSState :: JS -> JSS ()
restoreJSState jss = do                     -- restore stuff, except sequence
    savedSequence <- jsSequence <$> get
    put $ jss { jsSequence = savedSequence }
    return ()

preservingJSState act = do
    sv <- saveJSState
    retval <- act
    restoreJSState sv
    return retval

generateJS :: [Event] -> Block ()
generateJS ((Event (EventProgramStart) scope):es) =

    let s = JS scope 1000 0
        ((decls, remains), s') = runState (parseDecls es []) s
        ((body, []),s'') = runState (parseBody remains) s'
        blk = toJSBlock (decls, body) :: Block ()
    in blk

doCommentStmt :: String -> Stmt ()
doCommentStmt comment =
    ExpStmt $ JConst $ "/*  -------- " ++ comment ++ "*/"


getVariableNS (Event (EventVariable site nm _ _) scope) = getVariableName nm site scope

getVariableName nm (UnitScope sco) scope = sco ++ "$" ++ nm
getVariableName nm (FunctionRetVal) scope = "_rt_retval"
getVariableName nm (WithScope ws) scope = ws ++ "." ++ nm
getVariableName nm _ _  = nm

parseDecls :: [Event] -> [Stmt ()] -> JSS ( [Stmt ()], [Event] )

parseDecls ((Event (EventVariable _ _ _ (Just (Expr (SomeCode _) [] _))) _):es) acc =
    parseDecls es acc   -- skip "procedure as a var"

parseDecls ((Event (EventVariable FunctionArgScope _ _ _) _):es) acc =
    parseDecls es acc   -- skip "procedure as a var"

parseDecls ((Event (EventVariable _ _ (Just (TypeRefProcedure _ _ _)) _) _):es) acc =
    parseDecls es acc   -- skip "procedure as a var"

parseDecls (ev@(Event v@(EventVariable _ nm typ Nothing) scope):es) acc =
        parseDecls es $
                (JS.VarDeclAssign (getVariableNS ev) (initialValue (show v) scope typ )) :
        -- : (doCommentStmt $ show typ)
                acc

parseDecls (ev@(Event (EventVariable _ nm typ (Just expr)) scope):es) acc = parseDecls es $
        -- let typ' = trace ( "EventVariable: "++(show ("name=",nm,"typ=",typ, "expr=",expr))) typ
        case convertExpr scope typ expr of
            Just (REExpr exp) -> (JS.VarDeclAssign (getVariableNS ev) exp)
                        : acc
        -- : (doCommentStmt $ show typ)

parseDecls ((Event (EventFunctionStart site nm proc@(TypeRefProcedure arg mtyp _)) scope):es) acc = do
        (decls, remains) <- parseDecls es []
        (body, rem2) <- parseBody remains
        let  namespace = compiledName $ fromJustX "parseDecl1" $ scopeCurrentUnit scope
        parseDecls rem2 $
            (ExpStmt $ JConst ((getVariableName nm site scope) ++ ".RT$INFO = { firstArgIsVar: "++jsBoolean (arg /= [] && isOutArgument (head arg))++"}"))
            :
            (VarDeclAssign (getVariableName nm site scope) $ JConst $ "function" ++ (showArgs arg) ++ "{" ++ show (toJSBlock (decls, body)) ++ "}")
            :
            acc
-- otherwise

parseDecls es acc = return $ (reverse acc, es)

jsBoolean True = "true"
jsBoolean _ = "false"

produceStatement :: Statement -> JSS (Stmt ())

produceStatement (AssignSt e1 e2) = do
    scope <- getJSScope
    (e1',e2') <- transformAssignmentCasts (e1, e2)
    let left = show $ transformExpression scope e1'
    let right = show $ transformExpression scope e2'
    return $ ExpStmt $ JConst $ left ++ " = " ++ right

produceStatement (IfSt e1 (CodeBlock []) b2) = do
    scope <- getJSScope
    cb <- transformCodeBlock b2
    return $ ExpStmt $ JConst $ "if (!(" ++ (show $ transformExpression scope e1) ++ ")) { "
            ++ cb ++ " }"
produceStatement (IfSt e1 b1 (CodeBlock []) ) = do
    scope <- getJSScope
    cb <- transformCodeBlock b1
    return $ ExpStmt $ JConst $ "if (" ++ (show $ transformExpression scope e1) ++ ") { "
            ++ cb ++ " }"
produceStatement (IfSt e1 b1 b2) = do
    scope <- getJSScope
    cb2 <- transformCodeBlock b2
    cb1 <- transformCodeBlock b1
    return $ ExpStmt $ JConst $ "if (" ++ (show $ transformExpression scope e1) ++ ") { " ++ cb1 ++ " } else { "
            ++ cb2 ++ " }"

produceStatement (EvalSt (Expr (VarRef "BREAK") [] _)) = do
    loopBreaker <- getLoopBreaker;
    return $ ExpStmt $ JConst $ "{ "++loopBreaker++"=false; /* break-stmt */ continue; }"

produceStatement (EvalSt e1) = do
    scope <- getJSScope
    return $ ExpStmt $ JConst $ (show $ transformExpression scope e1)

produceStatement  e@(WithSt [] blk) = do
    scope <- getJSScope
    (ExpStmt . JConst) <$> transformCodeBlock blk

produceStatement e@(WithSt (exp:es) blk) = do
    scope <- getJSScope
    let ET etype _ = getExprType scope exp
        ok = isRecord etype
    if not ok
        then error $ "withst: not all record types: "++show (etype,e)
        else do
            stmt <- preservingJSState $ do
                augmentScope exp
                produceStatement (WithSt es blk)
            return $ ExpStmt $ JConst $
            -- "with (" ++ (show $ transformExpression scope exp)++ ") {" ++
                    (show $ stmt)
              --      ++ "}"
    where
        augmentScope exp = do
            scope <- getJSScope
            js <- get
            let ET (TypeRefRecord lst) _ = getExprType scope exp
                SymbolScope ss = scopeSymbols scope
                newsyms = map (\(nm,tr) -> (nm,(WithScope (show $ transformExpression scope exp),Just tr, Nothing))) lst
            put $ js { jsScope = scope { scopeSymbols = SymbolScope (newsyms ++ ss) }}




produceStatement e@(ForSt varr from downto to blk) = do
    scope <- getJSScope
    generateLoop $ do
        loopBreaker <- getLoopBreaker
        let ET t1 _ = getExprType scope from
            ET t2 _ = getExprType scope to
            ET varrt _ = getExprType scope varr
            from' = castExpr scope from varrt
            to' = castExpr scope to varrt
            (symComp, symCrement) = case not downto of
                    False -> ("<=","++")
                    True -> (">= ","--")
        codeBlockS <-
            case blk of
                CodeBlock [] -> return ";"
                _ -> do
                        cb <- transformCodeBlock blk
                        return $ "{" ++ cb ++ "}"
        let retval = case (from', to') of
                        (Right frE, Right toE) -> ExpStmt $ JConst $
                            "for (" ++ (show $ transformExpression scope varr) ++ " = "
                                ++ (show $ transformExpression scope frE) ++ "; " ++
                                loopBreaker ++ "&& (" ++ (show $ transformExpression scope varr) ++ symComp ++
                                (show $ transformExpression scope toE) ++ ");" ++
                                (show $ transformExpression scope varr) ++ symCrement  ++ ")" ++ codeBlockS
                        _ -> error $ "for stmt: expressions don't cast: " ++ show e
        return retval

produceStatement  e@(CaseSt cond cases defBlk) = do
    scope <- getJSScope
    let ET condType _ = getExprType scope cond
        getCodeBlock blk = case blk of
                                CodeBlock [] -> return $ ";"
                                _ -> do
                                            cb <- transformCodeBlock blk
                                            return $ "{" ++ cb ++ " break; }"
    defaultCodeBlock <- getCodeBlock defBlk
    let
        produceCase (caseLabels, blk) = do
            cblk <- getCodeBlock blk
            lblz <- mapM produceLabel caseLabels
            return $ (concat lblz) ++ cblk
        produceLabel e@(Expr Range [] _) = error $ "Ranges are not supported for the case stmt" ++ (show e)
        produceLabel label = return $ "case " ++ (show $ transformExpression scope label) ++ ":"
    checkedCases <- concat <$> mapM produceCase cases
    let allCasesS = checkedCases ++ "default: " ++ defaultCodeBlock
    return $ case condType of
        _ | isIntegerType condType || isChar condType -> ExpStmt $ JConst $
                    "switch (" ++(show $ transformExpression scope cond) ++ ") { " ++ allCasesS ++ "}"
          | otherwise -> error $ "case stmt: case expression is not char or integer: " ++ show (condType,cond)

produceStatement  e@(WhileSt cond blk) = do
    scope <- getJSScope
    generateLoop $ do
        loopBreaker <- getLoopBreaker
        cb <- transformCodeBlock blk
        let ET t1 _ = getExprType scope cond
            codeBlockS =
                case blk of
                    CodeBlock [] -> ";"
                    _ -> "{" ++ cb ++ "}"
        return $ case t1 of
            (TypeRefNative "boolean") -> ExpStmt $ JConst $
                        "while (" ++ loopBreaker ++ "&& ("++(show $ transformExpression scope cond) ++ "))" ++ codeBlockS
            _ -> error $ "whilest stmt: expressions don't cast: " ++ show t1

produceStatement e@(RepeatSt blk cond) = do
    scope <- getJSScope
    generateLoop $ do
        loopBreaker <- getLoopBreaker
        cb <- transformCodeBlock  blk
        let ET t1 _ = getExprType scope cond
            codeBlockS =
                case blk of
                    CodeBlock [] -> " {}"
                    _ -> "{" ++ cb ++ "}"
            keepCond = "(" ++ (show $ transformExpression scope cond) ++")"
        return $ case t1 of

            (TypeRefNative "boolean") -> ExpStmt $ JConst $
                        "while ( " ++ loopBreaker ++") { // repeat-until \n "++ codeBlockS ++"; if ("++keepCond++") break;}"
            _ -> error $ "repeat stmt: expressions don't cast: " ++ show t1


produceStatement  stmt =
    return $ ExpStmt $ JConst $ "NOTIMPL(/* " ++(take 20 $ show stmt) ++" */)"


transformAssignmentCasts (lvalue@(Expr FunCall ((Expr (VarRef fn) [] _):arg:[]) _),rvalue) = do
    scope <- getJSScope
    let TypeScope initial = initialTypeScope
    case lookup fn initial  of
        Just (_,trn) -> do
            let ET rt _ = getExprType scope rvalue
                ET lt _ = getExprType scope arg
            case (castExpr scope rvalue trn, lt) of
                (Right e, TypeRefNative nativeName) -> do
                    let wrappedRight = Expr FunCall [Expr (VarRef (map toUpper nativeName)) [] noPosition,rvalue] noPosition
                    return $ traceval "transformAssignmentCasts: " $ (arg, wrappedRight)
                    --error $  "all ok: "++show (lvalue,rvalue,trn,rt,lt)
                    -- error $  "all ok: "++show (arg, wrappedRight)
                (Left e,_) -> error $  "strange lvalue, does not cast.."++show (lvalue,rvalue,trn,rt)
        _ -> error $ "lvalue: not likely" ++ show lvalue

transformAssignmentCasts (lvalue,rvalue) =
    return (lvalue,rvalue)

generateLoop act = do
    js <- saveJSState
    modify (\o -> o { jsSequence = jsSequence o + 1, jsLoopIndex = jsSequence js} )
    loopBreaker <- getLoopBreaker
    retval <- act
    restoreJSState js
    return $
        case retval of
            ExpStmt (JConst str) -> ExpStmt $ JConst $
                            "var "++loopBreaker++" = true;" ++
                                str
            _ -> error "generate loop: bad case"

getLoopBreaker = do
    ix <- jsLoopIndex <$> get
    if ix <= 0
        then error "loopBreaker: called not inside loop"
        else return $ "loopBreaker" ++ (show ix)




transformCodeBlock :: CodeBlock -> JSS String
transformCodeBlock (CodeBlock []) = return $ ""
transformCodeBlock (CodeBlock stmts) = do
        stmts' <- mapM (produceStatement) stmts
        return $ (concat $ intersperse ";" $ map show stmts') ++ ";"


parseBody :: [Event] -> JSS ([Stmt ()], [Event])
parseBody ((Event (EventCodeBlock nm (CodeBlock stmts)) scope):es) = do
    modify (\o -> o { jsScope = scope })
    pbody <-
        mapM produceStatement (stmts ++
                                                    if (scopeHasRetVal scope || collectOutArgs scope /= [])
                                                        then [EvalSt $ Expr (VarRef "EXIT") [] noPosition]
                                                        else []
                                                )
    return (pbody, es)
        -- ExpStmt $ JConst "return"

parseBody (e:es) = error $ "expecting body, got: "++show e

parseBody [] = return $ ([doCommentStmt "empty BODY!"],[])

showArgs args = "(" ++ (concat $ intersperse "," $ map show1arg args) ++ ")"
show1arg (Argument _ nm _) = nm


toJSBlock :: ([Stmt ()],[Stmt ()]) -> Block ()
toJSBlock (decls,bs) = makeBlock $ decls ++ bs
   where makeBlock sx = foldl (\b s -> Sequence b s) (EmptyBlock) sx

renderJS b = renderBlock b

initialValue :: String -> Scope -> (Maybe TypeRef) -> Exp [Char]
initialValue var _ Nothing = error $ "no initial value for non-typed var: "++var
initialValue var _ (Just (TypeRefNative "boolean")) = JConst "false"
initialValue var _ (Just (TypeRefNative "word")) = JConst "0"
initialValue var _ (Just (TypeRefNative "integer")) = JConst "0"
initialValue var _ (Just (TypeRefNative "shortint")) = JConst "0"
initialValue var _ (Just (TypeRefNative "longint")) = JConst "0"
initialValue var _ (Just (TypeRefNative "real")) = JConst "0"
initialValue var _ (Just (TypeRefNative "byte")) = JConst "0"
initialValue var _ (Just (TypeRefNative "char")) = JConst "'\\0'"
initialValue var _ (Just (TypeRefNative "string")) = JConst "''"
initialValue var _ (Just (TypeRefNative "file")) = JConst "new RT_File()"
initialValue var _ (Just (TypeRefNative "pointer")) = JConst "null"
initialValue var _ (Just (TypeRefSet _ )) = JConst "RT_setof()"
initialValue var _ (Just (TypeRefPointer _)) = JConst "null"
initialValue var _ (Just (TypeRefSoftPointer _)) = JConst "null"
initialValue var _ (Just (TypeRefFixedString _)) = JConst "''"
initialValue var _ (Just (TypeRefProcedure _ _ _)) = JConst "function() { /* default value for function variable */ }"
initialValue var scope (Just (TypeRefRecord pairs)) = createRecordInitializer scope pairs
initialValue var scope (Just tra@(TypeRefArray range typ)) = createArrayInitializer scope range typ Nothing
initialValue var _ (Just tr) = JString $ ("initial for: "++show tr)

data ArrayIndex = ArrayIndexInt | ArrayIndexChar | ArrayIndexEnum TypeRef  deriving (Show)
data ArrayRange = ArrayRange ArrayIndex Integer Integer       deriving (Show)

requireEqualTypes scope t1 t2 =
    if typeEquals scope t1 t2 then True else error ("types are not equal: "++show (t1, t2))

resolveSoftType scope (TypeRefSoftPointer nm) =
    let (TypeScope syms) = scopeTypes scope
    in case lookup nm syms of
        Nothing -> error $ "TypeRefSoftPointer: symbol not found: "++nm
        Just (_,typ) -> TypeRefPointer typ
resolveSoftType _ t = t

typeEquals scope t1 t2 =
    let t1' =  resolveSoftType scope t1
        t2' =  resolveSoftType scope t2
    in typeEquals0 scope t1' t2' ||  typeEquals0 scope t2' t1'
typeEquals0 scope (TypeRefPointer _) TypeRefAnyPointer = True
typeEquals0 scope t1 t2 = t1 == t2


cast = toEnum . fromEnum

getRangeInfo0 scope r =
    case r of
       (Expr Range [r1, r2] _) ->
        let
            ET t1 v1 = getExprType scope r1
            ET t2 v2 = getExprType scope r2
        in if requireEqualTypes scope t1 t2
            then if (t1 == typeInteger && isJust v1 && isJust v2)
                    then ArrayRange ArrayIndexInt (fromJustX "gri0-1" $ getIntConst v1) (fromJustX "gri0-2"$ getIntConst v2)
                    else if (t1 == typeString && isJust v1 && isJust v2 && (length <$> getStringConst v1) == Just 1 && (length <$> getStringConst v2) == Just 1)
                       then ArrayRange ArrayIndexChar (cast $ head $ fromJustX "gri0-3" $ getStringConst v1) (cast $ head $ fromJustX "gri0-4" $ getStringConst v1)
                       else error $ "range must be char or integer"
            else error $ "range: end and start are not of same type"

getRangeInfo scope r =
    let ri@(ArrayRange _ a b) = getRangeInfo0 scope r
    in if b > a
            then ri
            else error "array range: indexes are not ordered well"


createArrayInitializer :: Scope -> Expr -> TypeRef -> (Maybe Expr) -> Exp [Char]
createArrayInitializer scope r typ mv =
    let s = case mv of
                    Nothing -> show $ initialValue "" scope (Just typ)
                    Just fillVal -> show $ transformExpression scope fillVal
        ArrayRange _ a b  = {- traceval "rangeInfo=" $ -} getRangeInfo scope r
        all = "RT_replicate(" ++ show (b - a + 1) ++ ", "++s++")"
        --concat $ intersperse "," $ take (fromEnum (b - a + 1)) $ repeat s
    in JConst all


createRecordInitializer scope pairs =
    JConst $ "{" ++ (concat $ intersperse "," $ map (\(n,t) -> n ++ ": "++(show $ initialValue "" scope $ Just t)) pairs) ++ "}"

showRE :: ResultExpr -> String
showRE (REExpr e) = show e
showRE (RENativeCall) = "callNative()"
showRE (REConstCode s) = s


doBinaryOp :: forall a . Maybe a -> Maybe a -> (a -> a -> a) -> Maybe a
doBinaryOp (Just x) (Just y) f = Just (f x y)
doBinaryOp _ _ _= Nothing

toDouble :: Num a => a -> Double
toDouble = read . show

toInt :: Num a => a -> Integer
toInt = read . show


data ExprType = ET TypeRef (Maybe Expr)
    deriving (Eq, Show)

etGetType (ET tr _) = tr

rotateList (l:ls) = ls ++ [l]

getExprType scope e =
    let ET t v = getExprType0 scope e
    in ET (resolveSoftType scope t) v

getExprType0 scope e@(Expr (Const ( ConstString s)) [] _) = ET typeString (Just e)
getExprType0 scope e@(Expr (Const ( ConstDouble d)) [] _) = ET typeDouble (Just e)
getExprType0 scope e@(Expr (Const ( ConstInteger _)) [] _) = ET typeInteger (Just e)
getExprType0 scope e@(Expr (Const ( ConstBool _)) [] _) = ET typeBoolean (Just e)
getExprType0 scope e@(Expr (VarRef "INC") _ _) = ET TypeRefVoid Nothing
getExprType0 scope e@(Expr (VarRef "DEC") _ _) = ET TypeRefVoid Nothing
getExprType0 scope e@(Expr (VarRef nm) [] _) =
    let (SymbolScope syms) = scopeSymbols scope
    in case lookup nm syms of
        Nothing
            | elem nm $ getAllUses (fromJustX "getExprType0/scopeCurrentUnit" $ scopeCurrentUnit scope) -> ET TypeRefUnitScope Nothing
            | otherwise -> error $ "findVarType: symbol not found: "++show (nm,e) ++ (take 1000 $ showScope syms)
        Just (_, Nothing, Just expr) -> getExprType scope expr
        Just (_, Just tr, Just val) ->
            let (ET et2 const) = getExprType scope val
            in ET tr const
        Just (_, Just (TypeRefProcedure [] (Just tr) _), Nothing) -> ET tr Nothing
        Just (_, Just tr, Nothing) ->
            ET tr Nothing
    where
        getAllUses (CompiledUnit (Unit {unitInterfaceDecls = publd, unitImplDecls = privd} )) = collectUses (publd ++ privd)
        getAllUses (CompiledProgram (Program {programImplDecls = publd} )) = collectUses (publd)


getExprType0 scope (Expr NE [e1,e2] pose) = getExprType0 scope (Expr Types.Equals [e1,e2] pose)
getExprType0 scope (Expr Types.Equals [e1,e2] _) =
    let (ET t1 _, ET t2 _) = (getExprType scope e1,getExprType scope e2)
    in case castExpr scope e2 t1 of
        Right e2' -> ET typeBoolean Nothing
        Left err -> error $ "getExprtype0 equals: not compatible types: "++show (t1, t2, e1, e2)

getExprType0 scope (Expr Add [e1,e2] _) =
    let (ET t1 v1, ET t2 v2) = (getExprType scope e1,getExprType scope e2)
    in if (isStringOrChar t1 && isStringOrChar t2) then ET typeString $ makeStringConst $ doBinaryOp (getStringConst v1) (getStringConst v2) (++)
     else if (isIntegerType t1 && isIntegerType t2) then ET typeInteger $ makeIntConst $ doBinaryOp (getIntConst v1) (getIntConst v2) (+)
     else if (t1 == typeDouble && isFloatingType t2) then ET typeDouble $ makeDoubleConst $ doBinaryOp (getDoubleConst v1) (getDoubleConst v2) (+)
     else if (isIntegerType t1 && isFloatingType t2) then ET typeDouble $ makeDoubleConst $ doBinaryOp (toDouble <$> getIntConst v1) (getDoubleConst v2) (+)
     else if (t1 == typeDouble && isIntegerType t2) then ET typeDouble $ makeDoubleConst $ doBinaryOp (getDoubleConst v2) (toDouble <$> getIntConst v1) (+)
     else error $ "unable to detect type of Add for " ++ show (t1, t2, e1, e2)
getExprType0 scope (Expr Sub [e1,e2] _) =
    let (ET t1 v1, ET t2 v2) = (getExprType scope e1,getExprType scope e2)
    in if (isIntegerType t1 && isIntegerType t2) then ET typeInteger $ makeIntConst $ doBinaryOp (getIntConst v1) (getIntConst v2) (-)
     else if (isFloatingType t1 && isFloatingType t2) then ET typeDouble $ makeDoubleConst $ doBinaryOp (getDoubleConst v1) (getDoubleConst v2) (-)
     else if (isIntegerType t1 && isFloatingType t2) then ET typeDouble $ makeDoubleConst $ doBinaryOp (toDouble <$> getIntConst v1) (getDoubleConst v2) (-)
     else if (isFloatingType t1 && isIntegerType t2) then ET typeDouble $ makeDoubleConst $ doBinaryOp (getDoubleConst v2) (toDouble <$> getIntConst v1) (-)
     else error $ "unable to detect type of Sub for " ++ show (t1, t2, e1, e2)
getExprType0 scope e@(Expr Mul [e1,e2] _) =
    let (ET t1 v1, ET t2 v2) = (getExprType scope e1,getExprType scope e2)
    in if (isIntegerType t1 && isIntegerType t2) then ET typeInteger $ makeIntConst $ doBinaryOp (getIntConst v1) (getIntConst v2) (*)
     else if (isFloatingType t1 && isFloatingType t2) then ET typeDouble $ makeDoubleConst $ doBinaryOp (getDoubleConst v1) (getDoubleConst v2) (*)
     else if (isIntegerType t1 && isFloatingType t2) then ET typeDouble $ makeDoubleConst $ doBinaryOp (toDouble <$> getIntConst v1) (getDoubleConst v2) (*)
     else if (isFloatingType t1 && isIntegerType t2) then ET typeDouble $ makeDoubleConst $ doBinaryOp (getDoubleConst v2) (toDouble <$> getIntConst v1) (*)
     else error $ "unable to detect type of Mul for " ++ show (t1, t2, e)
getExprType0 scope (Expr Types.Div [e1,e2] _) =
    let (ET t1 v1, ET t2 v2) = (getExprType scope e1,getExprType scope e2)
    in if (isIntegerType t1 && isIntegerType t2) then ET typeDouble $ makeDoubleConst $ doBinaryOp (toDouble <$> getIntConst v1) (toDouble <$> getIntConst v2) (/)
     else if (isFloatingType t1 && isFloatingType t2) then ET typeDouble $ makeDoubleConst $ doBinaryOp (getDoubleConst v1) (getDoubleConst v2) (*)
     else if (isIntegerType t1 && isFloatingType t2) then ET typeDouble $ makeDoubleConst $ doBinaryOp (toDouble <$> getIntConst v1) (getDoubleConst v2) (*)
     else if (isFloatingType t1 && isIntegerType t2) then ET typeDouble $ makeDoubleConst $ doBinaryOp (getDoubleConst v1) (toDouble <$> getIntConst v2) (*)
     else error $ "unable to detect type of Div for " ++ show (t1, t2)
getExprType0 scope e@(Expr Types.IDiv [e1,e2] _) =
    let (ET t1 v1, ET t2 v2) = (getExprType scope e1,getExprType scope e2)
    in if (isIntegerType t1 && isIntegerType t2) then ET typeInteger $ makeIntConst $ doBinaryOp (getIntConst v1) (getIntConst v2) (div)
     else if (isFloatingType t1 && isIntegerType t2) then ET typeInteger $ makeIntConst $ doBinaryOp (toInt <$> getDoubleConst v1) (getIntConst v2) (div)
     else if (isIntegerType t1 && isFloatingType t2) then ET typeInteger $ makeIntConst $ doBinaryOp (getIntConst v1) (toInt <$> getDoubleConst v2) (div)
     else if (isFloatingType t1 && isFloatingType t2) then ET typeInteger $ makeIntConst $ doBinaryOp (toInt <$> getDoubleConst v1) (toInt <$> getDoubleConst v2) (div)
     else error $ "unable to  type of IDiv for " ++ show (t1, t2, e1, e2)
getExprType0 scope (Expr Shl [e1,e2] _) =
    let (ET t1 v1, ET t2 v2) = (getExprType scope e1,getExprType scope e2)
    in if (isIntegerType t1 && isIntegerType t2) then ET typeInteger $ makeIntConst $ doBinaryOp (getIntConst v1) (getIntConst v2) (myShl)
     else error $ "unable to  type of SHL for " ++ show (t1, t2)
    where myShl a b = a * (2 ^ b)
getExprType0 scope (Expr PropertyExtr [l,r] _) =
    let ET et _ = getExprType scope l
    in case (et,r) of
        (TypeRefUnitScope, _) -> getExprType0 (keepOnlyUnitScopeSymbols scope) r
        (TypeRefRecord pairs, Expr (VarRef nm) [] _) ->
            case lookup nm pairs of
                Just tr -> ET tr Nothing
                Nothing -> error $ "property not found in record, "++show( l,  r)
        _ -> error $ "property extraction on non-record, or by non-name, "++showN ("*** left=",l,"\n *** left type=",et,"\n *** right=",r)

getExprType0 scope ex@(Expr SetValue [] _) = ET (TypeRefSet $ Right TypeRefVoid) Nothing
getExprType0 scope ex@(Expr SetValue (e:es) _) =
    let ET et _ = getExprType scope e
    in case et of
        TypeRefNative "string" -> ET (TypeRefSet $ Right typeChar) Nothing
        TypeRefNative "integer" -> ET (TypeRefSet $ Right $ typeByte) Nothing
        _ -> error $ "getExprType0: SetValue: " ++ show (et, ex)


getExprType0 scope (Expr ListValue [] _) = error $ "getExprType0 for list value of zero length"
getExprType0 scope e@(Expr ListValue lst epos) =
    let len = length lst
        first = Expr (Const $ ConstInteger 0) [] noPosition
        last = Expr (Const $ ConstInteger (toInteger $ len-1)) [] noPosition
        -- ET t _ = getExprType scope $ head lst
        -- castExpr scope
    in
        case getAnyType lst of
            Just es ->
                let t = etGetType $ getExprType scope $ head es
                in ET (TypeRefArray (Expr Range [first,last] epos) t) Nothing
            Nothing ->
                error $ "list initializer: different types in list: " ++
                    showN ("\ntypes=",map (etGetType . (getExprType scope)) lst,
                        "\nlst=",lst)
    where
        getAnyType :: [Expr] -> Maybe [Expr]  -- returns casted to same type, or zero length list
        getAnyType lst =
            let len = length lst
            in case catMaybes $ map (coerces) $ take len $ iterate rotateList lst  of
                (x:_) -> Just x
                _ -> Nothing
        coerces :: [Expr] -> Maybe [Expr]
        coerces lst =
            let t = etGetType $ (getExprType scope) $ head lst
                casted = map (\e -> castExpr scope e t) lst
            in if all isNameValue (map (etGetType . (getExprType scope)) lst) then Just $ [head lst]
                else if all isRight casted then Just $ map getRight casted
                else Nothing
        isNameValue (TypeRefNameValue _ _) = True
        isNameValue _ = False

getExprType0 scope (Expr FunCall ((Expr (VarRef "NEW") _ _) :args) _) = error "getExprType0: NEW"

getExprType0 scope (Expr FunCall (name:args) _) =
    let ET funt _ = getExprType scope name
    in case funt of
        TypeRefProcedure _ Nothing _ -> ET TypeRefVoid Nothing
        TypeRefProcedure _ (Just typ) _ -> ET typ Nothing
        _ -> error $ "getExprType0: found symbol, but it is not a procedure: "++(show name)

getExprType0 scope nil@(Expr (Const ConstNil) [] _) = ET (TypeRefAnyPointer) (Just nil)
getExprType0 scope (Expr ColonOp [n,v] _) =
    let
        ET vt val = getExprType scope v
    in case n of
        Expr (VarRef nm) [] _ -> ET (TypeRefNameValue nm vt) val
        _ -> error $ "Record initializer: strange thing on the left of colon: "++(show n)

-- last one!
getExprType0 scope e@(Expr PointerDeref [ptr] _) =
    let ET pt _ = getExprType scope ptr
    in case pt of
        TypeRefPointer t -> ET t Nothing
        TypeRefAnyPointer -> ET TypeRefVoid Nothing
        TypeRefNative "pointer" -> ET TypeRefVoid Nothing
        ex -> error $ "getExprType0  dereferencing non pointer? " ++ showN ("type=",pt,"\n ** ex=",ex, "\n ** expr=",e)

getExprType0 scope e@(Expr ArrayDeref [arr,ix] _) =
    let ET at _ = getExprType scope arr
        ET it _ = getExprType scope ix
    in case at of
        TypeRefArray e tr -> ET tr Nothing
        TypeRefNative "string" -> ET typeChar Nothing
        TypeRefPointer (TypeRefNative "char") -> ET typeChar Nothing
        ex -> error $ "getExprType0: ArrayDeref: non-array, non-pchar: "++showN ("arrExpr=",arr,"\n *** ixexpr",ix,"\n *** arr type",at,"\n *** ix type",it)

getExprType0 scope e@(Expr ArrayDeref (arr:ix:ixs) pose) =
    let na = Expr ArrayDeref [arr,ix] pose
    in getExprType0 scope $ Expr ArrayDeref (na:ixs) pose

getExprType0 scope (Expr TakeAddress [e] _) =
    let ET at _ = getExprType scope e
    in ET (TypeRefPointer at) Nothing

getExprType0 scope e@(Expr InSet [e1, e2] _) =
    let ET t1 _ = getExprType scope e1
        ET t2 _ = getExprType scope e2
    in case t2 of
        TypeRefSet (Right typ)
            | typeEquals scope typ t1 -> ET typeBoolean Nothing
            | otherwise -> error $ "getExprType0: inset: incompatible operands? "++show (typ,t1,e)
        _ -> error $ showN ("\ninset: t1=",t1,"\nt2=",t2,"\ne=",e)

getExprType0 scope e@(Expr Not [e1] _) =
    let ET t1 _ = getExprType scope e1
    in case t1 of
        TypeRefNative "boolean" -> ET typeBoolean Nothing
        _ -> error $ "getExprType0: Not: not boolean: "++show e
getExprType0 scope e@(Expr LOr [e1,e2] _) = getExprType0 scope (Expr LAnd [e1, e2] undefined)
getExprType0 scope e@(Expr LXor [e1,e2] _) = getExprType0 scope (Expr LAnd [e1, e2] undefined)

getExprType0 scope e@(Expr LAnd [e1,e2] _) =
    let ET t1 _ = getExprType scope e1
        ET t2 _ = getExprType scope e2
    in
        if isIntegerType t1 && isIntegerType t2 then ET typeInteger Nothing
          else if t1 == typeBoolean && t2 == typeBoolean then ET typeBoolean Nothing
          else error $ "getExprType0: LAnd/LOr: wrong types: "++showN ("\nt1=",t1, "\nt2=",t2, "\nexpr1=",e1,"\nexpr2=",e2)

getExprType0 scope e@(Expr IMod [e1,e2] _) =
    let ET t1 _ = getExprType scope e1
        ET t2 _ = getExprType scope e2
    in
        if isIntegerType t1 && isIntegerType t2 then ET typeInteger Nothing
          else error $ "getExprType0: IMod: wrong types: "++show (t1, t2, e)



getExprType0 scope expr = error $ "getExprType0: not implemented for: "++(show expr)

isLeft (Left _) = True
isLeft _ = False
isRight = not . isLeft
getRight (Right r) = r
getRight (Left v) = error $ "getRight: "++v
getLeft (Left l) = l
getLeft (Right r) = error $ "getLeft: isRight."

convertExpr :: Scope -> Maybe TypeRef -> Expr -> Maybe ResultExpr
convertExpr scope Nothing expr =
    case transformExpression scope expr of
        re@(REExpr _) -> Just re
        _ -> error $ "convertExpr: not simple expression " ++ show expr

convertExpr scope (Just typ) expr =
    let ET t v = getExprType scope expr
        cast = castExpr scope expr typ
    in case typ of
      _ -> if (typeEquals scope t typ)
              then
                case transformExpression scope expr of
                  re@(REExpr _) -> Just re
              else if isRight cast then
                case transformExpression scope (getRight cast) of
                  re@(REExpr _) -> Just re
              else error $ "convertExpr: not equals: "++show (t)++" to "++show typ++" in expr: "++(show expr)++" cast error="++getLeft cast

showN :: (Show a) => a -> String
showN x =
    let s = show x
    in replaceN s
    where
        replaceN [] = []
        replaceN (a:[]) = [a]
        replaceN ('\\':'n':as) = '\n':(replaceN as)
        replaceN (a:as) = a:(replaceN as)

castExpr :: Scope -> Expr -> TypeRef -> Either String Expr
castExpr scope expr@(Expr _ _ epos) typ =
    let ET et v = getExprType scope expr
    in if typeEquals scope et typ
      then Right expr
      else case (et, typ, expr) of
        (TypeRefArray e1 t1, TypeRefArray e2 t2, Expr ListValue items _)
            | getRangeLengths scope e1 == getRangeLengths scope e2
                 ->  Right $ Expr ListValue ( map (\e -> getRight $ castExpr scope e t2) items) epos
            | otherwise -> Left $ "incompatible array cast " ++ showN ("\n          ###### expr=",expr, "\nto", typ, "\nrange1=",
                                        getRangeLengths scope e1, "\nrange2=",getRangeLengths scope e2,
                                        "\ne1=",e1,"\ne2=",e2,"\net=",et)
        (TypeRefNative "string", TypeRefPointer (TypeRefNative "char"), Expr (Const (ConstString v)) [] _) ->
            Right $ Expr (Const (ConstPChar v)) [] epos
        (TypeRefArray _ (TypeRefNative "char"), TypeRefNative "string", _) ->
            Right $ Expr FunCall [((Expr (VarRef "RT_CHARR2STRING") [] epos)),expr] epos
        (TypeRefArray _ (TypeRefNative "char"), TypeRefFixedString _, _) ->
            Right $ Expr FunCall [((Expr (VarRef "RT_CHARR2STRING") [] epos)),expr] epos
        (TypeRefArray _ (TypeRefNative "char"), TypeRefPointer (TypeRefNative "char"), _) ->
            Right $ Expr FunCall [((Expr (VarRef "RT_CHARR2PCHAR") [] epos)),expr] epos
        (TypeRefNative "string", TypeRefPointer (TypeRefNative "char"), _) ->
            Right $ Expr FunCall [((Expr (VarRef "RT_STR2PCHAR") [] epos)),expr] epos
        (TypeRefPointer (TypeRefArray _ (TypeRefNative "char")) , TypeRefPointer (TypeRefNative "char"), _) ->
            Right $ Expr FunCall [((Expr (VarRef "RT_PCHARR2PCHAR") [] epos)),expr] epos
        (TypeRefNative "char", TypeRefNative "string", _) -> Right $ expr
        (_, TypeRefRecord fields, Expr ListValue lfields _) ->
            Right $ Expr (Const $ ConstRecord (map (extractFromListToRecord scope (lfields2list scope lfields)) fields)) [] epos
        (TypeRefAnyPointer, TypeRefNative "pointer", _) -> Right expr
        (TypeRefPointer _, TypeRefNative "pointer", _) -> Right expr
        (TypeRefSet _, TypeRefSet (Right TypeRefVoid), _) -> Right expr
        (TypeRefSet (Right TypeRefVoid), TypeRefSet _, _) -> Right expr
        v
            | isIntegerType typ && isFloatingType et -> Left $ "cannot cast double to int: "++ show expr
            | isFloatingType typ && isFloatingType et -> Right $ expr
            | isFloatingType typ && isIntegerType et -> Right $ expr
            | isIntegerType typ && isIntegerType et -> Right $ expr
            | isStringOrChar typ && isStringOrChar et -> Right $ expr
            | et == typ -> Right $ expr
            | otherwise -> Left $ "castExpr: continue here "++showN ("\n ** casting expr type=",et,"\n ** to type=",typ,"\n ** expr itself=",expr)

extractFromListToRecord :: Scope -> [(String,Expr)] -> (String, TypeRef) -> (String, ConstValue)
extractFromListToRecord scope lfields (fname,ftype) =
    case lookup fname lfields of
        Nothing -> error $ "Record missing in initializer: "++show(fname,"in",lfields)
        Just expr ->
            let ET et ev = getExprType scope expr
            in case (et,ev) of
                (TypeRefProcedure _ _ _, Nothing) -> do
                    case expr of
                        Expr (VarRef nm) [] _ -> (fname, ConstSymbolAddress nm)
                        _ -> error $ "record initializing is not a valid symbol ref: " ++show (et, ev, expr)
                (_, Nothing) -> error $ "record initializing is not a constant: " ++show (et, ev, expr)
                (_, Just (Expr (Const cv) [] _)) -> (fname, cv)
                _ -> error $ "record initializing other than a constant"

lfields2list :: Scope -> [Expr] -> [(String,Expr)]
lfields2list scope lfields = map colon2pair lfields
  where
    colon2pair (Expr ColonOp [Expr (VarRef nm) [] _,e2] _) = (nm, e2)
    colon2pair ex = error $ "colon2pair: in record initializer: bad initializer: name required at the left of colon: "++show ex

getRangeLengths scope = getRangeLength0
  where
    getRangeLength0 (Expr Range [r1, r2] _) =
        let ET _ r1v = getExprType scope r1
            ET _ r2v = getExprType scope r2
        in case (r1v, r2v) of
            (Just (Expr (Const (ConstInteger i1)) [] _), Just (Expr (Const (ConstInteger i2)) [] _)) -> i2-i1+1
            _ -> error $ "getRangeLengths: not implemented on non-integer or non-constant ranges: "++show (r1v, r2v)
--    getRange0 e = error $ "getRangeLengths called on non range: "++(show e)




data ResultExpr = forall a . REExpr (Exp a)
                    | REConstCode String
                    | RENativeCall

instance Show ResultExpr where
    showsPrec p var = case var of
      REExpr e -> shows e
      REConstCode s -> showString s
      RENativeCall -> showString "[nativecall]"

binaryOpExpression scope (Expr _ [e1, e2] _) fun =
    let e1' = transformExpression scope e1
        e2' = transformExpression scope e2
    in case (e1', e2') of
        (REExpr var1, REExpr var2) -> REExpr $ JConst $ "(" ++ (fun ("("++show e1'++")") ("("++show e2'++")")) ++ ")"
escapeString :: String -> String
escapeString s
    | any badChar s =
        let q = escapeString0 s
        in "(" ++ (concat $ intersperse "+" q) ++ ")"
    | otherwise = "'" ++ s ++ "'"
escapeString0  :: String -> [String]
escapeString0 [c] = [(escapeChar c)]
escapeString0 (s:ss) = (escapeChar s):(escapeString0 ss)
escapeChar :: Char -> String
escapeChar c
    | badChar c = "String.fromCharCode("++ (show $ fromEnum c)++ ")"
    | otherwise = "'"++[c] ++ "'"
badChar c = (fromEnum c) < 32 || ((fromEnum c) > 127 && (fromEnum c) < 256) || c == '\''|| c == '\\'

-- transformExpression scope (Expr CallNative []) = RENativeCall
transformExpression :: Scope -> Expr -> ResultExpr
transformExpression scope (Expr (Const (ConstString s)) [] _) = REExpr (JConst $ escapeString s)
transformExpression scope (Expr (Const (ConstDouble d)) [] _) = REExpr (JFloat (read $ show d))
transformExpression scope (Expr (Const (ConstInteger i)) [] _) = REExpr (JInt (read $ show i))
transformExpression scope (Expr (Const (ConstBool True)) [] _) = REExpr (JConst "true" :: Exp Bool)
transformExpression scope (Expr (Const (ConstBool False)) [] _) = REExpr (JConst "false" :: Exp Bool)
transformExpression scope (Expr (VarRef "CONTINUE") [] _) = REExpr (JConst "continue")
transformExpression scope (Expr (VarRef "BREAK") [] _) = REExpr (JConst "break")
transformExpression scope (Expr (VarRef "PLEASEFUCK") [] _) = REExpr (JConst "yield 0")
transformExpression scope (Expr (VarRef "EXIT") [] _) =
    let
        (SymbolScope syms) = scopeSymbols scope
        retToString ["_rt_retval"] = "_rt_retval"   -- to lazy to do it other way
        retToString [] = ""
        retToString names = "[" ++ (concat $ intersperse "," names) ++ "]"
        isFunction = (scopeHasRetVal scope)
        outArgs = collectOutArgs scope
        hasArgs = outArgs /= []
        maybeList = retToString ( (if isFunction then ["_rt_retval"] else []) ++ outArgs)
    in if (not isFunction)
        then REExpr (JConst $ "throw new OutVariables(" ++ maybeList ++ ")")
        else REExpr (JConst $ "return " ++ maybeList)
transformExpression scope e@(Expr (VarRef nm) [] _) =
    let (SymbolScope syms) = scopeSymbols scope
        foundSym = lookup nm syms
        meIsFunction = (scopeHasRetVal scope)
        maybeParens = case foundSym of
            Just (_, Just (TypeRefProcedure [] _ _),_) -> "()"
            _ -> ""
        isProcedure = case foundSym of
            Just (_, Just (TypeRefProcedure _ Nothing _),_) -> maybeParens /= [] && (not meIsFunction)
            _ -> False
    in case foundSym of
        Just (FunctionRetVal,_,_) -> REExpr $ JConst "_rt_retval"
        Just (WithScope ws,_,_) -> REExpr $ JConst $ maybeWrapProcedure isProcedure Nothing (ws ++ "." ++ nm)
        Just (UnitScope un,_,_) -> REExpr $ JConst $ maybeWrapProcedure isProcedure Nothing (un ++ "$" ++ nm ++ maybeParens)
        Just (site,_,_) -> REExpr $ JConst $ maybeWrapProcedure isProcedure Nothing (nm  ++ maybeParens)  -- ++ "/* " ++ show site ++" */"
        Nothing -> error $ "transformExpression: symbol not found: "++show (nm,e) ++ (take 1000 $ showScope syms)
transformExpression scope e@(Expr Add _ _) = binaryOpExpression scope e $ (\v1 v2 -> v1 ++"+" ++ v2)
transformExpression scope e@(Expr Sub _ _) = binaryOpExpression scope e $ (\v1 v2 -> v1 ++"-" ++ v2)
transformExpression scope e@(Expr Mul _ _) = binaryOpExpression scope e $ (\v1 v2 -> v1 ++"*" ++ v2)
transformExpression scope e@(Expr LAnd _ _) = binaryOpExpression scope e $ (\v1 v2 -> v1 ++" && " ++ v2)
transformExpression scope e@(Expr LOr _ _) = binaryOpExpression scope e $ (\v1 v2 -> v1 ++" || " ++ v2)
transformExpression scope e@(Expr LessThan _ _) = binaryOpExpression scope e $ (\v1 v2 -> v1 ++" < " ++ v2)
transformExpression scope e@(Expr GreaterThan _ _) = binaryOpExpression scope e $ (\v1 v2 -> v1 ++" > " ++ v2)
transformExpression scope e@(Expr LE _ _) = binaryOpExpression scope e $ (\v1 v2 -> v1 ++" <= " ++ v2)
transformExpression scope e@(Expr GE _ _) = binaryOpExpression scope e $ (\v1 v2 -> v1 ++" >= " ++ v2)
transformExpression scope e@(Expr NE _ _) = binaryOpExpression scope e $ (\v1 v2 -> v1 ++" != " ++ v2)
transformExpression scope e@(Expr Shr _ _) = binaryOpExpression scope e $ (\v1 v2 -> "RT_shr(" ++ v1 ++ "," ++ v2 ++ ")")
transformExpression scope e@(Expr IDiv _ _) =  binaryOpExpression scope e $ (\v1 v2 -> "RT_idiv(" ++ v1 ++ "," ++ v2 ++ ")")
transformExpression scope e@(Expr IMod _ _) = binaryOpExpression scope e $ (\v1 v2 -> "RT_imod(" ++  v1 ++ "," ++ v2 ++ ")")
transformExpression scope e@(Expr Shl _ _) = binaryOpExpression scope e $ (\v1 v2 -> "RT_shl(" ++ v1 ++ "," ++ v2 ++ ")")
transformExpression scope e@(Expr LXor [e1,e2] _) =
    let ET t1 _ = getExprType0 scope e1
        ET t2 _ = getExprType0 scope e2
    in
        if isIntegerType t1 && isIntegerType t2 then binaryOpExpression scope e $ (\v1 v2 -> v1 ++" ^ " ++ v2)
          else if t1 == typeBoolean && t2 == typeBoolean then binaryOpExpression scope e $ (\v1 v2 -> "RT_bool_xor(" ++ (show v1) ++ "," ++ (v2) ++ ")")
          else error $ "transformExpression: LXor: wrong types: "++show (t1, t2, e)

transformExpression scope e@(Expr Types.Equals [e1,e2] _) =
    let e1' = transformExpression scope e1
        e2' = transformExpression scope e2
        ET t1 _ = getExprType scope e1
        ET t2 _ = getExprType scope e2
        straightConvert = binaryOpExpression scope e $ (\v1 v2 -> v1 ++" == " ++ v2)
    in case castExpr scope e2 t1 of
        Right v
            | isPrimitiveType t1 || (isPointer t1 && isPointer t2)-> straightConvert
            | isSetType t1 && isSetType t2 -> binaryOpExpression scope e $ (\v1 v2 -> "RT_comparesets(" ++ v1 ++"," ++ v2 ++ ")")
            | otherwise ->
                        error $ "not implemented yet: comparing complex types: " ++ showN ("t1=",t1, "\nt2=",t2, "\ne1=",e1, "\ne2=",e2)
        Left err
            | isStringOrChar t1 && isStringOrChar t2 -> straightConvert
            | otherwise -> error $ "equals: cannot compare typess: "++showN ("\nt1=",t1,"\nt2=",t2,"\ne1=",e1,"\ne2=",e2,"\nerr=",err)
    {-
    in case (e1', e2') of
        (REExpr var1, REExpr var2) -> REExpr $ JConst $ "(" ++ (fun ("("++show e1++")") ("("++show e2++")")) ++ ")"
        -}


transformExpression scope e@(Expr FunCall (Expr(VarRef "SIZEOF") _ _ : (Expr (VarRef nm) _ _) : _) _) =
    let (TypeScope syms) = scopeTypes scope
    in case lookup nm syms of
        Just (_, TypeRefArray ra@(Expr Range [b, e] _) _) ->
            let ET _ limit = getExprType scope e
                ET _ start = getExprType scope b
                limitI = getIntConst limit
                startI = getIntConst start
            in case (startI, limitI) of
                (Just startV, Just limitV) ->
                        REExpr $ JConst $ (show (limitV - startV + 1)) ++ "/* SIZEOF NEW */"
                _ -> error $ "found range: "++show ra
        _ -> REExpr $ JConst $ "-1 /* SIZEOF " ++(show e) ++ " */"
    --

transformExpression scope e@(Expr FunCall (Expr(VarRef "SIZEOF") _ _ : param : _) _) =
    REExpr $ JConst $ "-2 /* SIZEOF " ++(show e) ++ " */"
--     error $ "SIZEOF not compilable for now: " ++ show param

transformExpression scope e@(Expr FunCall (Expr(VarRef "NEW") [] _ : param : _) _) =
    let ET newType _ = getExprType scope param
        recordInitVal =
            case newType of
                TypeRefPointer recordType -> initialValue "" scope $ Just recordType
                _ -> error $ "new: cannot generate RT_new for non-pointer or untyped pointer" ++ (show e)
    in REExpr (JConst $ (show $ transformExpression scope param) ++ " = RT_new(" ++ (show recordInitVal) ++ ")")


transformExpression scope (Expr FunCall [Expr(VarRef "FILLCHAR") [] _,
                                         var,
                                         _,
                                         Expr (Const (ConstInteger 0)) [] _ ] _) =
    let ET typeToFill _ = getExprType scope var
        badType = error $ "FillChar doesn't support native pointers: " ++ (show typeToFill)
        recordInitVal =
            case typeToFill of
                TypeRefNative "pointer" -> badType
                _ -> initialValue "" scope $ Just typeToFill
    in REExpr (JConst $ (show $ transformExpression scope var) ++ " = " ++ (show recordInitVal))

transformExpression scope (Expr FunCall [Expr(VarRef "FILLCHAR") [] _,
                                         var,
                                         _,
                                         fillVal@(Expr (Const (ConstInteger n)) [] _) ] _) =
    let ET typeToFill _ = getExprType scope var
        initVal range typ = createArrayInitializer scope range typ $ Just fillVal
        recordInitVal =
            case typeToFill of
                TypeRefArray range elemType
                    | isIntegerType elemType -> initVal range elemType
                    | otherwise -> error $ "FillChar doesn't support filling arrays with non atomic elems" ++ (show typeToFill)
                _ -> error $ "FillChar doesn't support filling non-arrays with non-zero values" ++ (show typeToFill)
    in REExpr (JConst $ (show $ transformExpression scope var) ++ " = " ++ (show recordInitVal))

transformExpression scope e@(Expr FunCall (Expr(VarRef "INC") [] _: es) _) = transformINCDEC scope '+' e
transformExpression scope e@(Expr FunCall (Expr(VarRef "DEC") [] _: es) _) = transformINCDEC scope '-' e

--transformExpression scope (Expr SetValue values _) =
--      let setValues = map constructValue values
--          constructValue value = let ET valueType _ = getExprType scope value
--                                 in case valueType of
--                                    _
--                                      | isIntegerType valueType || isStringOrChar valueType -> (show $ transformExpression scope value)
--                                      | otherwise -> error $ "Cannot construct set value for non-integer or non-char type" ++ (show value)
--      in REExpr $ JConst $ "RT_setof(" ++ (concat $ intersperse ", " setValues) ++ ")"
--    let ET lt _ = getExprType scope l
--        ET rt _ = getExprType scope  r
--        cast = castExpr scope r lt
--    in case cast of
--        Left v -> error $ "cannot cast (expr,to,type)" ++ showN (r,"\nto",lt,"\nreason=",v)
--        Right nr ->
--            let l' = transformExpression scope l
--                r' = transformExpression scope nr
--            in  REExpr $ case rt of
--                TypeRefArray _ _ -> (JAssign (JVar $ show l') (JConst $ "RT_NNN_copyarray_YYY_OF_ZZZ(" ++ show r' ++ ")"))
--                TypeRefRecord _ -> JAssign (JVar $ show l') (JConst $ "RT_NNN_copyrecord_YYY_OF_ZZZ(" ++ show r' ++ ")")
--                TypeRefSet _ -> JAssign (JVar $ show l') (JConst $ "RT_NNN_copyset_YYY_OF_ZZZ(" ++ show r' ++ ")")
--                _ -> JAssign (JVar $ show l') (JConst $ show r')


transformExpression scope (Expr (Const (ConstPChar s)) [] _) = REExpr (JConst $ escapeString s)
transformExpression scope (Expr ListValue valz _) =
    REExpr $ JConst $ "["++
                      (concat $ intersperse "," $ map (\e -> case transformExpression scope e of
                                                                 REExpr exp -> show exp
                                                                 _ -> error "Ooh(2345)") valz) ++ "]"
transformExpression scope (Expr (Const (ConstRecord valz)) [] _) =
    REExpr $ JConst $ "{"++
        (concat $ intersperse "," $ map (doPair) valz)
            ++ "}"
    where
        doPair (nm, const) =
            "\""++nm++"\""++": "++show (transformExpression scope $ Expr (Const $ const) [] noPosition )

transformExpression scope (Expr (Const ConstNil) [] _) = REExpr $ JConst "null"
transformExpression scope e@(Expr (Const (ConstSymbolAddress sym)) [] _) =
    let SymbolScope syms = scopeSymbols scope
    in case lookup sym syms of
        Just (site, _, _) -> REExpr $ JConst $ getVariableName sym site scope
        Nothing -> error $ "ConstSymbolAddress: not found: "++show e

transformExpression scope (Expr Types.Div [e1,r2] _) = REExpr $ JConst $ "(" ++ show ( transformExpression scope e1) ++ ") / ("
                        ++ show ( transformExpression scope e1) ++ ")"
transformExpression scope e@(Expr Not [e1] _) =
    REExpr $ JConst $ "!("++ show ( transformExpression scope e1) ++ ")"
transformExpression scope e@(Expr Range [e1,e2] _) =
    REExpr $ JConst $ "RT_mkrange("++ show ( transformExpression scope e1) ++ "," ++ show ( transformExpression scope e2) ++  ")"

transformExpression scope (Expr SetValue valz _) =
    let svalz = map (show . transformExpression scope) valz
    in REExpr $ JConst $ "RT_setof(" ++
             (concat $ intersperse "," svalz) ++ ") /* " ++ show valz ++ "*/"

transformExpression scope (Expr ArrayDeref (arr:ix:ix2:ixs) poss) =
    let narr = Expr ArrayDeref [arr,ix] poss
    in transformExpression scope (Expr ArrayDeref (narr:ix2:ixs) poss)

transformExpression scope (Expr ArrayDeref [arr,ix] _) =
            let ET at _ = getExprType scope arr
                lpart = (show $ transformExpression scope arr)
            in case at of
                TypeRefArray range et ->
                    let (r1,r2) = evaluateRange range
                        ET it _ = getExprType scope r1
                    in case (ix, castExpr scope ix it) of
                            (Expr ColonOp _ _,_) -> case arr of
                                            Expr (VarRef "MEM") _ _  -> REExpr $ JConst "RT_BLACKHOLE_MEM /* MEM */"
                                            Expr (VarRef "MEMW") _ _ -> REExpr $ JConst "RT_BLACKHOLE_MEMW /* MEMW */"
                                            _ -> error $ "addressing array with colon, not Mem, not MemW: "++show arr
                            (_,Right ix') ->
                                REExpr $ JConst $
                                    lpart ++ "[" ++
                                        (normalizeIndex ix' r1 r2) ++ "]"
                            (ix, Left err) -> error $ "ArrayDeref: "++show ("ix=",ix, err)
                TypeRefNative "string" ->
                    case castExpr scope ix typeInteger of
                            Right ix' ->
                                REExpr $ JConst $ "RT_strcharat("++
                                    (show $ transformExpression scope arr) ++ "," ++
                                        (show $ transformExpression scope ix') ++ ")"
                            Left err -> error $ "String charat: "++err
                TypeRefPointer (TypeRefNative "char") ->
                    case castExpr scope ix typeInteger of
                            Right ix' ->
                                REExpr $ JConst $ "RT_pcharptr("++
                                    (show $ transformExpression scope arr) ++ "," ++
                                        (show $ transformExpression scope ix') ++ ")"
                            Left err -> error $ "PCharDeref: "++err
                x -> error $ "array deref: "++show (arr," is not array!",at)
    where
        normalizeIndex ix' r1 r2 =
            "RT_range_check(" ++
                (show $ transformExpression scope ix') ++ "," ++
                (show $ transformExpression scope r1) ++ "," ++
                (show $ transformExpression scope r2) ++ ")"
        evaluateRange (Expr Range [r1,r2] _) =
            case (getExprType scope r1,getExprType scope r2) of
                (ET _ (Just rv1), ET _ (Just rv2)) -> (rv1, rv2)
                nc -> error $ "non-constant array range in specification: "++show (r1, r2)

        evaluateRange _ = error $ "evaluateRange: not range"


--transformExpression scope e@(Expr FunCall (Expr VarRef "INC":args) _) =
--transformExpression scope e@(Expr FunCall (Expr VarRef "DEC":args) _) =
transformExpression scope e@(Expr FunCall (fun:args) _) =
    let ET ft _ = getExprType scope fun
        fname = show $ transformExpression scope fun
    in REExpr $ JConst $ case ft of
        (TypeRefProcedure [(AnyArgument _)] mtr _) ->
            fname ++ (applyArguments args)
        (TypeRefProcedure fargs mtr _) ->
            if (length fargs == length args) then
                let nargs = map (\(a, ra) ->
                                    case a of
                                        Argument _ _ (Just at) ->
                                            getRight $ castExpr scope ra at
                                        Argument _ _ Nothing -> ra
                                        AnyArgument _ -> ra
                                        -- q -> error $ "transformExpression/FunCall: unhandled: "++(show q)
                                    ) $ zip fargs args
                    outargs = filter (isOutArgument . fst) $ zip fargs nargs
                    isFunction = isJust mtr
                    meIsFunction = (scopeHasRetVal scope)

                    postReceivingList addIndex = concatMap (\(ix,(_,na)) ->
                         ",(" ++ (show $ transformExpression scope na) ++ "="
                         ++ "_tmp$list[" ++ (show ix) ++ "])") $ zip [addIndex..] outargs
                    (receiver,postReceiver) = case outargs of
                        [] -> ("","")
                        argz -> (
                            "(_tmp$list = ",
                            ")" ++ (postReceivingList (if isJust mtr then 1 else 0 ) ++
                                (if isJust mtr then ",_tmp$list[0]" else "" ))
                            )
                    fcall = fname ++ (applyArguments nargs)
                    gen
                        | isFunction || meIsFunction  = receiver ++ fcall ++ postReceiver
                        | outargs == [] = maybeWrapProcedure True Nothing fcall
                        | otherwise = maybeWrapProcedure True (Just $
                            map (show . transformExpression scope . snd ) outargs) fcall
                in gen
             else
                error $ "incorrect number of arguments in function call: "++ show e
            -- fname ++ (applyArguments args)

        _ -> error $ "Call of not function: "++show (fun:args)
    where
        applyArguments :: [Expr] -> String
        applyArguments exprs = "("
            ++ (concat $ intersperse "," $ map (show . transformExpression scope) exprs)
            ++ ")"

transformExpression scope (Expr PropertyExtr [l,r] _) =
    let ET lt _ =  getExprType scope l
    in case (r, lt) of
        (Expr (VarRef propname) [] _, TypeRefRecord flds) ->
            case lookup propname flds of
                Just typ -> REExpr $ JConst $
                    (show $ transformExpression scope l) ++ "." ++ propname
                Nothing -> error $ "field, not found, in "++show (propname, "not found in", lt)
        (_, TypeRefUnitScope) -> transformExpression (keepOnlyUnitScopeSymbols scope) r
        ex -> error $ "prop extraction invalid, non-name or on non-recoed " ++ show (ex, l)

transformExpression scope e@(Expr PointerDeref [p] _) =
    let ET lt _ =  getExprType scope p
        defaultConv = REExpr $ JConst $
                  "(" ++ (show $ transformExpression scope p) ++ ").val"
    in case lt of
        TypeRefPointer pt -> defaultConv
        TypeRefAnyPointer -> defaultConv
        TypeRefNative "pointer" -> defaultConv
        ex -> error $ "dereferencing non-pointer " ++ show (lt,e,ex)

transformExpression scope e@(Expr TakeAddress [p@(Expr (VarRef nm) [] _)] _) =
    let SymbolScope ss = scopeSymbols scope
    in case lookup nm ss of
        Just (site,Just (TypeRefProcedure _ _ _),_) -> REExpr $ JConst $ (getVariableName nm site scope)
        -- Just (site,_,_) -> REExpr $ JConst $ (getVariableName nm site scope)
        Just (site,_,_) -> REExpr $ JConst $ "RT_takeAddr(" ++ (show $ transformExpression scope p) ++ ") /* " ++ show p ++ "*/"
        Nothing -> error $ "Expr TakeAddress: not found"

transformExpression scope e@(Expr TakeAddress [p] _) =
    REExpr $ JConst $ "RT_takeAddr(" ++ (show $ transformExpression scope p) ++ ") /* " ++ show p ++ "*/"

transformExpression scope e@(Expr InSet [e1, e2] _) =
    REExpr $ JConst $
                  "RT_in_set(" ++ (show $ transformExpression scope e1) ++ "," ++ (show $ transformExpression scope e2) ++ ") "

-- last one
transformExpression scope e = error $ "transformExpression: not implemented yet: "++show e


transformINCDEC scope sign e@(Expr FunCall (Expr(VarRef _) [] _: es) _) =
    let ET t1 _ = getExprType scope $ head es
        valu = show ( transformExpression scope $ head es)
    in case t1 of
        _
            | isIntegerType t1 && length es == 1 ->
                REExpr $ JConst $ "((" ++ valu ++ ")"++[sign,sign]++")"  -- postincr/decr: ++ or --
            | isIntegerType t1 && length es == 2 ->
                let delta = show ( transformExpression scope $ (es !! 1))
                in REExpr $ JConst $ "((" ++ valu ++ ")"++[sign] ++ "=("++delta++"))"
            | isPointer t1 ->
                let delta = if length es == 2
                                then show ( transformExpression scope $ (es !! 1))
                                else "1"
                in REExpr $ JConst $ "RT_incdecptr(" ++ valu ++ ","++[sign,'('] ++ delta ++ "))"
            | isChar t1 ->
                let delta = if length es == 2
                                then show ( transformExpression scope $ (es !! 1))
                                else "1"
                in REExpr $ JConst $ valu ++ "=RT_incchar(" ++ valu ++ ","++[sign,'('] ++ delta ++ "))"
            | otherwise -> error $ "INC/DEC: type: " ++ show (t1,e)

keepOnlyUnitScopeSymbols scope =
    let SymbolScope syms = scopeSymbols scope
        nsyms = filter (\(_,(site,_,_)) -> isUnitScope site) syms
    in scope { scopeSymbols = SymbolScope nsyms }


maybeWrapProcedure False _ code = code
maybeWrapProcedure True Nothing code = "try { var yi = " ++ code ++ "; if (RT_isGenerator(yi)) {for (var i in yi) yield i; }} catch (ex) { if (ex instanceof OutVariables); else throw ex;}"
maybeWrapProcedure True (Just lst) code = "try {var yi = " ++ code ++ "; if (RT_isGenerator(yi)) {for (var i in yi) yield i; } else {throw new OutVariables(yi)}} catch (ex) { if (ex instanceof OutVariables) {"++ assignOutVarsFromEx lst ++ "} else {throw ex;}}"
    where
        assignOutVarsFromEx lst =
            (concat $ intersperse ";" $
                map (\(i,l) -> l ++ " = ex.lst[" ++ (show i) ++ "]") $ zip [0..] lst) ++ ";"

