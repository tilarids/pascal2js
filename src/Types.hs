{-

SEE readme for terms of use

-}

module Types where

import Data.Char
import Data.List
import Data.Either
import Data.Maybe
import qualified Data.Map as M

import Language.HJavaScript.Syntax
import Debug.Trace

traceval l v =
    trace (l ++ show v) $ v

-- -------------------------- data

data Project = Project {
     projectDirectories :: [FilePath]
     , projectMainFile :: String              -- symbol, eg "MAINPROG"
     , projectCollectedFiles :: M.Map String FilePath
     , projectParsedUnits :: M.Map String Compiled
     } deriving             (Show,Read,Eq)

mkProject dirs main =
    Project dirs (map toUpper main) M.empty M.empty

data Unit = Unit {
        unitName :: String
        ,unitInterfaceDecls :: [Declaration]
        ,unitImplDecls :: [Declaration]
        ,unitInitCode :: CodeBlock
    } deriving (Show,Read,Eq)


data Program = Program {
        programName :: String
        ,programImplDecls :: [Declaration]
        ,programInitCode :: CodeBlock
    } deriving (Show,Read,Eq)

data TypeRef =      TypeRefByName String |
                    TypeRefUnitScope |            -- UnitName.SomeDefinition
                    TypeRefVoid |            -- procedure return
                    TypeRefNative String |            -- tree transformation
                    TypeRefFixedString Expr |         -- fixed string size
                    TypeRefPointer TypeRef |
                    TypeRefAnyPointer |
                    TypeRefNameValue String TypeRef |                  -- in record assignment
                    TypeRefSoftPointer String |         -- pointers which are forward references
                    TypeRefRecord [(String,TypeRef)] |
                    -- TypeRefUnion [(Expr, [(String,TypeRef)])] |        -- case record of
                    TypeRefArray Expr TypeRef |         -- expr = range expr
                    TypeRefSet (Either Expr TypeRef) |
                    TypeRefProcedure [Argument] (Maybe TypeRef) Bool


                    deriving (Show,Read,Eq)

data Argument =
                Argument Bool String (Maybe TypeRef)         -- Boolean = "var"
                | AnyArgument Bool                      -- native functions
        deriving (Show,Read,Eq)

mkArgument nm tr = Argument False nm (Just tr)
isOutArgument (Argument True _ _) = True
isOutArgument _ = False
getArgName (Argument _ nm _) = nm
getArgName _ = error $ "getArgName with broken arg"

data FunctionHeadDecl = FunctionHeadDecl String [Argument] (Maybe TypeRef)
    deriving (Show,Read,Eq)

data ScopeBlock = ScopeBlock [Declaration] CodeBlock
    deriving (Show,Read,Eq)

data Declaration =
        NativeFunDecl FunctionHeadDecl |                -- call to runtime, after tree transformation
        UsesDecl [String] |
        FunHeadDecl FunctionHeadDecl |
        FunDecl FunctionHeadDecl (Maybe ScopeBlock) |
        LabelDecl [String]   |
        VarDecl [(String,(Maybe TypeRef),(Maybe Expr))]   |       -- constants here as well
        TypeDecl [(String, TypeRef)]
            deriving (Show,Read,Eq)

data Compiled = CompiledUnit Unit | CompiledProgram Program | MissingUnit String deriving (Show,Read,Eq)

data CodeBlock = CodeBlock [Statement]
    deriving (Show,Read,Eq)

data ConstValue = ConstString String | ConstPChar String | ConstDouble Double  | ConstSymbolAddress String
                    | ConstInteger Integer | ConstBool Bool | ConstNil
                    | ConstRecord [(String,ConstValue)]
        deriving (Show,Read,Eq)
data Op =
            Const ConstValue | VarRef String |
            Add | Sub | Mul | Div | PropertyExtr | PointerDeref | LOr | LAnd | LXor | Shr | Shl | InSet |
            GE | LE | NE | Equals | Not | GreaterThan | LessThan |
            ArrayDeref | FunCall | TakeAddress |
            Range | IDiv | IMod | ColonOp | SetValue | ListValue |
            SomeCode ScopeBlock                 -- this is how procedures get into scope ;)
                    deriving (Show,Read,Eq)


data Expr = Expr Op [Expr] SourcePosition
    deriving (Show,Read,Eq)

data Statement =
        CallNative |
        AssignSt Expr Expr |
        LabelledStatement String Statement |
        WithSt [Expr] CodeBlock |                   -- !!
        CaseSt Expr [([Expr], CodeBlock)] CodeBlock | --- !!
        EvalSt Expr |
        IfSt Expr CodeBlock CodeBlock |
        AsmSt String |
        YieldSt |
        GotoSt String |
        ForSt Expr Expr Bool Expr  CodeBlock |   -- boolean False=downto
        WhileSt Expr CodeBlock |
        RepeatSt CodeBlock Expr |
        NoopSt
            deriving (Show,Read,Eq)

data StringPiece = QuotedString String | CharCode Char
    deriving (Show,Read,Eq)
data ProcedureModifier = Assembler | Near | Far | External | Interrupt | Forward
    deriving (Show, Eq,Read)


-----
---- Type Resolution
----

data CompiledTyped = CompiledTyped {
                        ctCompiled :: Compiled,
                        ctPublicTypes :: TypeScope,
                        ctPublicSymbols :: SymbolScope
                    } deriving (Show,Read,Eq)

data DeclarationSite = UnitScope String | LocalScope
        | GlobalScope | FunctionArgScope
        | FunctionItselfSpec        -- this is put on scope for current function
        | FunctionRetVal
        | WithScope String   -- "with" stmt
         deriving (Show,Read,Eq)

isUnitScope (UnitScope _) = True
isUnitScope _ = False

isFunctionItselfSpec FunctionItselfSpec = True
isFunctionItselfSpec _ = False

newtype SourcePosition = SourcePosition String deriving (Show,Read)
newtype TypeScope = TypeScope [(String,(DeclarationSite, TypeRef))] deriving (Show,Read,Eq)
newtype SymbolScope = SymbolScope [(String,(DeclarationSite, Maybe TypeRef, Maybe Expr))] deriving (Show,Read,Eq)
newtype CompiledCache = CompiledCache (M.Map String CompiledTyped)  deriving (Show,Read,Eq)

getSiteOfSymbol (_,(site,_,_)) = site

instance Eq SourcePosition
    where q == b = True

noPosition = SourcePosition ""

tsmap f (TypeScope q) = TypeScope (f q)
ssmap f (SymbolScope q) = SymbolScope (f q)
emptyTypeScope = TypeScope []
emptySymbolScope = SymbolScope []

data EventType = EventProgramStart
                | EventProgramEnd
                | EventUnitStart
                | EventUnitEnd
                | EventVariable DeclarationSite String (Maybe TypeRef) (Maybe Expr)
                | EventFunctionStart  DeclarationSite String TypeRef
                | EventCodeBlock String CodeBlock
                deriving (Show,Read,Eq)

data Event = Event EventType Scope
    deriving (Show,Read,Eq)

data Scope = Scope {
                scopeProject :: Project,
                scopeTypes :: TypeScope,
                scopeSymbols :: SymbolScope,
                scopeCompiled1 :: CompiledCache,        -- compiled interfaces (phase1)
                scopeCompiled2 :: CompiledCache,            -- compiled intf/impl (phase2)
                scopeCurrentUnit :: Maybe Compiled,
                scopeEvents :: [Event]
            }
            deriving (Show,Read,Eq)

compiledName (CompiledProgram (Program nm _ _)) = nm
compiledName (CompiledUnit (Unit nm _ _ _)) = nm
compiledName (MissingUnit nm) = nm

initialTypeScope :: TypeScope
initialTypeScope = TypeScope [
        ("STRING",(GlobalScope, TypeRefNative "string")),
        ("INTEGER",(GlobalScope, TypeRefNative "integer")),
        ("WORD",(GlobalScope, TypeRefNative "word")),
        ("BYTE",(GlobalScope, TypeRefNative "byte")),
        ("POINTER",(GlobalScope, TypeRefNative "pointer")),
        ("LONGINT",(GlobalScope, TypeRefNative "longint")),
        ("REAL",(GlobalScope, TypeRefNative "real")),
        ("EXTENDED",(GlobalScope, TypeRefNative "real")),
        ("SHORTINT",(GlobalScope, TypeRefNative "shortint")),
        ("CHAR",(GlobalScope, TypeRefNative "char")),
        ("PCHAR",(GlobalScope, TypeRefPointer (TypeRefNative "char"))),
        ("FILE",(GlobalScope, TypeRefNative "file")),
        ("PSTREAM",(GlobalScope, TypeRefPointer getStreamType)),
        ("BOOLEAN",(GlobalScope, TypeRefNative "boolean"))
    ]


initialTypeScope' = let TypeScope ts = initialTypeScope in ts

typeChar = snd $ fromJustX "typeChar" $ lookup "CHAR" initialTypeScope'
typePChar = TypeRefPointer typeChar
typeString = snd $ fromJustX "typeString" $ lookup "STRING" initialTypeScope'
typeDouble = snd $ fromJustX  "typeDouble" $ lookup "REAL" initialTypeScope'
typeInteger = snd $ fromJustX  "typeInteger" $ lookup "INTEGER" initialTypeScope'
typeByte = snd $ fromJustX  "typeInteger" $ lookup "BYTE" initialTypeScope'
typeBoolean = snd $ fromJustX  "typeBoolean" $ lookup "BOOLEAN" initialTypeScope'
typeAnyProcedure = TypeRefProcedure [AnyArgument False] Nothing False

isIntegerType (TypeRefNative "word") = True
isIntegerType (TypeRefNative "longint") = True
isIntegerType (TypeRefNative "byte") = True
isIntegerType (TypeRefNative "shortint") = True
isIntegerType (TypeRefNative "integer") = True
isIntegerType _ = False

isChar (TypeRefNative "char") = True
isChar _ = False

isSetType (TypeRefSet _) = True
isSetType _ = False

isStringOrChar (TypeRefNative "string") = True
isStringOrChar (TypeRefFixedString _) = True
isStringOrChar (TypeRefNative "char") = True
isStringOrChar _ = False

isFloatingType (TypeRefNative "real") = True
isFloatingType _ = False

isPrimitiveType t = isFloatingType t || isIntegerType t || t == typeBoolean || isStringOrChar t
isPointer TypeRefAnyPointer = True
isPointer (TypeRefNative "pointer") = True
isPointer (TypeRefPointer _) = True
isPointer _ = False         -- soft pointers should be already resolved when using this fun

isRecord (TypeRefRecord _)  = True
isRecord _  = False


initialSymbolScope :: SymbolScope
initialSymbolScope = SymbolScope [
        ("BYTE",(GlobalScope, Just $ TypeRefProcedure [AnyArgument False] (Just $ typeInteger) False,Nothing)),
        ("BOOLEAN",(GlobalScope, Just $ TypeRefProcedure [AnyArgument False] (Just $ typeBoolean) False,Nothing)),
        ("PCHAR",(GlobalScope, Just $ TypeRefProcedure [AnyArgument False] (Just $ typePChar) False,Nothing)),
        ("WORD",(GlobalScope, Just $ TypeRefProcedure [AnyArgument False] (Just $ typeInteger) False,Nothing)),
        ("INTEGER",(GlobalScope, Just $ TypeRefProcedure [AnyArgument False] (Just $ typeInteger) False,Nothing)),
        ("MAXINT",(GlobalScope, Just $ TypeRefProcedure [] (Just $ typeInteger) False,Nothing)),
        ("RANDOMIZE",(GlobalScope, Just $ TypeRefProcedure [] (Just $ TypeRefVoid) False,Nothing)),
        ("ABS",(GlobalScope, Just $ TypeRefProcedure [AnyArgument False] (Just $ typeDouble) False,Nothing)),
        ("LONGINT",(GlobalScope, Just $ TypeRefProcedure [AnyArgument False] (Just $ typeInteger) False,Nothing)),
        ("CHR",(GlobalScope, Just $ TypeRefProcedure [mkArgument "v" typeInteger] (Just $ typeChar) False,Nothing)),
        ("ODD",(GlobalScope, Just $ TypeRefProcedure [mkArgument "v" typeInteger] (Just $ typeBoolean) False,Nothing)),
        ("ROUND",(GlobalScope, Just $ TypeRefProcedure [mkArgument "v" typeDouble] (Just $ typeInteger) False,Nothing)),
        ("ORD",(GlobalScope, Just $ TypeRefProcedure [mkArgument "v" typeChar] (Just $ typeInteger) False,Nothing)),
        ("RANDOM",(GlobalScope, Just $ TypeRefProcedure [] (Just $ TypeRefVoid) False,Nothing)),
        -- TODO: fix it("RANDOM",(GlobalScope, Just $ TypeRefProcedure [mkArgument "limit" typeInteger] (Just $ typeInteger) False,Nothing)),
        ("MEMAVAIL",(GlobalScope, Just $ TypeRefProcedure [] (Just $ typeInteger) False,Nothing)),
        ("FILESIZE",(GlobalScope, Just $ TypeRefProcedure [AnyArgument False] (Just $ typeInteger) False,Nothing)),
        ("HALT",(GlobalScope, Just $ TypeRefProcedure [AnyArgument False] Nothing False,Nothing)),
        ("CLOSE",(GlobalScope, Just $ TypeRefProcedure [AnyArgument False] Nothing False,Nothing)),
        ("ASSIGN",(GlobalScope, Just $ TypeRefProcedure [AnyArgument False] Nothing False,Nothing)),
        ("RESET",(GlobalScope, Just $ TypeRefProcedure [AnyArgument False] Nothing False,Nothing)),
        ("REWRITE",(GlobalScope, Just $ TypeRefProcedure [AnyArgument False] Nothing False,Nothing)),
        ("BLOCKREAD",(GlobalScope, Just $ TypeRefProcedure [AnyArgument False] Nothing False,Nothing)),
        ("BLOCKWRITE",(GlobalScope, Just $ TypeRefProcedure [AnyArgument False] Nothing False,Nothing)),
        ("SEEK",(GlobalScope, Just $ TypeRefProcedure [AnyArgument False] Nothing False,Nothing)),
        ("ERASE",(GlobalScope, Just $ TypeRefProcedure [AnyArgument False] Nothing False,Nothing)),
        ("RUNERROR",(GlobalScope, Just $ TypeRefProcedure [mkArgument "errcode" typeInteger] Nothing False,Nothing)),
        ("WRITE",(GlobalScope, Just $ TypeRefProcedure [AnyArgument False] Nothing False,Nothing)),
        ("WRITELN",(GlobalScope, Just $ TypeRefProcedure [AnyArgument False] Nothing False,Nothing)),
        ("READLN",(GlobalScope, Just $ TypeRefProcedure [AnyArgument False] Nothing False,Nothing)),
        ("READ",(GlobalScope, Just $ TypeRefProcedure [AnyArgument False] Nothing False,Nothing)),
--        ("FILLCHAR",(GlobalScope, Just $ TypeRefProcedure [AnyArgument True, mkArgument "cnt" typeInteger, AnyArgument False] Nothing,Nothing)),
        ("FREEMEM",(GlobalScope, Just $ TypeRefProcedure [AnyArgument True] Nothing False,Nothing)),
        ("GETMEM",(GlobalScope, Just $ TypeRefProcedure [AnyArgument True] Nothing False,Nothing)),
        ("MOVE",(GlobalScope, Just $ TypeRefProcedure [AnyArgument True] Nothing False,Nothing)),
        ("FREESELECTOR",(GlobalScope, Just $ TypeRefProcedure [AnyArgument True] Nothing False,Nothing)),
        ("ALLOCSELECTOR",(GlobalScope, Just $ TypeRefProcedure [AnyArgument True] Nothing False,Nothing)),
        ("SETSELECTORBASE",(GlobalScope, Just $ TypeRefProcedure [AnyArgument True] Nothing False,Nothing)),
        ("SETSELECTORLIMIT",(GlobalScope, Just $ TypeRefProcedure [AnyArgument True] Nothing False,Nothing)),
        ("VAL",(GlobalScope, Just $ TypeRefProcedure [mkArgument "s" typeString, Argument True "i" $ Just typeInteger, Argument True "i" $ Just typeInteger] Nothing False,Nothing)),
        ("STR",(GlobalScope, Just $ TypeRefProcedure [mkArgument "i" typeInteger, Argument True "s" $ Just typeString ] Nothing False,Nothing)),
        ("UPCASE",(GlobalScope, Just $ TypeRefProcedure [AnyArgument False] (Just typeChar) False,Nothing)),
        ("SIZEOF",(GlobalScope, Just $ TypeRefProcedure [AnyArgument False] (Just typeInteger) False,Nothing)),
        ("ROUND",(GlobalScope, Just $ TypeRefProcedure [AnyArgument False] (Just typeInteger) False,Nothing)),
        ("NEW",(GlobalScope, Just $ TypeRefProcedure [AnyArgument False] (Just $ TypeRefPointer TypeRefVoid) False,Nothing)),
        ("DISPOSE",(GlobalScope, Just $ TypeRefProcedure [AnyArgument False] Nothing False,Nothing)),
        ("MEMW",(GlobalScope, Just $ TypeRefArray (createIntRange 0 0) (TypeRefNative "word"),Nothing)),
        ("MEM",(GlobalScope, Just $ TypeRefArray (createIntRange 0 0) (TypeRefNative "byte"),Nothing)),
        ("SEGA000",(GlobalScope, Just $ typeInteger,Nothing)),
        ("ERRORADDR",(GlobalScope, Just $ TypeRefNative "pointer",Nothing)),
        ("PARAMSTR",(GlobalScope, Just $ TypeRefProcedure [mkArgument "v" typeInteger] (Just $ typeString) False,Nothing)),
        ("EXITPROC",(GlobalScope, Just $ TypeRefNative "pointer",Nothing)),
        ("EXITCODE",(GlobalScope, Just $ TypeRefNative "integer",Nothing)),
        ("IORESULT",(GlobalScope, Just $ TypeRefNative "integer",Nothing)),
        ("RT_CHARR2STRING",(GlobalScope, Just $ TypeRefProcedure [AnyArgument False] (Just $ typeString) False,Nothing)),
        ("RT_CHARR2PCHAR",(GlobalScope, Just $ TypeRefProcedure [AnyArgument False] (Just $ typePChar) False,Nothing)),
        ("RT_STR2PCHAR",(GlobalScope, Just $ TypeRefProcedure [mkArgument "s" typeString] (Just $ typePChar) False,Nothing)),
        ("RT_PCHARR2PCHAR",(GlobalScope, Just $ TypeRefProcedure [AnyArgument False] (Just $ typePChar) False,Nothing)),
        ("STOK",(GlobalScope, Just $ TypeRefNative "integer",Nothing)),     -- stOk in streams
        ("PORT",(GlobalScope, Just $ TypeRefArray (createIntRange 0 65535) (TypeRefNative "byte"),Nothing))
    ]

createConstInt i = fromJust $ makeIntConst (Just i)
createIntRange a b = Expr Range [createConstInt a, createConstInt b] noPosition

getIntConst (Just (Expr (Const (ConstInteger v)) _ _)) = Just v
getIntConst _ = Nothing

getDoubleConst (Just (Expr (Const (ConstDouble v)) _ _)) = Just v
getDoubleConst _ = Nothing

getStringConst (Just (Expr (Const (ConstString v)) _ _)) = Just v
getStringConst _ = Nothing

getBooleanConst (Just (Expr (Const (ConstBool v)) _ _)) = Just v
getBooleanConst _ = Nothing

makeIntConst Nothing = Nothing
makeIntConst (Just v)  = (Just (Expr (Const (ConstInteger v)) [] noPosition ))

makeDoubleConst Nothing = Nothing
makeDoubleConst (Just v)  = (Just (Expr (Const (ConstDouble v)) [] noPosition  ))

makeStringConst Nothing = Nothing
makeStringConst (Just v)  = (Just (Expr (Const (ConstString v)) [] noPosition ))

makeBooleanConst Nothing = Nothing
makeBooleanConst (Just v)  = (Just (Expr (Const (ConstBool v)) [] noPosition ))





fromJustX _ (Just a) = a
fromJustX str Nothing = error $ "fromJustX failed: "++str

showScope :: [(String,(DeclarationSite, Maybe TypeRef, Maybe Expr))] -> String
showScope ss = unlines $ map showScope1 ss
showScope1  (nm, (site, mtr, me)) = " : "++ nm ++" "++(show site) ++ " " ++ (take 60 $ ("typ="++show mtr ++ " expr=" ++show me))



getStreamType = TypeRefRecord [
    ("STATUS", typeInteger),
    ("WRITE", typeAnyProcedure),
    ("READ", typeAnyProcedure)
    ]

collectUses [] = []
collectUses ((UsesDecl ss):ds) = ss++(collectUses ds)
collectUses (_:ds) = collectUses ds

findCurrentFun scope =
    let (SymbolScope syms) = scopeSymbols scope
    in head $ filter (isFunctionItselfSpec . getSiteOfSymbol) syms
scopeHasRetVal scope =
    let (_, (_, Just (TypeRefProcedure args rv _), _)) = findCurrentFun scope
    in isJust rv

collectOutArgs scope =
    let (_, (_, Just (TypeRefProcedure args rv _), _)) = findCurrentFun scope
    in map getArgName $ filter isOutArgument args
