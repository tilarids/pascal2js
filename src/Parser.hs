{-

SEE README for terms of use
        
-}
module Parser where

import System.IO
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Token
import Data.Char
import Data.List
import Data.Foldable (foldrM)
import Data.Either
import Data.Maybe
import Debug.Trace
import qualified  Data.Map as M
import Control.Applicative ((<$>))
import Control.Monad(when)
import System.Directory

import Types

-- -------------------------- parser

simple_string :: String -> Parser String
simple_string str = do
    mi <- try maybeidentif
    if ((map toUpper mi) == (map toUpper str))
        then return str
        else fail $ "not that string ("++str++")"
    {-
    simple_string_ str
    where
        simple_string_ [] = return str
        simple_string_ (s:ss) = do
            (char s) <|> (char $ toLower s)
            simple_string_ ss -}

pos :: Parser SourcePosition
pos = do
    p <- getPosition
    return $ SourcePosition $ sourceName p ++ ":" ++ (show $ sourceLine p) ++ ":" ++ (show $ sourceColumn p)

comment1 = do
    char '{'
    manyTill anyChar (char '}')
    return 'x'

comment2 = do
    string "(*"
    manyTill anyChar (try (string "*)"))
    return 'x'

whitespace = do
    rv <- (char '\r' <|> char '\n' <|> char '\t'
        <|> char ' ' <|> (try comment1) <|> (try comment2)) <?> "whitespace/comments"
    pos <- getPosition
    -- trace ("last reached: "++(show pos)) $
    return rv

identifierStart = letter <|> char '_'
identifierChar = alphaNum <|> char '_'

keywords = ["BEGIN","END","CONST","VAR","PROCEDURE","FUNCTION","RECORD","CASE","LABEL","GOTO",
            "IF","THEN","ELSE", "WITH","DO","NOT","DIV","MOD","FOR","TRUE","FALSE","IN","TO","DOWNTO",
            "NIL","TYPE","CONST","NEAR","ASSEMBLER","REPEAT","UNTIL","FAR"]

maybeidentif :: Parser String
maybeidentif =
    try $ do
        ws
        c <- identifierStart
        cs <- many identifierChar
        ws
        return $ c:cs

identif =
    try $ do
        iden <- maybeidentif
        if ((map toUpper iden) `elem` keywords) then fail ("reserved word: " ++ iden) else return $ strUpper iden

ws = many whitespace >> return ()

usesdecl = do
    ws
    simple_string "USES" <?> "USES keyword"
    fn <- identif
    fns <- many (do char ','; identif)
    ws
    char ';'
    return $ UsesDecl (fn:fns)

typeref = do
    retval <- (try recur) <|> (try ptr) <|> (try arrayRef) <|> (try fixedStringRef) <|>(try setRef) <|> (try recordRef) <|> (try procedureRef) <|> ( do
        i <- identif
        return $ TypeRefByName i)
    ws
    return retval;
    where
        recur = do
            ws
            char '('
            r <- typeref
            char ')'
            return r
        ptr = do
            ws
            char '^'
            r <- typeref
            return $ TypeRefPointer r
        arrayRef = do
            ws
            simple_string "ARRAY"
            -- trace "parse array type"
            ws
            char '['
            range <- expr
            ranges <- many $ try $ do
                ws
                char ','
                expr
            char ']'
            ws
            simple_string "OF"
            etype <- typeref
            return $ TypeRefArray range $ nestRanges ranges etype
        nestRanges [] etype = etype
        nestRanges (e:expr) etype = TypeRefArray e $ nestRanges expr etype
        fixedStringRef = do
            ws
            simple_string "string"
            -- trace "parse array type"
            ws
            char '['
            range <- expr
            char ']'
            ws
            return $ TypeRefFixedString range
        setRef = do
            ws
            simple_string "SET"
            ws
            simple_string "OF"
            ws
            maybetype <- optionMaybe $ try typeref
            eith <- case maybetype of
                        Just typ -> return $ Right typ
                        Nothing -> do
                            Left <$> try expr
            ws
            return $ TypeRefSet eith
        recordRef = do
            ws
            simple_string "RECORD" <?> "RECORD keyword"
            ws
            recs <- concat <$> sepEndBy varSingleTypeOrCase (char ';')
            ws
            simple_string "END" <?> "END(of record)"
            ws
            return $ TypeRefRecord $ map (\(a,b,_) -> (a,fromMaybe (TypeRefByName "undefinedType" )b)) recs
        procedureRef = do
            ws
            isFunction <- ((simple_string "PROCEDURE" <?> "procedue keyword") >> return False) <|>
                            ((simple_string "FUNCTION" <?> "function keyword") >> return True)
            ws
            (argz,tref) <- functionTypeSpec isFunction
            -- trace ("proc!"++(show argz)++" "++(show tref)) $ char ';'
            return $ TypeRefProcedure argz tref False


args = do
    ws
    isVar <- optionMaybe (try (simple_string "VAR" <?> "VAR keyword"))
    ws
    nm <- identif
    nms <- many ( try $ do
        ws
        char ','
        identif )
    tref <- optionMaybe (try $ do
        ws
        char ':'
        typeref)
    return $ map (\name -> Argument (isJust isVar) name tref) (nm:nms)

arglist = do
    ws
    az <- optionMaybe $ do
        char '('
        a <- args
        as <- many (try $ do
            ws
            char ';'
            args)
        ws
        char ')'
        return (a ++ concat as)
    return $ fromMaybe [] az

functionHeadDecl = do
    pos <- getPosition
    ws
    isFunction <- ((try (simple_string "PROCEDURE" <?> "procedue keyword in fun head")) >> return False) <|>
                    ((simple_string "FUNCTION" <?> "function keyword in fun head") >> return True)
    ws
    name <- identif
    -- trace ("name="++name)
    ws
    (argz,tref) <- functionTypeSpec isFunction
    ws
    char ';'
    return $ FunHeadDecl $ FunctionHeadDecl name argz tref

functionTypeSpec isFunction = do
    argz <- optionMaybe arglist
    ws
    tref <- optionMaybe $ try $ do
        char ':'
        typeref
    ws
    return ((fromMaybe [] argz), tref)

functionDecl = do
    FunHeadDecl head <- functionHeadDecl
    mods <- sepEndBy (try procedureModifier) (char ';')
    if (Assembler `elem` mods) then do
        block <- asmBlock
        return $ FunDecl head (Just block)
      else if (Forward `elem` mods) then do
        return $ FunHeadDecl head
      else if (External `elem` mods) then do
        return $ FunDecl head $ Just $ ScopeBlock [] $ CodeBlock [CallNative]
      else do
        ws
        block <- scopeBlock
        ws
        return $ FunDecl head $ Just block

procedureModifier = do
    ws
    val <- try ((simple_string "ASSEMBLER" <?> "ASSEMBLER modifier") >> return Assembler)
            <|> try ((simple_string "NEAR" <?> "NEAR modifier") >> return Near)
            <|> try ((simple_string "FAR" <?> "FAR modifier") >> return Far)
            <|> try ((simple_string "FORWARD" <?> "FORWARD modifier") >> return Forward)
            <|> try ((simple_string "INTERRUPT" <?> "INTERRUPT modifier") >> return Interrupt)
            <|> ((simple_string "EXTERNAL" <?> "EXTERNAL modifier") >> return External)
    ws
    return val

scopeBlock = do
    all <- (many ((try varDecl) <|> try typeDecl <|> try labelDecl <|> try functionDecl) <?> "variable, type of function decl")
    ws
    simple_string "BEGIN" <?> "BEGIN(of scope block)"
    ws
    code <- sepEndBy (try $ codeblock) (char ';')
    ws
    simple_string "END" <?> "END(of scope block)"
    ws
    char ';'
    return $ ScopeBlock all $ CodeBlock $ concatMap (\(CodeBlock c) -> c) code

asmBlock = do
    ws
    all <- many ((try varDecl) <|> try typeDecl) <?> "variable, type in asm block"
    ws
    inside <- asmst
    ws
    char ';'
    return $ ScopeBlock all $ CodeBlock [inside]


codeblock = do
    ws
    c <- lookAhead (anyChar)
    --trace ("la="++(show c) )$
    ws
    sts <-
         ( do
            if (c == ';')
              then  return []
              else fail "no that one")
     <|>
        try ( do
        simple_string "BEGIN" <?> "BEGIN of code block"
        ws
        code <- sepEndBy (try $ codeblock) (char ';')
        ws
        simple_string "END" <?> "END of code block"
        ws
        return $ concatMap (\(CodeBlock c) -> c) code
      ) <|>  (try $ do
        st <- statement
        ws
        return [st]
         )
         <|>
            (try $ do
                st <- lookAhead (simple_string "ELSE" <?> "ELSE(everywhere)")
                ws
                return []
            )
         <|>
         ( do
            eof
            return [])
    return $ CodeBlock sts

declaration = do
    ws
    (try usesdecl) <|> (try functionHeadDecl) <|> (try varDecl) <|> (try labelDecl) <|> typeDecl

declarationBody = do
    ws
    (try usesdecl) <|> (try varDecl) <|> (try typeDecl) <|> (functionDecl)

identList = do
    n <- identif
    ns <- many $ try $ do
            ws
            char ','
            identif
    return $ n:ns


frst (a,_,_) = a

varDecl :: Parser Declaration
varDecl = do
    isConst <- try ((simple_string "VAR" <?> "VAR keyword") >> return False) <|> try ((simple_string "CONST" <?> "CONST keyword") >> return True)
    ws
    idents <- many $ try $ do
        vt <- varSingleType
        ws; char ';'; ws
        return vt
    let idents' = concat idents
    -- trace ("add variable: "++(show $ map frst idents')) $
    return $ VarDecl $ idents'

labelDecl :: Parser Declaration
labelDecl = do
    simple_string "LABEL" <?> "LABEL keyword"
    ws
    idents <- identList
    ws
    char ';'
    return $ LabelDecl idents

typeDecl :: Parser Declaration
typeDecl = do
    ws
    simple_string "TYPE"
    ws
    idents <- many typeSingleType
    -- trace ("added type(s) startin with: "++(show $ head idents)) $
    return $ TypeDecl $ concat idents

varSingleTypeOrCase :: Parser [(String, Maybe TypeRef, Maybe Expr)]
varSingleTypeOrCase = do
    (try caseType) <|> varSingleType

caseType = do
    ws
    simple_string "CASE"
    (i,t) <- ( try $ do
        ident <- identif
        ws
        char ':'
        ws
        typ <- typeref
        return (ident, typ)
         ) <|>
      ( do
        typ <- typeref
        return ("_", typ)
        )
    ws
    simple_string "OF"
    allrecs <- concat <$> many (try $ do
        lbl <- expr_ "" 0     -- discarded
        ws
        char ':'
        ws
        char '('
        ws
        recs <- concat <$> sepEndBy varSingleTypeOrCase (char ';')
        char ')'
        ws
        char ';'
        return recs)
    ws
    return $ (if (i == "_") then [] else [(i, Just t, Nothing)]) ++ allrecs


varSingleType :: Parser [(String, Maybe TypeRef, Maybe Expr)]
varSingleType = do
    try $ do
        ws
        idents <- identList
        ws
        typ <- optionMaybe $ do
                    ws
                    char ':'
                    typeref
        val <- optionMaybe $ do
                    ws
                    char '='
                    expr
        ws
        return $ map (\name -> (name, typ, val)) idents

typeSingleType :: Parser [(String, TypeRef)]
typeSingleType = do
    try $ do
        ws
        idents <- identList
        ws
        char '='
        typ <- typeref
        ws
        char ';'
        ws
        return $ map (\name -> (name, typ)) idents


expr :: Parser Expr
expr = expr_ ":" 0

exprP :: Int -> Parser Expr
exprP prio = expr_ ":" prio

expr_ :: String -> Int -> Parser Expr
expr_ extras prio = do
    ws
    single <- exprPart
    rv <- concatExpr single
    ws
    return rv
    where
        exprPart :: Parser Expr
        exprPart = do
            mkExpr <- getMkExpr
            (try $ do
                ws
                char '('
                e <- expr
                ws
                c <- lookAhead (char ')' <|> char ',' <|> char ';')
                case c of
                    ')' -> do
                        char c
                        return $ case e of
                            Expr ListValue _ _ -> mkExpr ListValue [e]    -- single-element list
                            _ -> e
                    ',' -> do
                        es <- many (do
                            char ','
                            ws
                            e <- expr
                            ws
                            return e
                            )
                        char ')'
                        return $ mkExpr ListValue (e:es)
                    ';' -> do
                        es <- many (do
                            char ';'
                            ws
                            e <- expr
                            ws
                            return e
                            )
                        char ')'
                        return $ mkExpr ListValue (e:es)

             )
            <|> (try $ do
                ws
                mkExpr <- getMkExpr
                char '['
                ws
                exprs <- sepBy expr (char ',')
                ws
                char ']'
                return $ mkExpr SetValue exprs)
              <|> try sconstant
              <|> try fconstant
              <|> try iconstant
              <|> try trueconstant
              <|> try falseconstant
              <|> try nilconstant
              <|> try identifi
              <|> try unaryMinus
              <|> try unaryNot
              <|> try takeAddress

        priorities = [
            ('<',3),
            ('>',3),
            ('=',3),
            ('+',5),
            ('-',5),
            ('*',10),
            ('/',10)
            ]

        concatExpr single = do
            ws
            c <- optionMaybe $ try $ do
                cc <- lookAhead $ oneOf ("+-*/.^[(><=" ++ extras)
                case lookup ({- traceval (show ("concatExpr","prio=",prio,"single=",single,"nextchar=")) $ -} cc) priorities of
                    Just eprio ->
                        if eprio < prio
                            then fail "X"
                            else anyChar
                    _ ->  anyChar
                if (cc == ':') then
                    notFollowedBy $ char '='
                    else return ()
                return cc
            let nprio = fromJust $ lookup (fromMaybe 'z' c) priorities
            mkExpr <- getMkExpr
            case c of
                Just '>' -> do
                    eq <- optionMaybe $ char '='
                    rp <- exprP nprio
                    concatExpr (mkExpr (switchMaybe eq GE GreaterThan) [single,rp])
                Just '<' -> do
                    eq <- optionMaybe $ oneOf "=>"
                    let op = case eq of
                                Just '=' -> LE
                                Just '>' -> NE
                                Nothing -> LessThan

                    rp <- exprP nprio
                    concatExpr (mkExpr op [single,rp])
                Just ':' -> do
                    rp <- expr
                    concatExpr (mkExpr ColonOp [single,rp])
                Just '+' -> do
                    rp <- exprP nprio
                    concatExpr (mkExpr Add [single,rp])
                Just '=' -> do
                    rp <- exprP nprio
                    concatExpr (mkExpr Equals [single,rp])
                Just '-' -> do
                    rp <- exprP nprio
                    concatExpr (mkExpr Sub [single,rp])
                Just '*' -> do
                    rp <- exprP nprio
                    concatExpr (mkExpr Mul [single,rp])
                Just '/' -> do
                    rp <- exprP nprio
                    concatExpr (mkExpr Div [single,rp])
                Just '.' -> do
                    dotdot <- optionMaybe $ char '.'
                    case dotdot of
                        Just _ -> do
                            last <- expr_ "" 0
                            return $ mkExpr Range [single, last]
                        Nothing -> do
                            rp <- exprPart
                            concatExpr (mkExpr PropertyExtr [single,rp])
                Just '^' -> do
                    concatExpr (mkExpr PointerDeref [single])
                Just '[' -> do
                    rp <- expr
                    rps <- many $ try $ do
                        ws
                        char ','
                        expr
                    char ']'
                    concatExpr (mkExpr ArrayDeref (single:rp:rps))
                Just '(' -> do
                    rp <- expr
                    rps <- many $ try $ do
                        ws
                        char ','
                        expr
                    char ')'
                    concatExpr (mkExpr FunCall (single:rp:rps))
                Nothing -> do
                    ws
                    rv <- optionMaybe $ ((try $ simple_string "DIV")
                                           <|> (try $ simple_string "MOD")
                                           <|> (try $ simple_string "OR")
                                           <|> (try $ simple_string "XOR")
                                           <|> (try $ simple_string "AND")
                                           <|> (try $ simple_string "SHL")
                                           <|> (try $ simple_string "SHR")
                                           <|> (try $ simple_string "IN")
                                           )
                    case rv of
                        Just "DIV" -> do
                            rp <- exprPart
                            concatExpr (mkExpr IDiv [single,rp])
                        Just "MOD" -> do
                            rp <- exprPart
                            concatExpr (mkExpr IMod [single,rp])
                        Just "OR" -> do
                            rp <- expr
                            concatExpr (mkExpr LOr [single,rp])
                        Just "AND" -> do
                            rp <- exprPart
                            concatExpr (mkExpr LAnd [single,rp])
                        Just "XOR" -> do
                            rp <- exprPart
                            concatExpr (mkExpr LXor [single,rp])
                        Just "SHL" -> do
                            rp <- exprPart
                            concatExpr (mkExpr Shl [single,rp])
                        Just "SHR" -> do
                            rp <- exprPart
                            concatExpr (mkExpr Shr [single,rp])
                        Just "IN" -> do
                            rp <- expr
                            concatExpr (mkExpr InSet [single,rp])
                        Just x -> do
                            trace ("strange operator: "++x ) $ fail "strange op"
                        Nothing -> do
                            return single

        takeAddress = do
            mkExpr <- getMkExpr
            char '@'
            exp <- expr
            return $ mkExpr (TakeAddress) [exp]

        unaryMinus = do
            mkExpr <- getMkExpr
            char '-'
            exp <- exprPart
            return $ mkExpr Sub [Expr (Const $ ConstInteger 0) [] noPosition, exp]

        unaryNot = do
            mkExpr <- getMkExpr
            simple_string "NOT" <?> "NOT keyword"
            ws
            exp <- expr
            return $ mkExpr Not [exp]

        identifi = do
            mkExpr <- getMkExpr
            i <- identif
            return $ mkExpr (VarRef i) []

        number base baseDigit
                = do{ digits <- many1 baseDigit
                    ; let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
                    ; seq n (return n)
                    }

        fconstant =  do
                        mkExpr <- getMkExpr
                        f <- float haskell
                        return $ mkExpr (Const $ ConstDouble f) []
        iconstant =  do
                        mkExpr <- getMkExpr
                        f <- (try $ integer haskell <|> try ( char '$' >> number 16 hexDigit ))
                        return $ mkExpr (Const $ ConstInteger f) []
        sconstant = do
                mkExpr <- getMkExpr
                sp <- many1 (parseStringPiece)
                let s = foldStrings sp
                return $ mkExpr (Const $ ConstString s) []
                where
                    parseStringPiece =
                        (do
                            char '\''
                            cs <- manyTill anyChar (char '\'')
                            return $ QuotedString cs
                        ) <|> ( do
                            char '#'
                            d <- decimal haskell
                            return $ CharCode $ (toEnum $ fromIntegral d :: Char)
                        )
                    foldStrings :: [StringPiece] -> String
                    foldStrings [] = []
                    foldStrings (CharCode co:ss) = co:(foldStrings ss)
                    foldStrings (QuotedString a : QuotedString b : ss) = a ++ ('\'':(foldStrings ((QuotedString b):ss)))
                    foldStrings (QuotedString a: ss) = a++ (foldStrings ss)

        trueconstant = do
            mkExpr <- getMkExpr
            simple_string "TRUE" <?> "TRUE keyword"
            return $ mkExpr (Const $ ConstBool True) []
        falseconstant = do
            mkExpr <- getMkExpr
            simple_string "FALSE" <?> "FALSE keyword"
            return $ mkExpr (Const $ ConstBool False) []
        nilconstant = do
            mkExpr <- getMkExpr
            simple_string "NIL" <?> "NIL keyword"
            return $ mkExpr (Const $ ConstNil) []



switchMaybe :: Maybe a -> b -> b -> b
switchMaybe mb j n =
    case mb of
        Just _ -> j
        Nothing -> n



withst = do
    ws
    simple_string "WITH" <?> "WITH stmt"
    exp <- expr
    exprs <- many (do ws ; char ','; expr)
    ws
    simple_string "DO" <?> "DO stmt"
    cb <- codeblock
    ws
    return $ WithSt (exp:exprs) cb

gotost = do
    ws
    simple_string "GOTO" <?> "GOTO stmt"
    ide <- identif
    return $ GotoSt ide

repeatst = do
    ws
    simple_string "REPEAT" <?> "REPEAT stmt"
    ws
    cbs <- concatMap (\(CodeBlock cb) -> cb) <$> sepEndBy (try codeblock) ((try ws >> char ';'))
    ws
    simple_string "UNTIL" <?> "UNTIL stmt"
    cond <- expr
    return $ RepeatSt (CodeBlock cbs) cond

ifst = do
    simple_string "IF" <?> "IF stmt"
    ws
    ex <- expr
    ws
    simple_string "THEN" <?> "THEN stmt"
    ws
    la <- lookAhead anyChar
    the <- if (la == ';')
        then return $ CodeBlock []
        else codeblock
    -- trace ("after then: "++(show la))
    ws
    els <- optionMaybe $ try $ do
        ws
        simple_string "ELSE" <?> "ELSE stmt"
        ws
        codeblock
    ws
    return $ IfSt ex the $ fromMaybe (CodeBlock []) els

whilest = do
    simple_string "WHILE" <?> "WHILE stmt"
    cond <- expr
    ws
    simple_string "DO" <?> "DO stmt"
    cb <- (try codeblock <|> (lookAhead (char ';') >> (return $ CodeBlock [])))
    return $ WhileSt cond cb

statement :: Parser Statement
statement = do
    ws
    c <- lookAhead anyChar
    pos <- getPosition
    -- trace ("reached statement: "++(show c)++" @ " ++(show pos))
    ws
    val <- {- try noop <|> -} try withst <|> try labelst <|> try casest <|> try ifst <|> try gotost <|> try asmst <|> try forst <|> try whilest <|> try repeatst <|> evalassignst
    nx <- optionMaybe $ lookAhead anyChar
    -- trace ("parsed stmt: "++(show val)++" nx="++(show nx)) $
    return val
    where
        noop :: Parser Statement
        noop = do
            char ';'
            return NoopSt
        labelst = do
            ide <- identif
            char ':'
            notFollowedBy (char '=')
            ws
            empty <- optionMaybe $ char ';'
            case empty of
                Just _ -> return $ LabelledStatement ide NoopSt
                Nothing -> do
                    stmt <- statement
                    return $ LabelledStatement ide stmt
        evalassignst = do
            ws
            left <- expr
            ws
            rp <- optionMaybe $ try $ do
                ws
                char ':'
                char '='
                expr
            case rp of
                Nothing -> return $ EvalSt left
                Just e -> return $ AssignSt left e
        forst = do
            simple_string "FOR" <?> "FOR stmt"
            varr <- expr
            ws
            char ':'
            char '='
            fromm <- expr
            ws
            downto <- (try (simple_string "TO" <?> "TO stmt")) <|> (simple_string "DOWNTO" <?> "DOWNTO stmt")
            too <- expr
            ws
            simple_string "DO"  <?> "DO stmt"
            ws
            cb <- (try codeblock <|> (lookAhead (char ';') >> (return $ CodeBlock [])))
            return $ ForSt varr fromm (downto == "TO") too cb

asmst = do
    simple_string "ASM" <?> "ASM block"
    AsmSt <$> manyTill anyChar (try $ do
            simple_string "END" <?> "END of asm block"
            ws
            semi <- lookAhead anyChar
            if (semi == ';')
                then return ()
                else fail "oops, asm waiting ;" -- oneOf "!" >> return () -- fail here
            )

casest = do
    simple_string "CASE" <?> "CASE stmt"
    ex <- expr
    ws
    simple_string "OF"  <?> "OF keyword"
    cases <- sepEndBy ( try $ do
         casecase <- expr_ "" 0
         casecases <- many $ try $ do
            ws
            char ','
            expr_ "" 0
         ws
         char ':'
         cb <- codeblock
         ws
         return (casecase:casecases, cb)) (char ';')
    ws
    dflt <- optionMaybe $ try $ do
        simple_string "ELSE"
        ws
        cb <- codeblock
        ws
        (char ';')
        return cb
    ws
    simple_string "END" <?> "END(of case)"
    return $ CaseSt ex cases $ fromMaybe (CodeBlock []) dflt

strUpper = map toUpper

unit = do
    simple_string "UNIT" <?> "UNIT keyword"
    iden <- identif
    ws
    char ';'
    ws
    simple_string "INTERFACE" <?> "interface keyword"
    ws
    decls <- many $ try declaration
    ws
    simple_string "IMPLEMENTATION" <?> "implementation keyword"
    ws
    unitBody <- many $ try declarationBody
    emptyDecl <- optionMaybe ( try $ do ws; simple_string "END" <?> "END(of unit)"; ws; char '.'; ws; eof )
    initCode <- case emptyDecl of
        Just _ -> return $ CodeBlock []
        Nothing -> do
                cb <- codeblock;
                ws; char '.'; ws; eof
                return cb
    return $ CompiledUnit $ Unit iden decls unitBody initCode

program = do
    trace "trying program" ws
    simple_string "PROGRAM" <?> "PROGRAM keyword"
    iden <- identif
    ws
    char ';'
    ws
    programBody <- many $ try declarationBody
    emptyDecl <- optionMaybe ( try $ do ws; simple_string "END" <?> "END(of program)"; ws; char '.'; ws; eof )
    initCode <- case emptyDecl of
        Just _ -> return $ CodeBlock []
        Nothing -> do
                c <- lookAhead anyChar
                cb <- codeblock;
                ws; char '.'; (try ws); eof
                return cb
    return $ CompiledProgram $ Program iden programBody initCode


pascal = do
    ws
    up <- (try unit) <|> program
    pos <- getPosition
    return (up, pos)

parseFile :: FilePath -> IO (Maybe Compiled)
parseFile fn = do
    withFile fn ReadMode $ \fh -> do
        cp866 <- mkTextEncoding "CP866"
        hSetEncoding fh cp866
        s <- hGetContents fh
        let val = parse pascal fn s
        case val of
            Left err -> do
                print err
                return Nothing
            Right (val,pos) -> do
                putStrLn $ "parsed "++fn++ " - "++(show $(sourceLine pos-1))++" lines."
                return $ Just val

parseText pa txt =
    parse pa "some text" txt

isPascalFile f =
    take 4 (reverse $ map toUpper f) == "SAP."

readDir path ma = do
    allFiles <- filter isPascalFile <$> getDirectoryContents path
    return $ foldr (\f m -> M.insert (map toUpper f) (path ++ "/" ++ f) m) ma allFiles

prepareProject project = do
    fileMap <- foldrM readDir M.empty (projectDirectories project)
    return $ project { projectCollectedFiles = fileMap }

getMkExpr = do
    epos <- pos
    return $ (\op lst -> Expr op lst epos)
