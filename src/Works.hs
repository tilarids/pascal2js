{-

see README for terms of use

-}
module Works where

import Data.Char
import Data.List
import Data.Either
import Data.Maybe
import Debug.Trace
import qualified Data.Map as M
import Types
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Loops
import Control.Applicative

traceVal lbl val =
    trace (lbl ++ (show val)) val


isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

publicDecls (CompiledProgram (Program _ impl _)) = impl
publicDecls (CompiledUnit (Unit _ intf impl _)) = intf
publicDecls _ = []

privateDecls (CompiledProgram (Program _ impl _)) = []
privateDecls (CompiledUnit (Unit _ intf impl _)) = impl
privateDecls _ = []

collectUsedUnits comp = collectUses (privateDecls comp) ++ collectUses (publicDecls comp)
collectUsedPrivatelyUnits comp = collectUses (privateDecls comp)
collectUsedPubliclyUnits comp = collectUses (publicDecls comp)


mainUnit proj =
    let mf = projectMainFile proj
    in fromJustX "mainUnit" $ findUnit mf proj

findUnit nm proj =
    M.lookup nm (projectParsedUnits proj)


mkScope proj = Scope proj initialTypeScope initialSymbolScope (CompiledCache M.empty) (CompiledCache M.empty) Nothing []

getProj :: State Scope Project
getProj =
    scopeProject <$> get

getCompiledCache :: Bool -> State Scope CompiledCache
getCompiledCache doImpl =
    (if doImpl then scopeCompiled2 else scopeCompiled1) <$> get

putCompiledCache :: Bool -> CompiledCache -> State Scope ()
putCompiledCache doImpl cache = do
    if doImpl then
        modify (\o -> o { scopeCompiled2 = cache })
      else
        modify (\o -> o { scopeCompiled1 = cache })

typecheckProgram m = do
    addEvent EventProgramStart
    diveInto m False        -- interfaces only
    CompiledCache cache <- getCompiledCache False
    mapM_ (\CompiledTyped {ctCompiled = m } -> diveInto m True ) $ map snd $ M.toList cache
--    diveInto m True         -- implementation

-- repeatM = flip whileM_ (return ())


diveInto :: Compiled -> Bool -> State Scope ()
diveInto m doImplementation = do
    let myName = compiledName m
    (CompiledCache cache) <- getCompiledCache doImplementation
    case M.lookup myName cache of
        Nothing -> do
--            if doImplementation then trace "
--            proj <- getProj
            importNeededUnits doImplementation $ collectUsedPubliclyUnits m
            trace ("typechecking unit " ++ (show myName) ++ " " ++ (show doImplementation)) $ modify (\o -> o { scopeCurrentUnit = Just m })
            (newTypes, newSymbols) <- processDecls (UnitScope myName) m $ publicDecls m
            if (doImplementation)
              then do
                importNeededUnits doImplementation $ collectUsedPrivatelyUnits m
                (_, newPrivSymbols) <- processDecls (UnitScope myName) m $ privateDecls m
                CompiledCache ncache <- getCompiledCache doImplementation
                putCompiledCache doImplementation $ CompiledCache $ M.insert myName (CompiledTyped m (TypeScope newTypes) (SymbolScope newSymbols)) ncache
              else do
                CompiledCache ncache <- getCompiledCache doImplementation
                putCompiledCache doImplementation $ CompiledCache $ M.insert myName (CompiledTyped m (TypeScope newTypes) (SymbolScope newSymbols) ) ncache
        Just _ -> return ()
    return ()

processDecls site m decls = do
    Scope {scopeTypes = TypeScope scopeBeforeTypes, scopeSymbols = SymbolScope scopeBeforeSymbols} <- get
    resolveTypesVariablesProcedures site m decls
    Scope {scopeTypes = TypeScope scopeAfterTypes, scopeSymbols = SymbolScope scopeAfterSymbols} <- get
    let newTypes = take (length scopeAfterTypes - length scopeBeforeTypes) scopeAfterTypes
    let newSymbols = take (length scopeAfterSymbols - length scopeBeforeSymbols) scopeAfterSymbols
    return (newTypes, newSymbols)


isTypeDecl (TypeDecl _) = True
isTypeDecl _ = False

isVarDecl (VarDecl _) = True
isVarDecl _ = False

isFunDecl (FunDecl  _ _) = True
isFunDecl _ = False

isFunHeadDecl (FunHeadDecl _) = True
isFunHeadDecl _ = False

isUsesDecl (UsesDecl _) = True
isUsesDecl _ = False

resolveTypes :: DeclarationSite -> Compiled -> [Declaration] -> State Scope ()
resolveTypes site m decls = do
    let addAll typez = do
        remaining <- catMaybes <$> mapM addType typez
        if remaining == []
            then return ()
            else if remaining == typez
                then do
                    sco@Scope { scopeTypes = (TypeScope typez) } <- get
                    let (_, nm,rm) = head remaining :: (DeclarationSite, String, TypeRef)
                    let (Left err) = resolveType typez rm
                    fail $ "while resolving type: "++(compiledName m) ++"."++nm++" - unresolvable types: "++err
                else
                    addAll remaining
    let extractTypes (TypeDecl pairs) = map (\(a,b) -> (site, a, b)) pairs
    addAll $ ((concatMap extractTypes $ filter isTypeDecl decls) :: [(DeclarationSite,String,TypeRef)] )
    return ()


resolveVariables :: DeclarationSite -> Compiled -> [Declaration] -> State Scope ()
resolveVariables site m decls = do
    let extractVars (VarDecl trips) = map (\(a,b,c) -> (site, a, b, c )) trips
    mapM (addVar m) $ ((concatMap extractVars $ filter isVarDecl decls) :: [(DeclarationSite,String,Maybe TypeRef,Maybe Expr)] )
    return ()

resolveTypesVariablesProcedures :: DeclarationSite -> Compiled -> [Declaration] -> State Scope ()
resolveTypesVariablesProcedures site m decls = mapM_ resolveTypesVariablesProcedure decls
  where
     resolveTypesVariablesProcedure decl
            | isVarDecl decl = resolveVariables site m [decl]
            | isTypeDecl decl = resolveTypes site m [decl]
            | isFunHeadDecl decl || isFunDecl decl  = resolveProcedures site m [decl]
            -- | isUsesDecl decl = importNeededUnits decl
            | otherwise = return () -- error $ "oops in resolveTypesVariablesProcedures: "++show decl





addMaybes (Just a) (Just b) = Just a
addMaybes Nothing (Just b) = Just b
addMaybes (Just a) Nothing = Just a
addMaybes Nothing Nothing = Nothing

resolveProcedures :: DeclarationSite -> Compiled -> [Declaration] -> State Scope ()
resolveProcedures site m decls = do
    -- let extractVars (VarDecl trips) = trips
    let funs = filter isFunHeadDecl decls
    mapM_ (\(FunHeadDecl (FunctionHeadDecl nm arg mtr)) -> do
        addVar m (site, nm, Just $ TypeRefProcedure arg mtr False, Nothing)) funs
    let funsB = filter isFunDecl decls
    mapM_ (\(FunDecl (FunctionHeadDecl nm arg mtr) (scopeblock)) -> do
        Scope { scopeSymbols = SymbolScope symz } <- get
        jtrp@(Just (TypeRefProcedure arg' mtr' _)) <- mresolveType ("proc/fun " ++ nm) $ Just (TypeRefProcedure arg mtr False)
        let nexpr njtrp@(Just (TypeRefProcedure argz mtr'' _))  = case scopeblock of
                    Nothing ->
                        return Nothing
                    Just sblk@(ScopeBlock sdecls code) -> do
                        saved <- getSaved
--                        addVar m  (nm, njtrp, Nothing)
                        mapM_ addArgIntoScope argz
                        when (isJust mtr'') $
                            -- trace ("retval mtr=" ++ (show mtr'')) $
                            addVar m (FunctionRetVal, nm, mtr'', Nothing)
                        addVar m (FunctionItselfSpec, "$TMP$" ++ nm, njtrp, Nothing)
                        processDecls LocalScope m sdecls
                        -- (code', hasYields) <- traceYields code
                        addEvent $ EventCodeBlock nm code
                        let retval = Nothing -- Just $ (Expr (SomeCode sblk) [] "")
                        putSaved saved
                        return retval
        case lookup nm symz of
            Just (_, Just ( TypeRefProcedure oarg omtr _), mexpr) -> do
                when ((arg' /= []) && (arg' /= oarg)) $ fail $ "proc/fun was declared above, with different args: " ++ nm ++ show (oarg, arg)
                when (isJust mtr' && mtr' /= omtr) $ fail $ "proc/fun was declared above, with different return type: " ++ nm ++ show (omtr, mtr')
                when (isJust mexpr) $ fail "procedure seems to be already defined!"
                let nargs = if arg' == [] && oarg /= [] then oarg else arg'
                let nmtr = addMaybes omtr mtr'
                let nsymz = filter (\(f,_) -> f /= nm) symz
                modify (\o -> o { scopeSymbols = SymbolScope nsymz} )       -- remove old proc decl
                let njtrp = Just $ TypeRefProcedure nargs nmtr False
                addEvent $ EventFunctionStart site  nm (fromJustX "resolveProcedures-1" njtrp)
                nexpr' <- nexpr njtrp
                addVar m (site, nm, njtrp, nexpr')
            _ -> do             -- if variable was defined with same name, this is allowed
                addEvent $ EventFunctionStart site nm (fromJustX "resolveProcedures-2" jtrp)
                nexpr' <- nexpr jtrp
                addVar m (site, nm, jtrp, nexpr')
        ) funsB
    where
      addArgIntoScope (Argument _ anm mtr) = addVar m (FunctionArgScope, anm, mtr, Nothing)
      addArgIntoScope (AnyArgument _) = error $ "can't be (addArgIntoScope)"

findType msg tname = do
    sco@Scope { scopeTypes = (TypeScope typez) } <- get
    case lookup tname typez of
        Just v -> return v
        Nothing -> do
            fail $ msg ++ " in module " ++ (compiledName $ fromJustX "findType" $ scopeCurrentUnit sco) ++ " - unable to find type: "++tname

mresolveType :: String -> Maybe TypeRef -> State Scope (Maybe TypeRef)
mresolveType ctx tr = do
    sco@(Scope { scopeTypes = TypeScope typez }) <- get
    case (fmap (resolveType typez) tr) of
                Just (Right newref) -> return $ Just newref
                Just (Left err) -> fail $ ctx ++ err
                Nothing -> return $ Nothing


addVar :: Compiled -> (DeclarationSite, String, Maybe TypeRef, Maybe Expr) -> State Scope ()
addVar m (site, name, mtr, initval) = do
    sco@(Scope { scopeTypes = TypeScope typez, scopeSymbols = SymbolScope symz }) <- get
    mtr' <- mresolveType ("resolving var: " ++ name ++ " in module " ++ (compiledName $ fromJustX "addVar" $ scopeCurrentUnit sco)) mtr
    case lookup name symz of
        Just _ -> fail $ "Symbol already in scope: "++name
        Nothing -> return ()
    let varr = (name,(site,mtr',initval))
    put $ sco { scopeSymbols = SymbolScope (varr:symz) }
    addEvent $ EventVariable site name mtr' initval
    return ()


addType :: (DeclarationSite, String, TypeRef) -> State Scope (Maybe (DeclarationSite, String, TypeRef))
addType (site, name, ref) = do
    sco@Scope { scopeTypes = (TypeScope typez), scopeSymbols = (SymbolScope symz) } <- get
    let nref = resolveType typez ref
    case nref of
        Just newref -> do
            let tr = TypeRefProcedure [AnyArgument False] (Just newref) False
            put $ sco {
                scopeTypes = TypeScope ((name, (site, newref)):typez),
                scopeSymbols = SymbolScope ( (name, (site, Just tr, Nothing)) : symz)
                 }
            return Nothing
        Nothing ->
            return $ Just (site, name, ref)

resolveType  :: (Functor m, Monad m) => [(String,(DeclarationSite,TypeRef))]-> TypeRef -> m TypeRef
resolveType typez typ =
    resolveType' typ
    where
--        resolveType' :: Monad m => TypeRef -> m TypeRef
        resolveType' (TypeRefByName nm)  = case lookup nm typez of
                                            Just (_,t) -> return t
                                            Nothing -> fail $ "Cannot resolve type: "++(show nm)++" available: "++(show $ map fst typez)
        resolveType' (TypeRefPointer (TypeRefByName nm)) =
                                            return $ case lookup nm typez of
                                                Just (_,t) -> TypeRefPointer t
                                                Nothing -> TypeRefSoftPointer nm            -- so be it
        resolveType' (TypeRefPointer tr) = TypeRefPointer <$> resolveType' tr
        resolveType' (TypeRefRecord fields) = TypeRefRecord <$> mapM (\(n,t) -> ((,) n) <$> resolveType' t) fields
        resolveType' (TypeRefArray exprs tr) = TypeRefArray exprs <$> resolveType' tr
        resolveType' (TypeRefSet (Right tr)) = (TypeRefSet . Right) <$> resolveType' tr
        resolveType' (TypeRefProcedure argz mtr _) = do
            argz <- mapM resolveArgument' argz
            case mtr of
                Just tr -> do
                    typ <- resolveType' tr
                    return $ TypeRefProcedure argz (Just typ) False
                Nothing ->
                    return $ TypeRefProcedure argz Nothing False
        resolveType' typ = return typ

        resolveArgument' a@(Argument _ _ Nothing) = return a
        resolveArgument' (Argument b s (Just tr)) = (Argument b s . Just) <$> resolveType' tr

{-
                    TypeRefPointer TypeRef |
                    TypeRefRecord [(String,TypeRef)] |
                    -- TypeRefUnion [(Expr, [(String,TypeRef)])] |        -- case record of
                    TypeRefArray [Expr] TypeRef |         -- expr = range expr
                    TypeRefSet (Either Expr TypeRef) |
                    TypeRefProcedure [Argument] (Maybe TypeRef)
                    deriving (Show,Read,Eq)

-}



collectTypeRefs (TypeRefSet (Right r)) = concatMap collectTypeRefs [r]
collectTypeRefs (TypeRefArray _ r) = concatMap collectTypeRefs  [r]
collectTypeRefs (TypeRefRecord cs) = concatMap collectTypeRefs $ map snd cs
collectTypeRefs (TypeRefPointer r) = concatMap collectTypeRefs [r]
collectTypeRefs (TypeRefProcedure as (Just r) _) = (concatMap collectTR1 as) ++ (concatMap collectTypeRefs [r])
                                            where collectTR1 (Argument _ _ (Just r)) = concatMap collectTypeRefs[r]
collectTypeRefs _ = []

augmentScope :: Scope -> TypeScope -> SymbolScope -> Scope
augmentScope scope (TypeScope pt) (SymbolScope ps) =
 scope { scopeSymbols = ssmap (ps ++) (scopeSymbols scope),
          scopeTypes =  tsmap (pt ++) (scopeTypes scope) }

importNeededUnits _ [] = return ()
importNeededUnits False (u:units) = do
    proj <- getProj
    case findUnit u proj of
        Nothing -> fail $ "Cannot find unit: "++u
        Just un -> do
            saved <- getSaved
            diveInto un False
            newCache@(CompiledCache m)  <- getCompiledCache False
            case M.lookup (compiledName un) m of
                Just ct -> do
                    let pt = ctPublicTypes ct
                    let ps = ctPublicSymbols ct
                    putSaved $ augmentScope saved pt ps
                Nothing ->  do -- bad module
                    putSaved saved
                    return ()
            putCompiledCache False newCache   -- copied over
            importNeededUnits False units

importNeededUnits True (u:units) = do
    newCache@(CompiledCache m)  <- getCompiledCache False
    case M.lookup u m of
        Just ct -> do
            let pt = ctPublicTypes ct
            let ps = ctPublicSymbols ct
            saved <- getSaved
            putSaved $ augmentScope saved pt ps
            importNeededUnits True units
        Nothing -> error $ "importNeededUnits: not found: " ++ (show u)


processSingleUnit :: State Scope ()
processSingleUnit = do
    return ()

putSaved scope = do
    ev <- scopeEvents <$> get
    put $ scope { scopeEvents = ev }

getSaved = get

addEvent :: EventType -> State Scope ()
addEvent et = do
    sco <- get
    let ev = Event et sco
    modify (\o -> o { scopeEvents = ev:(scopeEvents o) })




