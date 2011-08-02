-----------------------------------------------------------------------------
--
-- Maintainer  : san @ sysdate , com
--
-----------------------------------------------------------------------------
{-

You may use this code freely to convert opensource and non-profit applications.
If you wish to convert applications used commercially, please contact
authors for license and support.
        
-}

module Main where

import Parser
import Types
import Works
import JSGen

import System.IO
import Data.Char
import Data.List
import Data.Either
import Data.Maybe
import qualified Data.Map as M
import Debug.Trace
import Control.Applicative ((<$>))
import Control.Monad(when)
import System.Directory
import System.Process
import Control.Monad.State

compileFiles proj [] = return proj
compileFiles proj (u:us) = do
    let oldPU =  projectParsedUnits proj
    let upperU = (map toUpper u)
    case M.lookup upperU oldPU of
        Just _ -> compileFiles proj us
        Nothing -> do
            let fullPath = M.lookup (upperU ++ ".PAS") $ projectCollectedFiles proj
            parsed <- case fullPath of
                Just p -> parseFile p
                Nothing -> return $ Just $ MissingUnit upperU
            case parsed of
                Just parsedok -> do
                    let units = collectUsedUnits parsedok
                    compileFiles proj { projectParsedUnits = M.insert upperU parsedok oldPU } (us ++ units)
                Nothing -> do
                    print "parse failed"
                    return proj

prettyJS str = do
    writeFile "/tmp/q" str
    system "rhino beautify.js /tmp/q > /tmp/out.js"
    readFile "/tmp/out.js"

main = do
    --let proj = mkProject ["/home/tilarids/contests/icfpc11/icfpc2011/btetr"] "btetr"
    let proj = mkProject ["../samples"] "paporotn"
--    let proj = mkProject ["/home/san/icfpc2011/btetr/bt/samples"] "prog1"
    proj <- prepareProject proj
    proj' <- compileFiles proj [projectMainFile proj]
    print $ length $ show proj'
    let m = mainUnit proj'
    let st1@Scope {scopeEvents = events} = execState (typecheckProgram m) $ mkScope proj'
    let q = show $ generateJS $ reverse events
    putStrLn $ "generating js\n"++(show $ length q)
    putStrLn $ "making pretty js: "
    qq <- prettyJS q
    putStrLn qq

    return ()

    -- print "ok"
