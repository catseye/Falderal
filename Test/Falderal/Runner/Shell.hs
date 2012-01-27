module Test.Falderal.Runner.Shell (run) where

--
-- Test.Falderal.Run.Shell -- Run shell tests directly from falderal
--

import System
import System.IO

import Test.Falderal.Common

import Test.Falderal.Formatter.Shell (expandCommand)


cleanRun True cmd = do
    system cmd
    return ()
cleanRun False cmd = do
    return ()


run :: [Block] -> String -> Bool -> IO ()

run blocks resultsFilename messy = do
    system ("echo -n \"\" >" ++ resultsFilename)
    runBlocks blocks resultsFilename messy
    cleanRun (not messy) "rm -f input.txt output.txt"

runBlocks :: [Block] -> String -> Bool -> IO ()

runBlocks [] _ _ = do
    return ()
runBlocks (block:blocks) resultsFilename messy = do
    result <- runBlock block resultsFilename messy
    runBlocks blocks resultsFilename messy

runBlock :: Block -> String -> Bool -> IO ()

runBlock test@(Test id [(ShellTest cmd)] desc body _ _) resultsFilename messy = do
    writeOutFile "input.txt" body
    execute (expandCommand cmd body) resultsFilename id

writeOutFile filename contents = do
    outputFileHandle <- openFile filename WriteMode
    hSetNewlineMode outputFileHandle noNewlineTranslation
    hPutStr outputFileHandle contents
    hClose outputFileHandle

execute cmd resultsFilename id = do
    exitCode <- system (cmd ++ " 2>&1")
    case exitCode of
        ExitSuccess -> do
            system ("echo \"output\" >>" ++ resultsFilename)
            execute' resultsFilename id
        ExitFailure _ -> do
            system ("echo \"exception\" >>" ++ resultsFilename)
            execute' resultsFilename id

execute' resultsFilename id = do
    system ("echo " ++ (show id) ++ " >>" ++ resultsFilename)
    system ("falderal newlinify output.txt >output2.txt")
    system ("mv output2.txt output.txt")
    system ("echo `wc -l output.txt` >>" ++ resultsFilename)
    system ("cat output.txt >>" ++ resultsFilename)
    return ()
