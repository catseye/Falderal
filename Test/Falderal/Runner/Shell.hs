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
    r <- openFile resultsFilename WriteMode
    hClose r
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
    newlinify outputFileHandle contents
    hClose outputFileHandle

execute cmd resultsFilename id = do
    exitCode <- system (cmd ++ " 2>&1")
    r <- openFile resultsFilename AppendMode
    writeExitCode r exitCode
    hSetNewlineMode r noNewlineTranslation
    hPutStrLn r (show id)
    text <- readFile "output.txt"
    hPutStrLn r $ show $ length $ lines text
    newlinify r text
    hClose r
    return ()

writeExitCode handle ExitSuccess = do
    hPutStrLn handle "output"
writeExitCode handle (ExitFailure _) = do
    hPutStrLn handle "exception"

newlinify handle text = do
    case last text of
        '\n' -> hPutStr handle text
        _ -> hPutStrLn handle text
