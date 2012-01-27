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


run :: [Block] -> Handle -> Bool -> IO ()

run blocks handle messy = do
    runBlocks blocks handle messy
    cleanRun (not messy) "rm -f input.txt output.txt"

runBlocks :: [Block] -> Handle -> Bool -> IO ()

runBlocks [] _ _ = do
    return ()
runBlocks (block:blocks) handle messy = do
    result <- runBlock block handle messy
    runBlocks blocks handle messy

runBlock :: Block -> Handle -> Bool -> IO ()

runBlock test@(Test id [(ShellTest cmd)] desc body _ _) handle messy = do
    writeOutFile "input.txt" body
    execute (expandCommand cmd body) handle

writeOutFile filename contents = do
    outputFileHandle <- openFile filename WriteMode
    hSetNewlineMode outputFileHandle noNewlineTranslation
    hPutStr outputFileHandle contents
    hClose outputFileHandle

execute cmd handle = do
    return ()

{--

testExecution cmd id =
    cmd ++ " 2>&1\n\
    \if [ $? != 0 ]; then\n\
    \  echo \"exception\"\n\
    \else\n\
    \  echo \"output\"\n\
    \fi\n\
    \echo " ++ (show id) ++ "\n\
    \falderal newlinify output.txt >output2.txt\n\
    \mv output2.txt output.txt\n\
    \echo `wc -l output.txt`\n\
    \cat output.txt\n"

--}
