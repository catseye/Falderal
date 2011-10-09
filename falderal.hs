import System
import System.IO
import System.Environment

import Test.Falderal.Common
import Test.Falderal.Loader (loadFiles)
import Test.Falderal.Runner
import Test.Falderal.Formatter (format)
import Test.Falderal.Reporter (report)

--
-- This module contains entry points to Falderal functionality intended
-- for use by users.
--

main :: IO ()
main = do
    args <- getArgs
    dispatch args

dispatch ("format":formatName:fileNames) = do
    (lines, blocks) <- loadFiles fileNames
    putStr $ format formatName lines blocks

dispatch ("test":reportFormat:fileNames) = do
    (lines, blocks) <- loadFiles fileNames
    haskellBlocks <- return $ filter (isHaskellTest) blocks
    shellBlocks <- return $ filter (isShellTest) blocks
    testHaskell haskellBlocks reportFormat
    testShell shellBlocks reportFormat
    exitWith ExitSuccess

dispatch _ = do
    putStrLn "Usage: falderal command {args}"
    putStrLn "where command is one of:"
    putStrLn "    format format-name {falderal-filenames}"
    putStrLn "    test report-style {falderal-filenames}"

--
-- Requires ghc.  Requires Test.Falderal is in the package path
-- (easiest way to ensure this is to install it as a Cabal package)
-- TODO: require only runhaskell.
-- TODO: allow "runhaskell" to be overridden with a cmd line opt.
--

testHaskell [] _ = do
    return ExitSuccess
testHaskell blocks reportFormat = do
    outputFileHandle <- openFile "GeneratedFalderalTests.hs" WriteMode
    hPutStr outputFileHandle $ format "haskell" [] blocks
    hClose outputFileHandle
    exitCode <- system "ghc GeneratedFalderalTests.hs -e testModule"
    system "rm -f GeneratedFalderalTests.hs"
    return exitCode

testShell [] _ = do
    return ExitSuccess
testShell blocks reportFormat = do
    outputFileHandle <- openFile "GeneratedFalderalTests.sh" WriteMode
    text <- return $ format "shell" [] blocks
    hPutStr outputFileHandle text
    hClose outputFileHandle
    exitCode <- system "sh GeneratedFalderalTests.sh"
    system "rm -f GeneratedFalderalTests.sh"
    return exitCode

isHaskellTest (Test (HaskellTest _ _) _ _ _) = True
isHaskellTest _ = False

isShellTest (Test (ShellTest _) _ _ _) = True
isShellTest _ = False
