import System
import System.IO
import System.Environment
import System.Console.GetOpt

import Test.Falderal.Common
import Test.Falderal.Loader (loadFiles)
import Test.Falderal.Runner
import Test.Falderal.Formatter (format)
import Test.Falderal.Reporter (report)

--
-- This module contains entry points to Falderal functionality intended
-- for use by users.
--

--
-- Command-line options
--

data Flag = ReportFormat String
          | Verbosity Integer
    deriving (Show, Ord, Eq)

determineReportFormat [] = "standard"
determineReportFormat (ReportFormat fmt:_) = fmt
determineReportFormat (_:rest) = determineReportFormat rest

determineVerbosity [] = 0
determineVerbosity (Verbosity v:_) = v
determineVerbosity (_:rest) = determineVerbosity rest

--
-- Command-line entry point
--

main :: IO ()
main = do
    args <- getArgs
    case getOpt Permute options args of
        (flags, newArgs, [])     -> dispatch newArgs flags
        (_,     _,       msgs)   -> error $ concat msgs ++ usageInfo header options

header = "Usage: falderal <command> [<option>...] <filename.falderal>...\n\
         \where <command> is one of:\n\
         \    test\n\
         \    format <format-name>"

options :: [OptDescr Flag]
options = [
    Option ['v'] ["verbosity"] (ReqArg ReportFormat "LEVEL") "verbosity level, higher is more verbose (default: 0)",
    Option ['r'] ["report"] (ReqArg ReportFormat "FORMAT") "success/failure report format (default: standard)"
  ]

dispatch ("format":formatName:fileNames) _ = do
    (lines, blocks) <- loadFiles fileNames
    putStr $ format formatName lines blocks

dispatch ("test":fileNames) flags =
    let
        reportFormat = determineReportFormat flags
    in do
        (lines, blocks) <- loadFiles fileNames
        haskellBlocks <- return $ extractBlocks (isHaskellTest) blocks
        shellBlocks <- return $ extractBlocks (isShellTest) blocks
        testHaskell haskellBlocks reportFormat
        testShell shellBlocks reportFormat
        exitWith ExitSuccess

dispatch _ _ = putStrLn header

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

extractBlocks pred [] =
    []
extractBlocks pred ((Test fns desc inp exp):rest) =
    case filter (pred) fns of
        [] ->
           extractBlocks (pred) rest
        fns' ->
           ((Test fns' desc inp exp):(extractBlocks (pred) rest))
extractBlocks pred (_:rest) =
    extractBlocks (pred) rest

isHaskellTest (HaskellTest _ _) = True
isHaskellTest _ = False

isShellTest (ShellTest _) = True
isShellTest _ = False
