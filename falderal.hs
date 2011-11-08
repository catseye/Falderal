import System
import System.IO
import System.Environment
import System.Console.GetOpt

import Test.Falderal.Common
import Test.Falderal.Loader (loadFiles)
import Test.Falderal.Formatter (format)
import Test.Falderal.Reporter (report)

--
-- Main module for the `falderal` tool -- a command-line interface to
-- `Test.Falderal`.
--

--
-- Command-line options
--

data Flag = ReportFormat String
          | ShowVersion
          | HaskellCommand String
          | ShellCommand String
          | Verbosity String
    deriving (Show, Ord, Eq)

determineReportFormat [] = "standard"
determineReportFormat (ReportFormat fmt:_) = fmt
determineReportFormat (_:rest) = determineReportFormat rest

determineVerbosity [] = 0
determineVerbosity (Verbosity v:_) = (read v) :: Int
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
    Option ['h'] ["haskell-command"] (ReqArg HaskellCommand "CMD") "command to run Haskell tests (default: 'ghc -e testModule')",
    Option ['r'] ["report-format"] (ReqArg ReportFormat "FORMAT") "success/failure report format (default: standard)",
    Option ['s'] ["shell-command"] (ReqArg ShellCommand "CMD") "command to run shell scripts (default: 'sh')",
    Option ['v'] ["verbosity"] (ReqArg Verbosity "LEVEL") "verbosity level, higher is more verbose (default: 0)",
    Option ['V'] ["version"] (NoArg ShowVersion) "show version and exit"
  ]

dispatch ("format":formatName:fileNames) _ = do
    (lines, blocks) <- loadFiles fileNames
    putStr $ format formatName lines blocks

dispatch ("test":fileNames) flags =
    let
        reportFormat = determineReportFormat flags
        verbosity = determineVerbosity flags
    in do
        (lines, blocks) <- loadFiles fileNames
        haskellBlocks <- return $ extractBlocks (isHaskellTest) blocks
        shellBlocks <- return $ extractBlocks (isShellTest) blocks
        testHaskell haskellBlocks reportFormat verbosity
        testShell shellBlocks reportFormat verbosity
        exitWith ExitSuccess

dispatch _ _ = putStrLn header

--
-- Requires ghc.  Requires Test.Falderal is in the package path
-- (easiest way to ensure this is to install it as a Cabal package)
-- TODO: require only runhaskell.
-- TODO: allow "runhaskell" to be overridden with a cmd line opt.
--
testHaskell blocks reportFormat verbosity =
    runTests blocks reportFormat verbosity
        ("GeneratedFalderalTests.hs", "haskell", "ghc -e testModule GeneratedFalderalTests.hs")

testShell blocks reportFormat verbosity =
    runTests blocks reportFormat verbosity
        ("GeneratedFalderalTests.sh", "shell", "sh GeneratedFalderalTests.sh")

runTests [] _ _ _= do
    return ExitSuccess
runTests blocks reportFormat verbosity (filename, formatName, command) = do
    outputFileHandle <- openFile filename WriteMode
    text <- return $ format formatName [] blocks
    hPutStr outputFileHandle text
    hClose outputFileHandle
    exitCode <- system command
    system ("rm -f " ++ filename)
    return exitCode

--
-- This will create separate tests for each functionality named
-- inside a test.  This is to ease reporting, etc.  This should
-- maybe even be a different kind of ADT, and unique-id'ing each
-- test should occur here.  And of course this should all be
-- somewhere besides falderal.hs...
--

extractBlocks pred [] =
    []
extractBlocks pred ((Test fns desc inp exp):rest) =
    case filter (pred) fns of
        [] ->
           extractBlocks (pred) rest
        fns' ->
            let
                tests = map (\fn -> (Test [fn] desc inp exp)) fns'
            in
                tests ++ extractBlocks (pred) rest
extractBlocks pred (_:rest) =
    extractBlocks (pred) rest

isHaskellTest (HaskellTest _ _) = True
isHaskellTest _ = False

isShellTest (ShellTest _) = True
isShellTest _ = False
