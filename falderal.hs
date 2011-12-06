import System
import System.IO
import System.Environment
import System.Console.GetOpt

import Test.Falderal.Common
import Test.Falderal.Loader (loadFiles)
import Test.Falderal.Partitioner (partitionTests, isHaskellTest, isShellTest)
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

determineHaskellCommand [] = "ghc -e testModule"
determineHaskellCommand (HaskellCommand s:_) = s
determineHaskellCommand (_:rest) = determineHaskellCommand rest

determineShellCommand [] = "sh"
determineShellCommand (ShellCommand s:_) = s
determineShellCommand (_:rest) = determineShellCommand rest

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
         \    format <format-name>\n\
         \    test\n\
         \    version"

options :: [OptDescr Flag]
options = [
    Option ['h'] ["haskell-command"] (ReqArg HaskellCommand "CMD") "command to run Haskell tests (default: 'ghc -e testModule')",
    Option ['r'] ["report-format"] (ReqArg ReportFormat "FORMAT") "success/failure report format (default: standard)",
    Option ['s'] ["shell-command"] (ReqArg ShellCommand "CMD") "command to run shell scripts (default: 'sh')",
    Option ['v'] ["verbosity"] (ReqArg Verbosity "LEVEL") "verbosity level, higher is more verbose (default: 0)"
  ]

dispatch ("format":formatName:fileNames) _ = do
    (lines, blocks) <- loadFiles fileNames
    putStr $ format formatName lines blocks

dispatch ("test":fileNames) flags = do
    (lines, blocks) <- loadFiles fileNames
    [haskellBlocks, shellBlocks] <- return $ partitionTests [isHaskellTest, isShellTest] blocks
    testHaskell haskellBlocks flags
    testShell shellBlocks flags
    exitWith ExitSuccess

dispatch ("version":_) _ = do
    putStrLn "Test.Falderal version 0.4"

dispatch _ _ = putStrLn header

--
-- Requires ghc.  Requires Test.Falderal is in the package path
-- (easiest way to ensure this is to install it as a Cabal package)
-- TODO: require only runhaskell.
--
testHaskell blocks flags =
    runTests blocks flags
        ("GeneratedFalderalTests.hs", "haskell", (determineHaskellCommand flags) ++ " GeneratedFalderalTests.hs")

testShell blocks flags =
    runTests blocks flags
        ("GeneratedFalderalTests.sh", "shell", (determineShellCommand flags) ++ " GeneratedFalderalTests.sh")

runTests [] _ _ = do
    return ExitSuccess
runTests blocks flags (filename, formatName, command) =
    let
        reportFormat = determineReportFormat flags
        verbosity = determineVerbosity flags
    in do
        outputFileHandle <- openFile filename WriteMode
        text <- return $ format formatName [] blocks
        hPutStr outputFileHandle text
        hClose outputFileHandle
        exitCode <- system command
        system ("rm -f " ++ filename)
        return exitCode
