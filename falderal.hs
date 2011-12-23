import System
import System.Environment
import System.Console.GetOpt

import Test.Falderal.Common
import Test.Falderal.Loader (loadFiles, parseFunctionality)
import Test.Falderal.Partitioner (
                                   partitionTests,
                                   isHaskellFunctionality,
                                   isShellFunctionality
                                 )
import Test.Falderal.Formatter (format)
import Test.Falderal.Runner (runTests)
import Test.Falderal.Reporter (report)

--
-- Main module for the `falderal` tool -- a command-line interface to
-- `Test.Falderal`.
--

--
-- Command-line options
--

data Flag = ReportFormat String
          | HaskellRunCommand String
          | ShellRunCommand String
          | Verbosity String
          | Functionality String
          | Messy
    deriving (Show, Ord, Eq)

determineReportFormat [] = "standard"
determineReportFormat (ReportFormat fmt:_) = fmt
determineReportFormat (_:rest) = determineReportFormat rest

determineVerbosity [] = 0
determineVerbosity (Verbosity v:_) = (read v) :: Int
determineVerbosity (_:rest) = determineVerbosity rest

determineHaskellRunCommand [] = "runhaskell"
determineHaskellRunCommand (HaskellRunCommand s:_) = s
determineHaskellRunCommand (_:rest) = determineHaskellRunCommand rest

determineShellRunCommand [] = "sh"
determineShellRunCommand (ShellRunCommand s:_) = s
determineShellRunCommand (_:rest) = determineShellRunCommand rest

collectFunctionalityDefinitions [] = []
collectFunctionalityDefinitions (Functionality spec:rest) =
    (parseFunctionalitySpec spec:collectFunctionalityDefinitions rest)
collectFunctionalityDefinitions (_:rest) =
    collectFunctionalityDefinitions rest

-- -f 'Run Pixley Program:shell command "./pixley.sh %(test)"'

parseFunctionalitySpec str =
    let
        name = takeWhile (\c -> c /= ':') str
        rest = tail (dropWhile (\c -> c /= ':') str)
    in
        (name, parseFunctionality rest)

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
    Option ['h'] ["haskell-command"] (ReqArg HaskellRunCommand "CMD") "command to run Haskell tests (default: 'runhaskell')",
    Option ['f'] ["functionality"] (ReqArg Functionality "SPEC") "specify implementation of a functionality under test",
    Option ['m'] ["messy"] (NoArg Messy) "messy: do not delete generated files (default: clean)",
    Option ['r'] ["report-format"] (ReqArg ReportFormat "FORMAT") "success/failure report format (default: standard)",
    Option ['s'] ["shell-command"] (ReqArg ShellRunCommand "CMD") "command to run shell scripts (default: 'sh')",
    Option ['v'] ["verbosity"] (ReqArg Verbosity "LEVEL") "verbosity level, higher is more verbose (default: 0)"
  ]

dispatch ("format":formatName:fileNames) _ = do
    (lines, blocks) <- loadFiles fileNames []
    putStr $ format formatName lines blocks

dispatch ("test":fileNames) flags =
    let
        reportFormat = determineReportFormat flags
        verbosity = determineVerbosity flags
        funcDefs = collectFunctionalityDefinitions flags
        preds = [isHaskellFunctionality, isShellFunctionality]
    in do
        (lines, blocks) <- loadFiles fileNames funcDefs
        [haskellBlocks, shellBlocks] <- return $ partitionTests preds blocks
        haskellBlocks' <- testHaskell haskellBlocks flags
        shellBlocks' <- testShell shellBlocks flags
        report reportFormat (haskellBlocks' ++ shellBlocks')
        exitWith ExitSuccess

dispatch ("version":_) _ = do
    putStrLn "Test.Falderal version 0.5"

dispatch _ _ = putStrLn header

testHaskell blocks flags =
    runTests blocks "GeneratedFalderalTests.hs" "haskell" ((determineHaskellRunCommand flags) ++ " GeneratedFalderalTests.hs") (Messy `elem` flags)

testShell blocks flags =
    runTests blocks "GeneratedFalderalTests.sh" "shell" ((determineShellRunCommand flags) ++ " GeneratedFalderalTests.sh") (Messy `elem` flags)
