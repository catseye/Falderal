import System
import System.Environment
import System.Console.GetOpt

import Test.Falderal.Common
import Test.Falderal.Loader (
                              loadFile,
                              parseFunctionality,
                              collectFunctionalityDefinitions,
                              assignFunctionalities
                            )
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
          | ClearFunctionality String
          | SkipFunctionality String
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

determineFunctionalityDefinitions [] = []
determineFunctionalityDefinitions (Functionality spec:rest) =
    (parseFunctionalitySpec spec:determineFunctionalityDefinitions rest)
determineFunctionalityDefinitions (_:rest) =
    determineFunctionalityDefinitions rest

parseFunctionalitySpec str =
    let
        name = takeWhile (\c -> c /= ':') str
        rest = tail (dropWhile (\c -> c /= ':') str)
    in
        (name, parseFunctionality rest)

determineFunctionalitiesToClear [] = []
determineFunctionalitiesToClear (ClearFunctionality name:rest) =
    (name:determineFunctionalitiesToClear rest)
determineFunctionalitiesToClear (_:rest) =
    determineFunctionalitiesToClear rest

determineFunctionalitiesToSkip [] = []
determineFunctionalitiesToSkip (SkipFunctionality name:rest) =
    (name:determineFunctionalitiesToSkip rest)
determineFunctionalitiesToSkip (_:rest) =
    determineFunctionalitiesToSkip rest

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
    Option ['c'] ["clear-functionality"] (ReqArg ClearFunctionality "NAME") "clear all implementations of a named functionality",
    Option ['f'] ["functionality"] (ReqArg Functionality "SPEC") "specify additional implementation of a named functionality",
    Option ['h'] ["haskell-command"] (ReqArg HaskellRunCommand "CMD") "command to run Haskell tests (default: 'runhaskell')",
    Option ['k'] ["skip-functionality"] (ReqArg SkipFunctionality "NAME") "skip all tests for this named functionality",
    Option ['m'] ["messy"] (NoArg Messy) "messy: do not delete generated files (default: clean)",
    Option ['r'] ["report-format"] (ReqArg ReportFormat "FORMAT") "success/failure report format (default: standard)",
    Option ['s'] ["shell-command"] (ReqArg ShellRunCommand "CMD") "command to run shell scripts (default: 'sh')",
    Option ['v'] ["verbosity"] (ReqArg Verbosity "LEVEL") "verbosity level, higher is more verbose (default: 0)"
  ]

dispatch ("format":formatName:fileNames) _ = do
    (lines, blocks) <- loadFiles fileNames
    putStr $ format formatName lines blocks

dispatch ("test":fileNames) flags =
    let
        reportFormat = determineReportFormat flags
    in do
        results <- testFiles fileNames flags
        report reportFormat results
        exitWith ExitSuccess

dispatch ("newlinify":fileName:_) flags = do
    text <- readFile fileName
    case last text of
        '\n' -> putStr text
        _ -> putStrLn text
    exitWith ExitSuccess

dispatch ("version":_) _ = do
    putStrLn "Test.Falderal version 0.6"

dispatch _ _ = putStrLn header

--
-- Orchestrating the tests
--

testFiles [] flags = return []
testFiles (fileName:rest) flags =
    let
        verbosity = determineVerbosity flags
        funcDefs = determineFunctionalityDefinitions flags
        funcsToClear = determineFunctionalitiesToClear flags
        funcsToSkip = determineFunctionalitiesToSkip flags
        preds = [isHaskellFunctionality, isShellFunctionality]
    in do
        (lines, blocks) <- loadFile fileName
        fds <- return $ collectFunctionalityDefinitions lines
        fds' <- return $ clearFuncs fds funcsToClear
        blocks' <- return $ assignFunctionalities blocks [] (fds' ++ funcDefs)
        blocks'' <- return $ removeFuncsToSkip blocks' funcsToSkip
        [haskellBlocks, shellBlocks] <- return $ partitionTests preds blocks''
        haskellBlocks' <- testHaskell haskellBlocks flags
        shellBlocks' <- testShell shellBlocks flags
        further <- testFiles rest flags
        return (haskellBlocks' ++ shellBlocks' ++ further)

--
-- Transforming tests before running them
--

-- NOTE: this runs each file into the last -- not so good, defn's and such bleed
loadFiles [] = do
    return ([], [])
loadFiles (fileName:rest) = do
    (ls, bs) <- loadFile fileName
    (restLs, restBs) <- loadFiles rest
    return (ls ++ restLs, bs ++ restBs)

clearFuncs [] names = []
clearFuncs (def@(name,fn):rest) names
    | name `elem` names = clearFuncs rest names
    | otherwise         = (def:clearFuncs rest names)

removeFuncsToSkip blocks funcsToSkip =
    blocks

--
-- Running the tests
--

testHaskell blocks flags =
    runTests blocks "GeneratedFalderalTests.hs" "haskell" ((determineHaskellRunCommand flags) ++ " GeneratedFalderalTests.hs") (Messy `elem` flags)

testShell blocks flags =
    runTests blocks "GeneratedFalderalTests.sh" "shell" ((determineShellRunCommand flags) ++ " GeneratedFalderalTests.sh") (Messy `elem` flags)
