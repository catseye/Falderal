import Char (isDigit, ord)

import System
import System.IO
import System.Process
import System.Environment
import System.Console.GetOpt

import Test.Falderal.Common
import Test.Falderal.Loader (loadFiles)
import Test.Falderal.Partitioner (
                                   partitionTests,
                                   isHaskellFunctionality,
                                   isShellFunctionality
                                 )
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
          | HaskellRunCommand String
          | ShellRunCommand String
          | Verbosity String
    deriving (Show, Ord, Eq)

determineReportFormat [] = "standard"
determineReportFormat (ReportFormat fmt:_) = fmt
determineReportFormat (_:rest) = determineReportFormat rest

determineVerbosity [] = 0
determineVerbosity (Verbosity v:_) = (read v) :: Int
determineVerbosity (_:rest) = determineVerbosity rest

determineHaskellRunCommand [] = "ghc -e testModule"
determineHaskellRunCommand (HaskellRunCommand s:_) = s
determineHaskellRunCommand (_:rest) = determineHaskellRunCommand rest

determineShellRunCommand [] = "sh"
determineShellRunCommand (ShellRunCommand s:_) = s
determineShellRunCommand (_:rest) = determineShellRunCommand rest

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
    Option ['h'] ["haskell-command"] (ReqArg HaskellRunCommand "CMD") "command to run Haskell tests (default: 'ghc -e testModule')",
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
        verbosity = determineVerbosity flags
        preds = [isHaskellFunctionality, isShellFunctionality]
    in do
        (lines, blocks) <- loadFiles fileNames
        [haskellBlocks, shellBlocks] <- return $ partitionTests preds blocks
        haskellFails <- testHaskell haskellBlocks flags
        shellFails <- testShell shellBlocks flags
        tests <- return $ assembleFailingTests (haskellBlocks ++ shellBlocks) (haskellFails ++ shellFails)
        -- putStrLn (show (haskellFails ++ shellFails))
        -- putStrLn (show (haskellBlocks ++ shellBlocks))
        -- putStrLn (show tests)
        report reportFormat tests
        exitWith ExitSuccess

dispatch ("version":_) _ = do
    putStrLn "Test.Falderal version 0.4"

dispatch _ _ = putStrLn header

--
-- Requires ghc.  Requires Test.Falderal in the package path
-- (easiest way to ensure this is to install it as a Cabal package)
-- TODO: require only runhaskell.
--
testHaskell blocks flags =
    runTests blocks flags
        ("GeneratedFalderalTests.hs", "haskell", (determineHaskellRunCommand flags) ++ " GeneratedFalderalTests.hs")

testShell blocks flags =
    runTests blocks flags
        ("GeneratedFalderalTests.sh", "shell", (determineShellRunCommand flags) ++ " GeneratedFalderalTests.sh")

captureLines command =
    let
        procDesc = (shell command){ std_out = CreatePipe }
    in do
        (_, Just hStdout, _, proc) <- createProcess procDesc
        output <- collect hStdout
        exitCode <- waitForProcess proc
        return (output, exitCode)

collect handle = do
    eof <- hIsEOF handle
    if
        eof
      then do
        return []
      else do
        line <- hGetLine handle
        remainder <- collect handle
        return (line:remainder)

-- TODO: what to do with exitCode?

runTests [] _ _ = do
    return []
runTests blocks flags (filename, formatName, command) =
    let
        reportFormat = determineReportFormat flags
        verbosity = determineVerbosity flags
    in do
        outputFileHandle <- openFile filename WriteMode
        text <- return $ format formatName [] blocks
        hPutStr outputFileHandle text
        hClose outputFileHandle
        (lines, exitCode) <- captureLines command
        system ("rm -f " ++ filename)
        return $ collectFails lines

data Fail = Fail Int String
    deriving (Ord, Eq, Show)

collectFails [] =
    []
collectFails (idStr:numLinesStr:rest) =
    let
        id = parseNumStr idStr 0
        numLines = parseNumStr numLinesStr 0
        failLines = take numLines rest
        rest' = drop numLines rest
    in
        ((Fail id (join "\n" failLines)):collectFails rest')
collectFails (idStr:rest) =
    let
        id = parseNumStr idStr 0
    in
        ((Fail id ""):collectFails rest)

parseNumStr [] acc = acc
parseNumStr (x:xs) acc
    | isDigit x = parseNumStr xs (acc * 10 + ((ord x) - (ord '0')))
    | otherwise = acc

assembleFailingTests [] fails = []
assembleFailingTests (t@(Test testId fns literalText testText expected _):tests) fails =
    case filter (\(Fail failId _) -> failId == testId) fails of
        [(Fail _ actualText)] ->
            (Test testId fns literalText testText expected (Just (Output actualText))):assembleFailingTests tests fails
        _ ->
            (t:assembleFailingTests tests fails)
assembleFailingTests (test:tests) fails =
    (test:assembleFailingTests tests fails)
