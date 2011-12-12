module Test.Falderal.Runner (runTests) where

import Char (isDigit, ord)

import System.IO
import System.Process

import Test.Falderal.Common
import Test.Falderal.Formatter (format) -- boo?

--
-- Test-running engine.  This has just completely changed
-- from what it used to be!
--

data Result = Result Int Expectation
    deriving (Ord, Eq, Show)

cleanRun False cmd = do
    system cmd
    return ()
cleanRun True cmd = do
    return ()

runTests :: [Block] -> String -> String -> String -> Bool -> IO [Block]

-- TODO: what to do with exitCode?

runTests [] _ _ _ _ = do
    return []
runTests blocks filename formatName command messy = do
    outputFileHandle <- openFile filename WriteMode
    text <- return $ format formatName [] blocks
    hPutStr outputFileHandle text
    hClose outputFileHandle
    (lines, exitCode) <- captureLines command
    results <- return $ collectResults lines
    cleanRun messy ("rm -f " ++ filename)
    return $ decorateTestsWithResults blocks results

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

collectResults [] =
    []
collectResults (kindStr:idStr:numLinesStr:rest) =
    let
        id = parseNumStr idStr 0
        numLines = parseNumStr numLinesStr 0
        failLines = take numLines rest
        rest' = drop numLines rest
        resText = (join "\n" failLines)
        res = case kindStr of
            "output" -> Output resText
            "exception" -> Exception resText
    in
        ((Result id res):collectResults rest')
collectResults (idStr:rest) =
    let
        id = parseNumStr idStr 0
    in
        ((Result id (Output "")):collectResults rest)

parseNumStr [] acc = acc
parseNumStr (x:xs) acc
    | isDigit x = parseNumStr xs (acc * 10 + ((ord x) - (ord '0')))
    | otherwise = acc

decorateTestsWithResults [] fails = []
decorateTestsWithResults (t@(Test testId fns literalText testText expected _):tests) fails =
    case filter (\(Result resultId _) -> resultId == testId) fails of
        [(Result _ result)] ->
            (Test testId fns literalText testText expected (Just result)):decorateTestsWithResults tests fails
        _ ->
            (t:decorateTestsWithResults tests fails)
decorateTestsWithResults (test:tests) fails =
    (test:decorateTestsWithResults tests fails)
