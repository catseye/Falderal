module Test.Falderal.Runner (runTests) where

import System
import System.IO

import Test.Falderal.Common
import Test.Falderal.Formatter (format) -- boo?

--
-- Test-running engine.  This has just completely changed
-- from what it used to be!
--

data Result = Result Int Expectation
    deriving (Ord, Eq, Show)

cleanRun True cmd = do
    system cmd
    return ()
cleanRun False cmd = do
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
    exitCode <- system (command ++ " >results.txt")
    contents <- readFile "results.txt"
    results <- return $ collectResults $ lines $ contents
    cleanRun (not messy) ("rm -f " ++ filename)
    cleanRun (not messy) ("rm -f results.txt")
    return $ decorateTestsWithResults blocks results

collectResults [] =
    []
collectResults (kindStr:idStr:numLinesStr:rest) =
    let
        id = parseNatNumStr idStr 0
        numLines = parseNatNumStr numLinesStr 0
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
        id = parseNatNumStr idStr 0
    in
        ((Result id (Output "")):collectResults rest)

decorateTestsWithResults [] fails = []
decorateTestsWithResults (t@(Test testId fns literalText testText expected _):tests) fails =
    case filter (\(Result resultId _) -> resultId == testId) fails of
        [(Result _ result)] ->
            (Test testId fns literalText testText expected (Just result)):decorateTestsWithResults tests fails
        _ ->
            (t:decorateTestsWithResults tests fails)
decorateTestsWithResults (test:tests) fails =
    (test:decorateTestsWithResults tests fails)
