module Test.Falderal.Runner (runTests) where

import System
import System.IO

import Test.Falderal.Common
import Test.Falderal.Formatter (format) -- boo?

import qualified Test.Falderal.Runner.Shell as Shell

--
-- Test-running engine.  This has just completely changed
-- from what it used to be!
--

cleanRun True cmd = do
    system cmd
    return ()
cleanRun False cmd = do
    return ()

runTests :: [Block] -> String -> String -> String -> Bool -> IO [Block]

--
-- Special case for shell tests
--

runTests blocks filename "shell" command messy = do
    (resultsFilename, handle) <- openTempFile "." "results.txt"
    hClose handle
    hSetNewlineMode handle noNewlineTranslation
    Shell.run blocks resultsFilename messy
    processResultsFile blocks filename resultsFilename messy

-- TODO: what to do with exitCode?

runTests [] _ _ _ _ = do
    return []
runTests blocks filename formatName command messy = do
    outputFileHandle <- openFile filename WriteMode
    hSetNewlineMode outputFileHandle noNewlineTranslation
    text <- return $ format formatName [] blocks
    hPutStr outputFileHandle text
    hClose outputFileHandle
    (resultsFilename, h) <- openTempFile "." "results.txt"
    hClose h
    exitCode <- system (command ++ " >" ++ resultsFilename)
    processResultsFile blocks filename resultsFilename messy

processResultsFile blocks filename resultsFilename messy = do
    contents <- readFile resultsFilename
    let results = collectResults $ lines $ contents
    cleanRun (not messy) ("rm -f " ++ filename)
    cleanRun (not messy) ("rm -f " ++ resultsFilename)
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
