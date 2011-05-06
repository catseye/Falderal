module Test.Falderal.Runner where

-- Nothing much here yet but some dreams, really.  Based on some code
-- I extracted from Rho that I want to use for writing test suites for some
-- of the other languages I've implemented in Haskell.

import System
import qualified Control.Exception as Exc

--
-- Definitions.
--

data Line = TestInput String
          | ExpectedResult String
          | ExpectedError String
          | LiteralText String
          | SectionHeading String
          deriving (Show, Eq, Ord)

data Block = OutputTest String String String
           | ErrorTest String String String
          deriving (Show, Eq, Ord)

data Result = Failure String String String String
            deriving (Show, Eq, Ord)

--
-- File loading functions.
--

-- A hack for now.
run fileName [(_, testFun)] =
    loadAndRunTests fileName testFun

loadAndRunTests fileName testFun = do
    tests <- loadFile fileName
    reportTests testFun tests

loadFile fileName = do
    testText <- readFile fileName
    lines <- return $ transformLines $ lines testText
    blocks <- return $ convertLinesToBlocks $ lines
    return blocks

loadLines fileName = do
    testText <- readFile fileName
    lines <- return $ transformLines $ lines testText
    return lines

transformLines lines =
    let
        lines' = map classifyLine lines
        lines'' = findSectionHeadings lines' (LiteralText "0")
    in
        coalesceLines lines'' (LiteralText "0")

classifyLine line
    | prefix == "| " = TestInput suffix
    | prefix == "= " = ExpectedResult suffix
    | prefix == "? " = ExpectedError suffix
    | otherwise      = LiteralText line
    where
        prefix = take 2 line
        suffix = drop 2 line

findSectionHeadings [] last =
    [last]
findSectionHeadings ((line@(LiteralText suspectedUnderline)):lines) last@(LiteralText suspectedHeading) =
    if
        ((discoverRepeatedCharacter suspectedUnderline) == Just '-') &&
        ((length suspectedUnderline) == (length suspectedHeading))
    then
        findSectionHeadings lines (SectionHeading suspectedHeading)
    else
        (last:findSectionHeadings lines line)
findSectionHeadings (line:lines) last =
    (last:findSectionHeadings lines line)

discoverRepeatedCharacter [] =
    Nothing
discoverRepeatedCharacter (first:rest) =
    confirmRepeatedCharacter first rest

confirmRepeatedCharacter char [] =
    Just char
confirmRepeatedCharacter char (next:rest)
    | char == next = confirmRepeatedCharacter char rest
    | otherwise    = Nothing

--
-- Coalesce neigbouring lines.  For each line, if it is classified the
-- same way as the line previously examined, combine them.
--

coalesceLines [] last =
    [last]
coalesceLines ((TestInput more):lines) (TestInput last) =
    coalesceLines lines (TestInput (last ++ "\n" ++ more))
coalesceLines ((ExpectedResult more):lines) (ExpectedResult last) =
    coalesceLines lines (ExpectedResult (last ++ "\n" ++ more))
coalesceLines ((ExpectedError more):lines) (ExpectedError last) =
    coalesceLines lines (ExpectedResult (last ++ "\n" ++ more))
coalesceLines ((LiteralText more):lines) (LiteralText last) =
    coalesceLines lines (LiteralText (last ++ "\n" ++ more))
coalesceLines (line:lines) last =
    (last:coalesceLines lines line)

convertLinesToBlocks ((LiteralText literalText):(TestInput testText):(ExpectedResult expected):rest) =
    ((OutputTest literalText testText expected):convertLinesToBlocks rest)
convertLinesToBlocks ((LiteralText literalText):(TestInput testText):(ExpectedError expected):rest) =
    ((ErrorTest literalText testText expected):convertLinesToBlocks rest)
convertLinesToBlocks ((TestInput testText):(ExpectedResult expected):rest) =
    ((OutputTest "(undescribed output test)" testText expected):convertLinesToBlocks rest)
convertLinesToBlocks ((TestInput testText):(ExpectedError expected):rest) =
    ((ErrorTest "(undescribed output test)" testText expected):convertLinesToBlocks rest)

-- Invalid sequences (such as an expected result without any preceding test
-- input) are silently ignored for now, but should be flagged as errors.

convertLinesToBlocks (_:rest) =
    convertLinesToBlocks rest
convertLinesToBlocks [] = []

--
-- The main test-running engine of Falderal:
--

runTests testFun [] = do
    return []

runTests testFun ((OutputTest literalText testText expected):rest) = do
    r <- Exc.try (do return (testFun testText))
    case r of
        Right result ->
            if
                result == expected
            then do
                runTests (testFun) rest
            else do
                markFailure (testFun) literalText testText expected result rest
        Left exception ->
            let
                result = "*** Exception: " ++ (show (exception :: Exc.SomeException))
            in do
                markFailure (testFun) literalText testText expected result rest

runTests testFun ((ErrorTest literalText testText expected):rest) = do
    r <- Exc.try (do return (testFun testText))
    case r of
        Right result -> do
            markFailure (testFun) literalText testText expected result rest
        Left exception ->
            let
                result = (show (exception :: Exc.SomeException))
            in
                if
                    result == expected
                then do
                    runTests (testFun) rest
                else do
                    markFailure (testFun) literalText testText expected result rest

markFailure testFun literalText testText expected result rest = do
    remainder <- runTests (testFun) rest
    return ((Failure literalText testText expected result):remainder)

reportTests testFun tests = do
    failures <- runTests testFun tests
    putStrLn "--------------------------------"
    putStrLn ("Total tests: " ++ (show (length tests)) ++ ", failures: " ++ (show (length failures)))
    putStrLn "--------------------------------\n"
    reportEachTest failures

reportEachTest [] = do
    return ()
reportEachTest ((Failure literalText testText expected result):rest) = do
    putStrLn ("FAILED: " ++ literalText)
    putStrLn ("Input   : " ++ testText)
    putStrLn ("Expected: " ++ expected)
    putStrLn ("Actual  : " ++ result)
    putStrLn ""
    reportEachTest rest
