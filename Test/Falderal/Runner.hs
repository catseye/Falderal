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

data Expectation = Output String
                 | Exception String
                 deriving (Show, Eq, Ord)

data Block = Test String String Expectation
             deriving (Show, Eq, Ord)

data Result = Failure String String Expectation Expectation
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
    ((Test literalText testText (Output expected)):convertLinesToBlocks rest)
convertLinesToBlocks ((LiteralText literalText):(TestInput testText):(ExpectedError expected):rest) =
    ((Test literalText testText (Exception expected)):convertLinesToBlocks rest)
convertLinesToBlocks ((TestInput testText):(ExpectedResult expected):rest) =
    ((Test "(undescribed output test)" testText (Output expected)):convertLinesToBlocks rest)
convertLinesToBlocks ((TestInput testText):(ExpectedError expected):rest) =
    ((Test "(undescribed output test)" testText (Exception expected)):convertLinesToBlocks rest)

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

runTests testFun ((Test literalText inputText expected):rest) = do
    actual <- runFun (testFun) inputText
    case compareTestOutcomes actual expected of
        True ->
            runTests (testFun) rest
        False -> do
            remainder <- runTests (testFun) rest
            return ((Failure literalText inputText expected actual):remainder)

runFun testFun inputText = do
    Exc.catch (Exc.evaluate (Output $! (testFun inputText)))
              (\exception -> return (Exception (show (exception :: Exc.SomeException))))

-- This may be improved to do pattern-matching of some kind, someday.
compareTestOutcomes actual expected =
    actual == expected

reportTests testFun tests = do
    failures <- runTests testFun tests
    putStrLn "--------------------------------"
    putStrLn ("Total tests: " ++ (show (length tests)) ++ ", failures: " ++ (show (length failures)))
    putStrLn "--------------------------------\n"
    reportEachTest failures

reportEachTest [] = do
    return ()
reportEachTest ((Failure literalText testText expected actual):rest) = do
    putStrLn ("FAILED: " ++ literalText)
    putStrLn ("Input   : " ++ testText)
    putStrLn ("Expected: " ++ (show expected))
    putStrLn ("Actual  : " ++ (show actual))
    putStrLn ""
    reportEachTest rest
