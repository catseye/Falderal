module Test.Falderal.Runner (run) where

--
-- Test.Falderal.Runner -- The Falderal Test Runner
-- Copyright (c)2011 Cat's Eye Technologies.  All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
--
--  1. Redistributions of source code must retain the above copyright
--     notices, this list of conditions and the following disclaimer.
--  2. Redistributions in binary form must reproduce the above copyright
--     notices, this list of conditions, and the following disclaimer in
--     the documentation and/or other materials provided with the
--     distribution.
--  3. Neither the names of the copyright holders nor the names of their
--     contributors may be used to endorse or promote products derived
--     from this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
-- FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE
-- COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
-- INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
-- BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
-- LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
-- ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.
--

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

data Block = Section String
           | Test String String Expectation
           deriving (Show, Eq, Ord)

--
-- First element is the literal text preceding the test.
-- Second element is the textual input to the test.
-- Third element is the result that we expected from the test.
-- Fourth element is the actual result of the test.
--

data Result = Failure String String Expectation Expectation
              deriving (Show, Eq, Ord)

--
-- Main entry point to test runner.
--
-- First argument is a list of filenames to harvest and run tests from.
-- Second argument is a "property list" of options in String format,
--   currently not used.
-- Third argument maps section headers to the function to be tested in
--   that section.
--

run :: [String] -> [(String, String)] -> [(String, String -> String)] -> IO ()

run [] options funMap =
    return ()
run (filename:filenames) options funMap = do
    loadAndRunTests filename funMap
    run filenames options funMap

--
-- File loading functions.
--

loadAndRunTests fileName funMap = do
    tests <- loadFile fileName
    reportTests funMap tests

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
convertLinesToBlocks ((SectionHeading text):rest) =
    ((Section text):convertLinesToBlocks rest)
convertLinesToBlocks ((LiteralText _):(SectionHeading text):rest) =
    ((Section text):convertLinesToBlocks rest)

-- Invalid sequences (such as an expected result without any preceding test
-- input) are silently ignored for now, but should be flagged as errors.

convertLinesToBlocks (_:rest) =
    convertLinesToBlocks rest
convertLinesToBlocks [] = []

--
-- The main test-running engine of Falderal:
--

runTests funMap testFun [] = do
    return []
runTests funMap testFun ((Section sectionText):rest) = do
    -- select a new testFun from the funMap
    testFun' <- return $ selectTestFun funMap sectionText
    runTests funMap testFun' rest
runTests funMap testFun ((Test literalText inputText expected):rest) = do
    actual <- runFun (testFun) inputText
    case compareTestOutcomes actual expected of
        True ->
            runTests funMap testFun rest
        False -> do
            remainder <- runTests funMap testFun rest
            return ((Failure literalText inputText expected actual):remainder)

-- This ought to be more forgiving;
-- if no fun could be found in this section, skip the tests.
selectTestFun ((text, fun):rest) sectionText
    | text == sectionText = fun
    | otherwise           = selectTestFun rest sectionText

runFun testFun inputText = do
    Exc.catch (Exc.evaluate (Output $! (testFun inputText)))
              (\exception -> return (Exception (show (exception :: Exc.SomeException))))

-- This may be improved to do pattern-matching of some kind, someday.
compareTestOutcomes actual expected =
    actual == expected

isTest (Test _ _ _) = True
isTest _ = False

reportTests funMap tests = let
        numTests = length (filter (isTest) tests)
    in do
        failures <- runTests funMap (\x -> error "No test function selected") tests
        putStrLn "--------------------------------"
        putStrLn ("Total tests: " ++ (show numTests) ++ ", failures: " ++ (show (length failures)))
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
