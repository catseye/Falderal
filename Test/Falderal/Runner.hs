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

import Test.Falderal.Loader

--
-- Definitions.
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

loadAndRunTests fileName funMap = do
    blocks <- loadFile fileName
    tests <- return $ reDescribeBlocks blocks
    reportTests funMap tests

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
    reportText 8 "FAILED"   (stripLeading '\n' (stripTrailing '\n' literalText))
    putStrLn ""
    reportText 8 "Input"    testText
    reportText 8 "Expected" (show expected)
    reportText 8 "Actual"   (show actual)
    putStrLn ""
    reportEachTest rest

reportText width fieldName text =
    if
        contains text '\n'
      then do
        putStrLn (fieldName ++ ":")
        putStrLn text
      else do
        putStrLn ((pad fieldName width) ++ ": " ++ text)

contains [] _ = False
contains (x:xs) y
    | x == y    = True
    | otherwise = contains xs y

pad s n = padFrom s (n-(length s))
padFrom s n
    | n <= 0    = s
    | otherwise = padFrom (s ++ " ") (n-1)
