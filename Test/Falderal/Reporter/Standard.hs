module Test.Falderal.Reporter.Standard (report) where

--
-- Test.Falderal.Reporter.Standard -- Std. report for Falderal test results
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

import Test.Falderal.Common

report blocks = let
        numTests = length (filter (isTest) blocks)
        failures = filter (isFailingTest) blocks
    in do
        putStrLn "--------------------------------"
        putStrLn ("Total tests: " ++ (show numTests) ++ ", failures: " ++ (show (length failures)))
        putStrLn "--------------------------------\n"
        reportEachTest failures

reportEachTest [] = do
    return ()
reportEachTest (Test id fns literalText testText expected (Just actual):rest) = do
    reportText 8 "FAILED"   (stripLeading '\n' (stripTrailing '\n' literalText))
    putStrLn ""
    -- reportText 8 "ID"       (show id)
    reportText 8 "Input"    testText
    reportText 8 "Expected" (show expected)
    reportText 8 "Actual"   (show actual)
    putStrLn ""
    reportEachTest rest

reportText width fieldName text =
    if
        elem '\n' text
      then do
        putStrLn (fieldName ++ ":")
        putStrLn text
      else do
        putStrLn ((pad fieldName width) ++ ": " ++ text)

isTest (Test _ _ _ _ _ _) = True
isTest _ = False

isFailingTest (Test _ _ _ _ _ (Just _)) = True
isFailingTest _ = False
