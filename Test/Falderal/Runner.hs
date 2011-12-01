module Test.Falderal.Runner (runTests) where

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

import qualified Control.Exception as Exc

import Test.Falderal.Common

--
-- Test-running engine.  Takes a list of (function, block) tuples;
-- any block that is not a Test is simply ignored.
--

runTests [] = do
    return []
runTests ((testFun, Test id testType literalText inputText expected):rest) = do
    actual <- runFun (testFun) inputText
    case compareTestOutcomes actual expected of
        True ->
            runTests rest
        False -> do
            remainder <- runTests rest
            return ((Failure literalText inputText expected actual):remainder)
runTests (_:rest) = do
    runTests rest

runFun testFun inputText = do
    Exc.catch (Exc.evaluate (Output $! (testFun inputText)))
              (\exception -> return (Exception (show (exception :: Exc.SomeException))))

-- This may be improved to do pattern-matching of some kind, someday.
compareTestOutcomes actual expected =
    actual == expected
