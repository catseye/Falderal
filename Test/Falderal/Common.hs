module Test.Falderal.Common where

--
-- Test.Falderal.Common -- Common data def'n and functions for Falderal
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

import qualified Char

--
-- Definitions for the structure of a test suite in Falderal format.
--

--
-- Before processing...
--

data Line = TestInput String
          | ExpectedResult String
          | ExpectedError String
          | LiteralText String
          | QuotedCode String
          | SectionHeading String
          | Pragma String (Maybe Directive)
          | Placeholder
          deriving (Show, Eq, Ord)

--
-- ...and in the middle of processing...
--

data Directive = TestsFor Functionality
               | FunctionalityDefinition String Functionality
               | Encoding String
               deriving (Show, Eq, Ord)

data Functionality = HaskellTest String String -- module name, function name
                   | ShellTest String -- command
                   | NamedFunctionality String
                   deriving (Show, Eq, Ord)

--
-- ...and after.
--

data Expectation = Output String
                 | Exception String
                 deriving (Show, Eq, Ord)

data Block = Section String
           | Test Int [Functionality] String String Expectation
           deriving (Show, Eq, Ord)

--
-- Data type for test results.
--
-- First element is the literal text preceding the test.
-- Second element is the textual input to the test.
-- Third element is the result that we expected from the test.
-- Fourth element is the actual result of the test.
--

data Result = Failure String String Expectation Expectation
              deriving (Show, Eq, Ord)

--
-- Common functions.
--

-- TODO: How many of these can be replaced by standard Haskell functions?

discoverRepeatedCharacter [] =
    Nothing
discoverRepeatedCharacter (first:rest)
    | all (\x -> x == first) rest = Just first
    | otherwise                   = Nothing

allWhitespace = all Char.isSpace

stripLeading y = dropWhile $ \x -> x == y

stripTrailing y str = reverse (stripLeading y (reverse str))

stripLeadingWhitespace = dropWhile $ Char.isSpace

--
-- A version of `lines` that always considers the input "" to
-- represent a single, blank line.
--

allLines x =
    case (lines x) of
        []    -> [""]
        other -> other

prefixEachLine prefix text =
    foldl (++) "" (map (\x -> prefix ++ x ++ "\n") (allLines text))

formatLines formatter lines = foldl (++) "" (map (formatter) lines)

pad s n = padFrom s (n-(length s))
padFrom s n
    | n <= 0    = s
    | otherwise = padFrom (s ++ " ") (n-1)
