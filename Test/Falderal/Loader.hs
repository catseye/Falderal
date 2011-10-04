module Test.Falderal.Loader where

-- TODO: export a more restricted interface

--
-- Test.Falderal.Loader -- The Falderal Test Loader
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

--
-- Definitions for the structure of a test suite in Falderal format.
--

data Line = TestInput String
          | ExpectedResult String
          | ExpectedError String
          | LiteralText String
          | QuotedCode String
          | SectionHeading String
          | Placeholder
          deriving (Show, Eq, Ord)

-- TODO: move these datatypes to a common module?

data Expectation = Output String
                 | Exception String
                 deriving (Show, Eq, Ord)

data Block = Section String
           | Test String String Expectation
           deriving (Show, Eq, Ord)

--
-- File loading functions.
--

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
        lines'' = findSectionHeadings lines' Placeholder
        lines''' = coalesceLines lines'' Placeholder
    in
        stripPlaceholders lines'''

stripPlaceholders [] = []
stripPlaceholders (Placeholder:rest) = stripPlaceholders rest
stripPlaceholders (other:rest) = other:(stripPlaceholders rest)

classifyLine line
    | prefix == "| " = TestInput suffix
    | prefix == "= " = ExpectedResult suffix
    | prefix == "? " = ExpectedError suffix
    | prefix == "> " = QuotedCode suffix
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
    coalesceLines lines (ExpectedError (last ++ "\n" ++ more))
coalesceLines ((LiteralText more):lines) (LiteralText last) =
    coalesceLines lines (LiteralText (last ++ "\n" ++ more))
coalesceLines ((QuotedCode more):lines) (QuotedCode last) =
    coalesceLines lines (QuotedCode (last ++ "\n" ++ more))
coalesceLines (line:lines) (LiteralText last) =
    ((LiteralText (last ++ "\n")):coalesceLines lines line)
coalesceLines (line:lines) last =
    (last:coalesceLines lines line)

--
-- Convert (coalesced) lines to blocks.
--

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

convertLinesToBlocks (_:rest) =
    convertLinesToBlocks rest
convertLinesToBlocks [] = []

--
-- Give blocks that don't have a description, the description of the previous
-- block that did have a description.  Note that when we encounter a new
-- section, we do not remember the previous description, as it will surely
-- be irrelevant now.
--

-- TODO: move this to a common module where Block is defined?

reDescribeBlocks blocks = reDescribeBlocks' blocks "" 2

reDescribeBlocks' [] desc n =
    []
reDescribeBlocks' (block@(Test literalText inp exp):rest) desc n
    | allWhitespace literalText = (Test numberedDesc inp exp):(reDescribeBlocks' rest desc (n+1))
    | otherwise                 = (block):(reDescribeBlocks' rest literalText 2)
    where numberedDesc = "(#" ++ (show n) ++ ") " ++ (stripLeading '\n' desc)
reDescribeBlocks' (block:rest) desc n =
    block:(reDescribeBlocks' rest "" 2)

--
-- This could use Char.isSpace
--

-- TODO: move these to a common utility module?
-- TODO: can these be replaced by standard Haskell functions?

allWhitespace [] = True
allWhitespace (' ':rest) = allWhitespace rest
allWhitespace ('\n':rest) = allWhitespace rest
allWhitespace ('\t':rest) = allWhitespace rest
allWhitespace (_:rest) = False

stripLeading y [] = []
stripLeading y all@(x:xs)
    | x == y    = stripLeading y xs
    | otherwise = all

stripTrailing y str = reverse (stripLeading y (reverse str))

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
