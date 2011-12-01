module Test.Falderal.Loader (loadFile, loadFiles, loadText) where

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

import Data.List
import System

import Test.Falderal.Common

--
-- File loading functions.
--

loadFile fileName = do
    testText <- readFile fileName
    (ls, bs) <- return $ loadText testText
    return (ls, bs)

loadFiles [] = do
    return ([], [])
loadFiles (fileName:rest) = do
    (ls, bs) <- loadFile fileName
    (restLs, restBs) <- loadFiles rest
    return (ls ++ restLs, bs ++ restBs)

--
-- Returns both the (coaslesced) lines and the (redescribed) blocks,
-- allowing the caller to choose which one they want to look at.
--

loadText text =
    let
        ls = transformLines $ lines text
        ls' = resolvePragmas ls
        fds = collectFunctionalityDefinitions ls'
        bs = convertLinesToBlocks ls' [] fds
        bs' = reDescribeBlocks bs
    in
        (ls', bs)

transformLines ls =
    let
        ls' = map classifyLine ls
        ls'' = findSectionHeadings ls' Placeholder
        ls''' = coalesceLines ls'' Placeholder
    in
        stripPlaceholders ls'''

stripPlaceholders [] = []
stripPlaceholders (Placeholder:rest) = stripPlaceholders rest
stripPlaceholders (other:rest) = other:(stripPlaceholders rest)

classifyLine line
    | prefix == "| " = TestInput suffix
    | prefix == "= " = ExpectedResult suffix
    | prefix == "? " = ExpectedError suffix
    | prefix == "> " = QuotedCode suffix
    | prefix == "->" = Pragma suffix Nothing
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
coalesceLines ((Pragma more Nothing):lines) (Pragma last Nothing) =
    coalesceLines lines (Pragma (last ++ "\n" ++ more) Nothing)
coalesceLines (line:lines) (LiteralText last) =
    ((LiteralText (last ++ "\n")):coalesceLines lines line)
coalesceLines (line:lines) last =
    (last:coalesceLines lines line)

resolvePragmas ((Pragma text Nothing):rest) =
    ((Pragma text $ Just $ parsePragma text):resolvePragmas rest)
resolvePragmas (other:rest) = (other:resolvePragmas rest)
resolvePragmas [] = []

--
-- Convert (coalesced) lines to blocks.
--

convertLinesToBlocks :: [Line] -> [Functionality] -> [(String, Functionality)] -> [Block]

convertLinesToBlocks ((LiteralText literalText):(TestInput testText):(ExpectedResult expected):rest) fns fnMap =
    ((Test fns literalText testText (Output expected)):(convertLinesToBlocks rest fns fnMap))
convertLinesToBlocks ((LiteralText literalText):(TestInput testText):(ExpectedError expected):rest) fns fnMap =
    ((Test fns literalText testText (Exception expected)):(convertLinesToBlocks rest fns fnMap))
convertLinesToBlocks ((TestInput testText):(ExpectedResult expected):rest) fns fnMap =
    ((Test fns "(undescribed output test)" testText (Output expected)):(convertLinesToBlocks rest fns fnMap))
convertLinesToBlocks ((TestInput testText):(ExpectedError expected):rest) fns fnMap =
    ((Test fns "(undescribed output test)" testText (Exception expected)):(convertLinesToBlocks rest fns fnMap))
convertLinesToBlocks ((SectionHeading text):rest) fn fnMap =
    ((Section text):(convertLinesToBlocks rest fn fnMap))
convertLinesToBlocks ((Pragma _ (Just (TestsFor (NamedFunctionality name)))):rest) fns fnMap =
    case map (snd) $ filter (\(s,fn) -> s == name) fnMap of
        []   -> error ("Can't find " ++ name ++ " in " ++ (show fnMap))
        fns' -> convertLinesToBlocks rest fns' fnMap
convertLinesToBlocks ((Pragma _ (Just (TestsFor fn))):rest) fns fnMap =
    convertLinesToBlocks rest [fn] fnMap
convertLinesToBlocks ((Pragma _ Nothing):rest) fns fnMap =
    error $ "should have resolved all pragmas to directives by now"
convertLinesToBlocks ((Pragma _ _):rest) fns fnMap =
    convertLinesToBlocks rest fns fnMap
convertLinesToBlocks ((LiteralText _):(SectionHeading text):rest) fns fnMap =
    ((Section text):(convertLinesToBlocks rest fns fnMap))

convertLinesToBlocks (_:rest) fns fnMap =
    convertLinesToBlocks rest fns fnMap
convertLinesToBlocks [] _ _ = []

collectFunctionalityDefinitions ((Pragma _ (Just (FunctionalityDefinition name functionality))):rest) =
    ((name, functionality):collectFunctionalityDefinitions rest)
collectFunctionalityDefinitions ((Pragma _ Nothing):rest) =
    error $ "should have resolved all pragmas to directives by now"
collectFunctionalityDefinitions (_:rest) =
    collectFunctionalityDefinitions rest
collectFunctionalityDefinitions [] =
    []

--
-- Give blocks that don't have a description, the description of the previous
-- block that did have a description.  Note that when we encounter a new
-- section, we do not remember the previous description, as it will surely
-- be irrelevant now.
--

reDescribeBlocks blocks = reDescribeBlocks' blocks "" 2

reDescribeBlocks' [] desc n =
    []
reDescribeBlocks' (block@(Test fn literalText inp exp):rest) desc n
    | allWhitespace literalText = (Test fn numberedDesc inp exp):(reDescribeBlocks' rest desc (n+1))
    | otherwise                 = (block):(reDescribeBlocks' rest literalText 2)
    where numberedDesc = "(#" ++ (show n) ++ ") " ++ (stripLeading '\n' desc)
reDescribeBlocks' (block:rest) desc n =
    block:(reDescribeBlocks' rest "" 2)

--
-- Parse a pragma.
--

possiblePragmas = [
                    (["Tests", "for"], \rest -> TestsFor $ tryFunctionalities functionalities rest),
                    (["Functionality"], \rest -> parseFuncDefn rest),
                    (["encoding:"], \rest -> Encoding rest)
                  ]

parsePragma text =
    parsePossiblePragmas text possiblePragmas

parsePossiblePragmas :: String -> [([String], String -> Directive)] -> Directive
parsePossiblePragmas text [] =
    error $ "bad pragma: " ++ text
parsePossiblePragmas text ((words,f):ps) =
    case consumeWords words text of
        Just rest -> f rest
        Nothing -> parsePossiblePragmas text ps


functionalities = [
                    parseHaskellFunctionality,
                    parseShellFunctionality,
                    parseNamedFunctionality
                  ]

tryFunctionalities [] text =
    error $ "bad functionality: " ++ text
tryFunctionalities (func:rest) text =
    case func text of
        Just x  -> x
        Nothing -> tryFunctionalities rest text

parseHaskellFunctionality text =
    case consumeWords ["Haskell", "function"] text of
        Just specifier ->
            let
                (moduleName, functionName) = parseSpecifier specifier
            in
                Just $ HaskellTest moduleName functionName
        Nothing ->
            Nothing

parseShellFunctionality text =
    case consumeWords ["shell", "command"] text of
        Just specifier ->
            let
                (command, _) = parseQuotedString specifier
            in
                Just $ ShellTest command
        Nothing ->
            Nothing

parseNamedFunctionality text =
    case consumeWords ["functionality"] text of
        Just specifier ->
            let
                (name, _) = parseQuotedString specifier
            in
                Just $ NamedFunctionality name
        Nothing ->
            Nothing

parseSpecifier specifier =
    let
        (m, f) = break (\y -> y == ':') specifier
    in
        (m, stripLeading ':' f)

parseFuncDefn text =
    let
        (name, rest) = parseQuotedString text
    in
        case consumeWords ["is", "implemented", "by"] rest of
            Just funky ->
                let
                    functionality = tryFunctionalities functionalities funky
                in
                    FunctionalityDefinition name functionality
            Nothing ->
                error $ "bad functionality definition: " ++ text
        

parseQuotedString ('"':rest) =
    parseQuotedString' rest
parseQuotedString str =
    error $ "bad quoted string: " ++ str

parseQuotedString' ('"':rest) =
    ("", rest)
parseQuotedString' (char:rest) =
    let
        (next, remainder) = parseQuotedString' rest
    in
        (char:next, remainder)

consumeWords [] text =
    Just $ stripLeadingWhitespace text
consumeWords (word:rest) text =
    case stripPrefix word $ stripLeadingWhitespace text of
        Just text' ->
            consumeWords rest text'
        Nothing ->
            Nothing
