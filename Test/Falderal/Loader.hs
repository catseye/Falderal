module Test.Falderal.Loader (
                              loadFile,
                              loadText,
                              parseFunctionality,
                              collectFunctionalityDefinitions,
                              assignFunctionalities
                            ) where

--
-- Test.Falderal.Loader -- The Falderal Test Loader
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

--
-- Returns a pair of the lines and the blocks, allowing the caller to choose
-- which one they want to look at.
--
-- Note that the lines so returned are coalesced, and contain parsed pragmas.
--
-- Note that the blocks so returned are redescribed, but are not processed;
-- that is, the list still contains Directives and Sections, and tests are
-- not assigned functionalities.  We leave this up to the caller.
-- The functions to do this should maybe be in some module other than this one.
--

loadText text =
    let
        ls = resolvePragmas $ transformLines $ lines text
        bs = reDescribeBlocks $ convertLinesToBlocks ls
    in
        (ls, bs)

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
-- Convert (coalesced) lines to blocks.  We expect the pragmas to have
-- been parsed, and retain directives from them in the blocks.
--

convertLinesToBlocks :: [Line] -> [Block]

convertLinesToBlocks ((LiteralText literalText):(TestInput testText):(ExpectedResult expected):rest) =
    (Test 0 [] literalText testText (Output expected) Nothing):convertLinesToBlocks rest
convertLinesToBlocks ((LiteralText literalText):(TestInput testText):(ExpectedError expected):rest) =
    (Test 0 [] literalText testText (Exception expected) Nothing):convertLinesToBlocks rest
convertLinesToBlocks ((TestInput testText):(ExpectedResult expected):rest) =
    (Test 0 [] "(undescribed output test)" testText (Output expected) Nothing):convertLinesToBlocks rest
convertLinesToBlocks ((TestInput testText):(ExpectedError expected):rest) =
    (Test 0 [] "(undescribed error test)" testText (Exception expected) Nothing):convertLinesToBlocks rest
convertLinesToBlocks ((SectionHeading text):rest) =
    (Section text):convertLinesToBlocks rest
convertLinesToBlocks ((Pragma _ (Just dir)):rest) =
    (Directive dir):convertLinesToBlocks rest
convertLinesToBlocks ((LiteralText _):(SectionHeading text):rest) =
    ((Section text):convertLinesToBlocks rest)
convertLinesToBlocks (_:rest) =
    convertLinesToBlocks rest
convertLinesToBlocks [] = []

--
-- Give each test block a functionality, expanding named functionalities to
-- concrete functionalities as needed.  Strip all Directives and Sections
-- from the list of blocks.
--

assignFunctionalities :: [Block] -> [Functionality] -> [(String, Functionality)] -> [Block]

assignFunctionalities ((Test 0 [] literalText testText expectation Nothing):rest) fns fnMap =
    (Test 0 fns literalText testText expectation Nothing):assignFunctionalities rest fns fnMap

assignFunctionalities ((Directive (TestsFor (NamedFunctionality name))):rest) fns fnMap =
    case map (snd) $ filter (\(s,fn) -> s == name) fnMap of
        []   -> error ("Can't find " ++ name ++ " in " ++ (show fnMap))
        fns' -> assignFunctionalities rest fns' fnMap

assignFunctionalities ((Directive (TestsFor fn)):rest) fns fnMap =
    assignFunctionalities rest [fn] fnMap

assignFunctionalities (_:rest) fns fnMap =
    assignFunctionalities rest fns fnMap

assignFunctionalities [] _ _ = []

--
-- Collect Functionality-definition pragmas.
--

collectFunctionalityDefinitions ((Pragma _ (Just (FunctionalityDefinition name functionality))):rest) =
    ((name, functionality):collectFunctionalityDefinitions rest)
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
reDescribeBlocks' (block@(Test id fn literalText inp exp result):rest) desc n
    | allWhitespace literalText = (Test id fn numberedDesc inp exp result):(reDescribeBlocks' rest desc (n+1))
    | otherwise                 = (block):(reDescribeBlocks' rest literalText 2)
    where numberedDesc = "(#" ++ (show n) ++ ") " ++ (stripLeading '\n' desc)
reDescribeBlocks' (block:rest) desc n =
    block:(reDescribeBlocks' rest "" 2)

--
-- Parse a pragma.
--

possiblePragmas = [
                    (["Tests", "for"], \rest -> TestsFor $ parseFunctionality rest),
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

parseFunctionality text = tryFunctionalities functionalities text

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
                    functionality = parseFunctionality funky
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
