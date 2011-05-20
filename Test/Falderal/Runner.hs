module Test.Falderal.Runner (run) where

--
-- The Falderal Test Runner
--
-- This was originally based on some code for running "literate" test suites
-- on string functions that I extracted from a language project I was working
-- on called Rho, that I want to use for other languages I've implemented in
-- Haskell.  Rapidly progressing towards something of more general utility.
--

import System
import qualified Control.Exception as Exc

--
-- collecting TODOs here because I don't have access to the issue tracker atm
--
-- TODO: in convertLinesToBlocks, Invalid sequences (such as an expected
-- result without any preceding test input) should be flagged as errors
-- instead of being silently ignored
--
-- TODO: selectTestFun ought to be more forgiving: if no fun could be found
-- in this section, skip the tests.  This necessitates a "skip" result.
--

--
-- Definitions.
--

data Line = TestInput String
          | ExpectedResult String
          | ExpectedError String
          | LiteralText String
          | QuotedCode String
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
    blocks <- return $ reDescribeBlocks $ convertLinesToBlocks $ lines
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
    coalesceLines lines (ExpectedResult (last ++ "\n" ++ more))
coalesceLines ((LiteralText more):lines) (LiteralText last) =
    coalesceLines lines (LiteralText (last ++ "\n" ++ more))
coalesceLines ((QuotedCode more):lines) (QuotedCode last) =
    coalesceLines lines (QuotedCode (last ++ "\n" ++ more))
coalesceLines (line:lines) last =
    (last:coalesceLines lines line)

--
-- Convert lines to blocks.
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

allWhitespace [] = True
allWhitespace (' ':rest) = allWhitespace rest
allWhitespace ('\n':rest) = allWhitespace rest
allWhitespace ('\t':rest) = allWhitespace rest
allWhitespace (_:rest) = False

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
    reportText 8 "FAILED"   (stripLeading '\n' literalText)
    reportText 8 "Input"    testText
    reportText 8 "Expected" (show expected)
    reportText 8 "Actual"   (show actual)
    putStrLn ""
    reportEachTest rest

stripLeading y [] = []
stripLeading y all@(x:xs)
    | x == y    = stripLeading y xs
    | otherwise = all

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
