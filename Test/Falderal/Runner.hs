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
          deriving (Show, Eq, Ord)

data Block = OutputTest String String String
           | ErrorTest String String String
          deriving (Show, Eq, Ord)

data Result = Success String
            | Failed String String String
            deriving (Show, Eq, Ord)

--
-- File loading functions.
--

-- A hack for now.
run fileName [(_, testFun)] =
    loadTests fileName testFun

loadTests fileName testFun = do
    tests <- loadFile fileName
    runTests testFun tests 0 0

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
    reduceLines (map classifyLine lines) (LiteralText "0") []
    where
        classifyLine line
            | prefix == "| " = TestInput suffix
            | prefix == "= " = ExpectedResult suffix
            | prefix == "? " = ExpectedError suffix
            | otherwise      = LiteralText line
            where
                prefix = take 2 line
                suffix = drop 2 line

--
-- Coalesce neigbouring lines.  For each line, if it is classified the
-- same way as the line previously examined, combine them.
--

reduceLines [] last acc = reverse (last:acc)
reduceLines ((TestInput more):lines) (TestInput last) acc =
    reduceLines lines (TestInput (last ++ "\n" ++ more)) acc
reduceLines ((ExpectedResult more):lines) (ExpectedResult last) acc =
    reduceLines lines (ExpectedResult (last ++ "\n" ++ more)) acc
reduceLines ((ExpectedError more):lines) (ExpectedError last) acc =
    reduceLines lines (ExpectedResult (last ++ "\n" ++ more)) acc
reduceLines ((LiteralText more):lines) (LiteralText last) acc =
    reduceLines lines (LiteralText (last ++ "\n" ++ more)) acc
reduceLines (line:lines) last acc =
    reduceLines lines line (last:acc)

convertLinesToBlocks ((LiteralText literalText):(TestInput testText):(ExpectedResult expected):rest) =
    ((OutputTest literalText testText expected):convertLinesToBlocks rest)
convertLinesToBlocks ((LiteralText literalText):(TestInput testText):(ExpectedError expected):rest) =
    ((ErrorTest literalText testText expected):convertLinesToBlocks rest)
convertLinesToBlocks ((TestInput testText):(ExpectedResult expected):rest) =
    ((OutputTest "(undescribed output test)" testText expected):convertLinesToBlocks rest)
convertLinesToBlocks ((TestInput testText):(ExpectedError expected):rest) =
    ((ErrorTest "(undescribed output test)" testText expected):convertLinesToBlocks rest)

-- Invalid sequences (such as an expected result without any preceding test
-- input) are silently ignored for now, but should be flagged as errors.

convertLinesToBlocks (_:rest) =
    convertLinesToBlocks rest
convertLinesToBlocks [] = []

--
-- The main test-running engine of Falderal:
--

runTests testFun [] failures total = do
    putStrLn ("Total tests: " ++ (show total) ++ ", failures: " ++ (show failures))
    return []

runTests testFun ((OutputTest literalText testText expected):rest) failures total = do
    r <- Exc.try (do return (testFun testText))
    case r of
        Right result -> do
            if result == expected then do
                runTests (testFun) rest failures (total + 1)
              else do
                markFailure (testFun) testText expected result rest failures total
        Left exception ->
            let
                result = "*** Exception: " ++ (show (exception :: Exc.SomeException))
            in do
                markFailure (testFun) testText expected result rest failures total

runTests testFun ((ErrorTest literalText testText expected):rest) failures total = do
    r <- Exc.try (do return (testFun testText))
    case r of
        Right result -> do
            markFailure (testFun) testText expected result rest failures total
        Left exception ->
            let
                result = (show (exception :: Exc.SomeException))
            in
                if
                    result == expected
                then do
                    runTests (testFun) rest failures (total + 1)
                else do
                    markFailure (testFun) testText expected result rest failures total

markFailure testFun testText expected result rest failures total = do
    putStrLn (show (Failed testText expected result))
    remainder <- runTests (testFun) rest (failures + 1) (total + 1)
    return ((Failed testText expected result):remainder)
