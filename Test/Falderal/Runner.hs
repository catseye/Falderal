module Test.Falderal.Runner where

-- Nothing much here yet but some dreams, really.  Based on some code
-- I extracted from Rho that I want to use for writing test suites for some
-- of the other languages I've implemented in Haskell.

import System
import qualified Control.Exception as Exc

--
-- Definitions.
--

data Block = Test String
           | ExpectedResult String
           | ExpectedError String
           | Comment String
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
    testText <- readFile fileName
    tests <- return $ transformLines $ lines testText
    runTests testFun tests 0 0

transformLines lines =
    let
        lines' = map transformLine lines
        lines'' = reduceLines lines' (Comment "0") []
        lines''' = stripLines lines''
    in
        lines'''

transformLine line
    | prefix == "| " = Test suffix
    | prefix == "= " = ExpectedResult suffix
    | prefix == "? " = ExpectedError suffix
    | otherwise = Comment line
    where
        prefix = take 2 line
        suffix = drop 2 line

reduceLines [] last acc = reverse acc
reduceLines ((Test more):lines) (Test last) acc =
    reduceLines lines (Test (last ++ "\n" ++ more)) acc
reduceLines ((ExpectedResult more):lines) (ExpectedResult last) acc =
    reduceLines lines (ExpectedResult (last ++ "\n" ++ more)) acc
reduceLines ((ExpectedError more):lines) (ExpectedError last) acc =
    reduceLines lines (ExpectedResult (last ++ "\n" ++ more)) acc
reduceLines ((Comment _):lines) (Comment last) acc =
    reduceLines lines (Comment last) acc
reduceLines (line:lines) last acc =
    reduceLines lines line (last:acc)

stripLines [] = []
stripLines ((Comment _):lines) = stripLines lines
stripLines (line:lines) = (line:(stripLines lines))

--
-- The main test-running engine of Falderal:
--

runTests testFun [] failures total = do
    putStrLn ("Total tests: " ++ (show total) ++ ", failures: " ++ (show failures))
    return []

runTests testFun ((Test testText):((ExpectedResult expected):rest)) failures total = do
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

runTests testFun ((Test testText):((ExpectedError expected):rest)) failures total = do
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

runTests testFun (x:rest) failures total = do
    putStrLn ("??? Unexpected " ++ (show x))
    runTests (testFun) rest failures total

markFailure testFun testText expected result rest failures total = do
    putStrLn (show (Failed testText expected result))
    remainder <- runTests (testFun) rest (failures + 1) (total + 1)
    return ((Failed testText expected result):remainder)
