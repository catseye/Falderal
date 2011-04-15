module Test.Falderal.Runner where

import System
import qualified Control.Exception as Exc

data Block = Test String
           | ExpectedResult String
           | ExpectedError String
           | Comment String
           deriving (Show, Eq, Ord)

data Result = Success String
            | Failed String String String
            deriving (Show, Eq, Ord)

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
    | prefix == "> " = Test suffix
    | prefix == "= " = ExpectedResult suffix
    | prefix == "! " = ExpectedError suffix
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

runTests testFun [] failures total = do
    putStrLn ("Total tests: " ++ (show total) ++ ", failures: " ++ (show failures))
    return []
runTests testFun ((Test testText):((ExpectedResult expected):rest)) failures total = do
    result <- eeval (testFun) testText
    if result == expected then do
        runTests (testFun) rest failures (total + 1)
      else do
        putStrLn (show (Failed testText expected result))
        remainder <- runTests (testFun) rest (failures + 1) (total + 1)
        return ((Failed testText expected result):remainder)
runTests testFun ((Test testText):((ExpectedError expected):rest)) failures total = do
    result <- eeval (testFun) testText
    if (take 1 result) == "$" then do
        runTests (testFun) rest failures (total + 1)
      else do
        putStrLn (show (Failed testText expected result))
        remainder <- runTests (testFun) rest (failures + 1) (total + 1)
        return ((Failed testText expected result):remainder)
runTests testFun (x:rest) failures total = do
    putStrLn ("??? Unexpected " ++ (show x))
    runTests (testFun) rest failures total

eeval :: (String -> String) -> String -> IO String
eeval testFun x = do
    r <- Exc.try (do return (testFun x))
    case r of
        Right a -> return (show a)
        Left e -> return ("$" ++ (pretty (show (e :: Exc.SomeException))))

pretty string
    | (length string) <= 78 = string
    | otherwise             = (take 75 string) ++ "..."

