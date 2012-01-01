module Test.Falderal.Reporter.Standard (report) where

--
-- Test.Falderal.Reporter.Standard -- Std. report for Falderal test results
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
    reportText 8 "Impl"     (show fn)
    reportText 8 "Input"    testText
    reportText 8 "Expected" (show expected)
    reportText 8 "Actual"   (show actual)
    putStrLn ""
    reportEachTest rest
    where [fn] = fns
reportEachTest (Test id fns literalText testText expected Nothing:rest) = do
    reportText 8 "NOT RUN"  (stripLeading '\n' (stripTrailing '\n' literalText))
    putStrLn ""
    reportText 8 "Impl"     (show fn)
    reportText 8 "Input"    testText
    reportText 8 "Expected" (show expected)
    putStrLn ""
    reportEachTest rest
    where [fn] = fns

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

isFailingTest (Test _ _ _ _ x (Just y)) = if x == y then False else True
isFailingTest _ = True
