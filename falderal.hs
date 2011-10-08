import System
import System.Environment

import Test.Falderal.Common
import Test.Falderal.Loader
import Test.Falderal.Runner
import Test.Falderal.Formatter
import Test.Falderal.Reporter.Standard

--
-- This module contains entry points to Falderal functionality intended
-- for use by users.
--

main :: IO ()
main = do
    args <- getArgs
    dispatch args

dispatch ("format":formatName:fileName:[]) = do
    formatFile formatName fileName

dispatch ("test":reportFormat:filename:[]) = do
    putStrLn "'test' command not implemented yet"
--        failures <- runTests testTuples
--        report testTuples failures

dispatch _ = do
    putStrLn "Usage: falderal command {args}"
    putStrLn "where command is one of:"
    putStrLn "    format format-name input-falderal-filename"
    putStrLn "    test report-style input-falderal-filename"
