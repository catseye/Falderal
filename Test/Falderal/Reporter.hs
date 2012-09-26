module Test.Falderal.Reporter (report) where

--
-- Dispatch module for Falderal results reporting.
--

import Test.Falderal.Common

import qualified Test.Falderal.Reporter.Standard as Standard

--
-- Map from names of reporting styles to reporting functions.
--

getReporter "standard" = Standard.report

report :: String -> [Block] -> [Block] -> IO ()

report format blocks failures =
    (getReporter format) blocks failures
