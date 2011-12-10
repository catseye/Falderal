module Test.Falderal.Reporter (report) where

--
-- Dispatch module for Falderal results reporting.
--

import System

import Test.Falderal.Common

import qualified Test.Falderal.Reporter.Standard as Standard
import qualified Test.Falderal.Reporter.FailureDump as FailureDump

--
-- Map from names of reporting styles to reporting functions.
--

getReporter "standard" = Standard.report
getReporter "failure-dump" = FailureDump.report

report :: String -> [Block] -> IO ()

report format blocks =
    (getReporter format) blocks
