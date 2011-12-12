module Test.Falderal.Formatter (format) where

--
-- Test.Falderal.Formatter -- The Falderal Test Suite Formatter
--

import System.IO

import Test.Falderal.Common
import qualified Test.Falderal.Formatter.Identity as Identity
import qualified Test.Falderal.Formatter.Markdown as Markdown
import qualified Test.Falderal.Formatter.Haskell as Haskell
import qualified Test.Falderal.Formatter.Shell as Shell

--
-- Driver for Falderal file formatting.
--

--
-- Map from names of formats to formatter functions.
--

getFormatter "identity" = Identity.format
getFormatter "markdown" = Markdown.format
getFormatter "haskell"  = Haskell.format
getFormatter "shell"    = Shell.format
getFormatter "dump"     = dumpLines

dumpLines lines blocks = formatLines (\x -> (show x) ++ "\n") lines

--
-- Some formats, mainly the "human-readable" ones, format by (coalesced)
-- lines, because by the time the file has been parsed into blocks, some
-- content has been dropped.
--
-- Other formats, mainly the "run these tests" ones, format by blocks,
-- because they contain the most relevant information for testing.
--
-- This is why loadFile returns both.
--

format format lines blocks =
    (getFormatter format) lines blocks
