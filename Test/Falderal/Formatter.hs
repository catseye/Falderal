module Test.Falderal.Formatter (formatFile) where

--
-- Test.Falderal.Formatter -- The Falderal Test Suite Formatter
-- Copyright (c)2011 Cat's Eye Technologies.  All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
--
--  1. Redistributions of source code must retain the above copyright
--     notices, this list of conditions and the following disclaimer.
--  2. Redistributions in binary form must reproduce the above copyright
--     notices, this list of conditions, and the following disclaimer in
--     the documentation and/or other materials provided with the
--     distribution.
--  3. Neither the names of the copyright holders nor the names of their
--     contributors may be used to endorse or promote products derived
--     from this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
-- FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE
-- COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
-- INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
-- BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
-- LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
-- ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.
--

import System

import Test.Falderal.Loader
import qualified Test.Falderal.Formatter.Identity as Identity
import qualified Test.Falderal.Formatter.Markdown as Markdown
import qualified Test.Falderal.Formatter.Haskell as Haskell

--
-- Driver for Falderal file formatting.
--

--
-- Map from names of formats to formatter functions.
--

getFormatter "identity" = Identity.format
getFormatter "markdown" = Markdown.format
getFormatter "haskell"  = Haskell.format
getFormatter "dump"     = formatLines (\x -> (show x) ++ "\n")

--
-- We format by (coalesced) lines, instead of by blocks, because by the time
-- the file has been parsed into blocks, some content has been dropped.
--

formatFile format fileName = do
    testText <- readFile fileName
    lines <- return $ transformLines $ lines testText
    outputText <- return $ (getFormatter format) lines
    putStr outputText
