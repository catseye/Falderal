module Test.Falderal.Formatter.Haskell (format) where

--
-- Test.Falderal.Formatter.Haskell -- Haskell compiler for Falderal
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

import Test.Falderal.Common

--
-- Formatting function which compiles a Falderal file to Haskell source.
--

formatBlocks (test@(Test desc text expectation):rest) =
    "    " ++ (show test) ++ ",\n" ++ (formatBlocks rest)
formatBlocks (_:rest) =
    formatBlocks rest
formatBlocks [] =
    ""

format _ blocks =
    (prelude blocks) ++ (formatBlocks blocks) ++ postlude

gatherImports ((HaskellDirective moduleName functionName):rest) =
    "import " ++ moduleName ++ "\n" ++ gatherImports rest
gatherImports (_:rest) =
    gatherImports rest
gatherImports [] =
    ""

--
-- XXX this hard-codes some stuff, just to see things running
-- XXX instead, scan list for HaskellDirectives, pluck out module
-- names and turn them into imports.  Put function names in
-- list somehow.
--

prelude blocks =
    "module GeneratedFalderalTests where\n\
    \\n\
    \import Test.Falderal.Common\n\
    \import Test.Falderal.Runner\n" ++ (gatherImports blocks) ++ "\
    \\n\
    \testModule = runTests [] (everySecond) [\n"

postlude =
    "    (Section \"DONE\")\n\
    \    ]\n"
