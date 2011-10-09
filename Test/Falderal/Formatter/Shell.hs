module Test.Falderal.Formatter.Shell (format) where

--
-- Test.Falderal.Formatter.Shell -- Shell-script compiler for Falderal
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
-- Formatting function which compiles a Falderal file to a shell script.
--

format _ blocks =
    prelude ++ (formatBlocks blocks) ++ postlude

formatBlocks (test@(Test (ShellTest cmd) desc input expectation):rest) =
    let
        Output expected = expectation
        inputHereDoc = hereDoc "input.txt" input
        expectedHereDoc = hereDoc "expected.txt" expected
        hereDocs = inputHereDoc ++ expectedHereDoc
        command = cmd ++ " <input.txt >output.txt\n"
        diff = "diff -u expected.txt output.txt || echo 'failed'\n"
        cleanUp = "rm -f input.txt expected.txt output.txt\n"
        formattedBlock = hereDocs ++ command ++ diff ++ cleanUp
    in
        formattedBlock ++ "\n" ++ formatBlocks rest
formatBlocks (_:rest) =
    formatBlocks rest
formatBlocks [] =
    ""

-- XXX derive sentinel from text

hereDoc filename text =
    "cat >" ++ filename ++ " <<EOF\n" ++ text ++ "\nEOF\n"

prelude =
    "#!/bin/sh\n\
    \\n\
    \\n"

postlude =
    "\n\
    \\n"
