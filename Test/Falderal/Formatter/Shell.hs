module Test.Falderal.Formatter.Shell (format) where

--
-- Test.Falderal.Formatter.Shell -- Bourne shell script compiler for Falderal
--

import Test.Falderal.Common

--
-- Formatting function which compiles a Falderal file to a shell script.
--
-- XXX tests should be partitioned before this is called, but this should
-- be able to handle multiple shell-implemented functionalities.
--

format _ blocks =
    prelude ++ (formatBlocks blocks) ++ postlude

formatBlocks (test@(Test id [(ShellTest cmd)] desc body _ _):rest) =
    let
        inputHereDoc = hereDoc "input.txt" body
        cmd' = expandCommand cmd body
        formattedBlock = inputHereDoc ++ testExecution cmd' id
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
    \# This file was automatically generated by Test.Falderal.Formatter.Shell\n\
    \# Edit at your own risk!\n\
    \\n"

postlude =
    "\n\
    \rm -f input.txt output.txt\n"

-- TODO: capture output/errors from command, even when output variable
-- is present

expandCommand cmd body =
    let
        substitutions = [
                          ("test-file", "input.txt"),
                          ("test-text", escapeSingleQuotes body),
                          ("output-file", "output.txt")
                        ]
        suppliedInput =
            if
                containsVariable cmd "test-file" ||
                containsVariable cmd "test-text"
            then
                ""
            else
                " <input.txt"
        providedOutput =
            if
                containsVariable cmd "output-file"
            then
                ""
            else
                " >output.txt"
        cmd' = expandVariables cmd substitutions
    in
        cmd' ++ suppliedInput ++ providedOutput

testExecution cmd id =
    cmd ++ " 2>&1\n\
    \if [ $? != 0 ]; then\n\
    \  echo \"exception\"\n\
    \else\n\
    \  echo \"output\"\n\
    \fi\n\
    \echo " ++ (show id) ++ "\n\
    \falderal newlinify output.txt >output2.txt\n\
    \mv output2.txt output.txt\n\
    \echo `wc -l output.txt`\n\
    \cat output.txt\n"
