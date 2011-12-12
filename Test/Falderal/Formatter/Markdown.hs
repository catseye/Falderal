module Test.Falderal.Formatter.Markdown (format) where

--
-- Test.Falderal.Formatter.Markdown -- Markdown formatter for Falderal format
--

import Test.Falderal.Common

--
-- Formatting function which formats a Falderal file to vanilla Markdown
-- file.  Falderal-specific sections (test input, expected results) are still
-- presented with Falderal syntax.
--

formatLine (TestInput text) =
    (prefixEachLine "    | " text)
formatLine (ExpectedResult text) =
    (prefixEachLine "    = " text)
formatLine (ExpectedError text) =
    (prefixEachLine "    ? " text)
formatLine (LiteralText text) =
    (prefixEachLine "" text)
formatLine (QuotedCode text) =
    (prefixEachLine "    " text)
formatLine (Pragma text (Just (Encoding _))) =
    ""
formatLine (Pragma text _) =
    (prefixEachLine "    ->" text)
formatLine (SectionHeading text) =
    text ++ "\n" ++ (take (length text) (repeat '-')) ++ "\n"

format lines _ = formatLines (formatLine) lines
