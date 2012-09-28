module Test.Falderal.Formatter.Identity (format) where

--
-- Test.Falderal.Formatter.Identity -- Identity formatter for Falderal format
--

import Test.Falderal.Common

--
-- Formatting function which formats a Falderal file to an identical
-- Falderal file.
--

formatLine (TestInput text) =
    (prefixEachLine "| " text)
formatLine (ExpectedResult text) =
    (prefixEachLine "= " text)
formatLine (ExpectedError text) =
    (prefixEachLine "? " text)
formatLine (LiteralText text) =
    (prefixEachLine "" text)
formatLine (QuotedCode text) =
    (prefixEachLine "> " text)
formatLine (Pragma text _) =
    (prefixEachLine "->" text)
formatLine (SectionHeading text) =
    text ++ "\n" ++ (take (length text) (repeat '-')) ++ "\n"

format lines _ = formatLines (formatLine) lines
