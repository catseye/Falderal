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
-- Bird-style embedded code is translated to HTML embedded in the Markdown,
-- with a specific class on the `pre` element, `quoted-code`.  Two reasons:
--
-- 1. In the LHS->Markdown path, Markdown is most likely an intermediate
--    step, before conversion to HTML or such; it's not so important that
--    it be easily readable.
-- 2. This gives you a way to distinguish between plain indented code
--    blocks in Markdown, and blocks of embedded code.
--
-- Given that there are innumerable ways you might want to tweak the
-- output of this formatter, either falderal should let you pass formatter-
-- specific options to the formatter, or falderal should get out of the
-- formatting-to-Markdown business.
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
    "<pre class=\"quoted-code\"><code>" ++
    (prefixEachLine "" text) ++
    "</code></pre>"
formatLine (Pragma text (Just (Encoding _))) =
    ""
formatLine (Pragma text _) =
    (prefixEachLine "    ->" text)
formatLine (SectionHeading text) =
    text ++ "\n" ++ (take (length text) (repeat '-')) ++ "\n"

format lines _ = formatLines (formatLine) lines
