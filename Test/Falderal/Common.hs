module Test.Falderal.Common where

--
-- Test.Falderal.Common -- Common data def'n and functions for Falderal
--

import qualified Char

--
-- Definitions for the structure of a test suite in Falderal format.
--

--
-- Before processing...
--

data Line = TestInput String
          | ExpectedResult String
          | ExpectedError String
          | LiteralText String
          | QuotedCode String
          | SectionHeading String
          | Pragma String (Maybe Directive)
          | Placeholder
          deriving (Show, Eq, Ord)

--
-- ...and in the middle of processing...
--

data Directive = TestsFor Functionality
               | FunctionalityDefinition String Functionality
               | Encoding String
               deriving (Show, Eq, Ord)

data Functionality = HaskellTest String String -- module name, function name
                   | ShellTest String -- command
                   | NamedFunctionality String
                   deriving (Show, Eq, Ord)

--
-- ...and after.
--

data Expectation = Output String
                 | Exception String
                 deriving (Show, Eq, Ord)

--
-- First element is the test ID.
-- Second element is a  list of functionalities being tested.
-- Third element is the literal text preceding the test.
-- Fourth element is the textual input to the test.
-- Fifth element is what we expect the test to result in.
-- Sixth element is the actual result of the test, after it has been run.
--

data Block = Section String
           | Test Int [Functionality] String String Expectation (Maybe Expectation)
           deriving (Show, Eq, Ord)

--
-- Common functions.
--

-- TODO: How many of these can be replaced by standard Haskell functions?

discoverRepeatedCharacter [] =
    Nothing
discoverRepeatedCharacter (first:rest)
    | all (\x -> x == first) rest = Just first
    | otherwise                   = Nothing

allWhitespace = all Char.isSpace

stripLeading y = dropWhile $ \x -> x == y

stripTrailing y str = reverse (stripLeading y (reverse str))

stripLeadingWhitespace = dropWhile $ Char.isSpace

--
-- A version of `lines` that always considers the input "" to
-- represent a single, blank line.
--

allLines x =
    case (lines x) of
        []    -> [""]
        other -> other

prefixEachLine prefix text =
    foldl (++) "" (map (\x -> prefix ++ x ++ "\n") (allLines text))

formatLines formatter lines = foldl (++) "" (map (formatter) lines)

pad s n = padFrom s (n-(length s))
padFrom s n
    | n <= 0    = s
    | otherwise = padFrom (s ++ " ") (n-1)

join _ [] = ""
join _ [x] = x
join glue (x:xs) = x ++ glue ++ (join glue xs)
