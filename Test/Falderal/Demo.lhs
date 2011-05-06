> module Demo where
> import qualified Test.Falderal.Runner as Runner

The Function to be Tested
-------------------------

> everySecond :: String -> String
> everySecond [] = []
> everySecond [a] = []
> everySecond "silly" = head []
> everySecond (a : b : rest) = (b : everySecond rest)

The Falderal Driver
-------------------

> testDemo = Runner.run "Test/Falderal/Demo.lhs" [
>                         ("Tests for everySecond", everySecond)
>                       ]

Tests for everySecond
---------------------

Every second symbol in the string is retained.

| Falderal
= adrl

Works for odd-length strings, too.

| Bandana
= adn

If there aren't even two symbols in the string, the result is
the empty string.  Note that we have to precede the expected
empty string with "= ", that is, an equals sign and a space.

| A
= 

This is an intentionally failing test, to demonstrate how Falderal will
present it.

| Something
= Anything

A test that expects an exception.

| silly
? Exception

Another intentionally failing test to demonstrate how Falderal will
present expecting an exception and not getting one.

| ridiculous
? Exception
