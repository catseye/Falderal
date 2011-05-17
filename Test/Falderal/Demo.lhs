> module Demo where
> import qualified Test.Falderal.Runner as Runner

The Function to be Tested
-------------------------

A function taking Strings to Strings.

> everySecond :: String -> String
> everySecond [] = []
> everySecond [a] = []
> everySecond "silly" = head []
> everySecond "silliness" = error "silliness"
> everySecond (a : b : rest) = (b : everySecond rest)

A function taking Strings to Lists of Booleans.  We test this by
composing it with show.

> parseBits :: String -> [Bool]
> parseBits [] = []
> parseBits ('0':rest) = (False:parseBits rest)
> parseBits ('1':rest) = (True:parseBits rest)

The Falderal Driver
-------------------

Naming the test function "test" makes testing the file as simple as:

    ghc Test/Falderal/Demo.lhs -e test

> test = Runner.run ["Test/Falderal/Demo.lhs"] [] [
>              ("Tests for everySecond", everySecond),
>              ("Tests for parseBits",   show . parseBits)
>            ]

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
? Prelude.head: empty list

Another test that expects an exception.

| silliness
? silliness

Another intentionally failing test to demonstrate how Falderal will
present expecting an exception and not getting one.

| ridiculous
? Prelude.head: empty list

Tests for parseBits
-------------------

We can test functions of type

    f :: (Show a) => String -> a

by simply composing them with show, i.e.

    show . f :: String -> String

| 01
= [False,True]

An intentionally failing test to demonstrate that it is important
to get the formatting of the output right, when testing with show.

| 01
= [False, True]

| 
= []
