> module LiterateHaskellDemo where

Falderal in Literate Haskell
============================

This file demonstrates how Falderal tests can be embedded in a
Bird-style Literate Haskell source file.

This module was written by Chris Pressey.  It is hereby placed in
the public domain.

The Function to be Tested
-------------------------

A function taking Strings to Strings.

> everySecond :: String -> String
> everySecond [] = []
> everySecond [a] = []
> everySecond "silly" = head []
> everySecond "silliness" = error "silliness"
> everySecond "supercilious" = error "not\non\nmy\nwatch"
> everySecond "suoilic\nrepus" = "not\non\nmy\nwatch"
> everySecond (a : b : rest) = (b : everySecond rest)

A function taking Strings to Lists of Booleans.  We test this by
composing it with show.

> parseBits :: String -> [Bool]
> parseBits [] = []
> parseBits ('0':rest) = (False:parseBits rest)
> parseBits ('1':rest) = (True:parseBits rest)
> parseBits ('\n':rest) = parseBits rest

> showParseBits = show . parseBits

Pragmas are able to extend over multiple lines, just like anything else.
In addition, spaces between words (outside of quotes strings) are not
significant; you can have as many as you like.

-> Functionality "Retain every second character"
-> is         implemented
-> by Haskell function LiterateHaskellDemo:everySecond

Tests for everySecond
---------------------

-> Tests for functionality "Retain every second character"

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

The expected text of an exception can extend over several lines.

| supercilious
? not
? on
? my
? watch

The input and expected text and extend over several lines, too.

| suoilic
| repus
= not
= on
= my
= watch

Another intentionally failing test to demonstrate how Falderal will
present expecting an exception and not getting one.

| ridiculous
? Prelude.head: empty list

Tests for parseBits
-------------------

-> Tests for Haskell function LiterateHaskellDemo:showParseBits

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

Input can consist of multiple lines of text.  These are joined together
with intervening newline characters.

| 00
| 11
= [False,False,True,True]

An intentionally failing test to demonstrate show what a failure
looks like on multi-line input.

| 01
| 10
= [False,False,True,True]

If we have a set of tests where the tests after the first one have no
descriptions of their own, we can take this to suggest they are all
testing the same thing.  In this case, the literal text that is displayed
when any of them fails is the text that comes before the first of them,
annotated with the number of the test in the set that failed.  The
intentionally-failing third test below demonstrates this.

| 00
= [False,False]

| 000
= [False,False,False]

| 0000
= [False,False,False,Flse]