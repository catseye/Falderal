TODO
====

(collected from the [Falderal issue tracker on Bitbucket][] and the
[py-falderal issue tracker on github][])

Falderal Literate Test Format
-----------------------------

### Allow use of patterns in expected output

2011-05-17

Likely by way of regexps. This would be particularly valuable in
exception-expecting tests, where we don't care about details such as the
line number of the Haskell file at which the exception occurred.

### Allow equivalency tests to be defined and run.

2011-05-17

To test functions of type `(Eq a) => String -> a`, you should be able to
give give multiple input strings in a set; if the function does not map
them all to the same value, that's a test failure.

Syntax for an equivalency test might look like this:

    | 2+2
    ==
    | 3+1
    ==
    | 7-3


Test.Falderal
-------------

### Show filenames and implementations in standard failure report ###

2011-12-11

To make the standard failure report more useful, it should show the source
Falderal document filename(s) in the summary, the relevant source filename
in each error, and the implementation used to test the functionality in
each error.

### Flag invalid sequences of lines as errors ###

2011-08-05

Currently, in `convertLinesToBlocks`, some invalid sequences of lines are
ignored. They should be flagged as errors in the test suite file.
