TODO
====

(collected from the [Falderal issue tracker on Bitbucket][] and the
[py-falderal issue tracker on github][])

Falderal Literate Test Format
-----------------------------

### Support specifying input to tests

2011-10-13

Since Falderal is shaping up to be (good at being) a test framework for
languages, often, each test presents an example program in the language
being tested. And guess what, some programs don't just produce output,
they take input, too. It would be really handy, then, to be able to specify
some input to provide to a test when it is run. This could be specified in a
block after the test contents block, with its own introducer. Example with
a simple Scheme-like language:

    | (* 5 (read))
    : 100
    = 500

The same test block could be re-used with multiple input blocks.

    | (* 5 (read))
    : 100
    = 500

    : 5
    = 25

How to actually implement this might take some thinking. For shell commands,
different `%test` and `%input` variables could be used, like

    shell command "myinterp %test <%input >%output"

For Haskell functions, the obvious thing is to have the function be
`String -> String -> String`, but do we insist all Haskell functions to be
tested have that signature even if they don't have separate input, or do we
have a different pragma to indicate "with input", or...?

### Allow expectations to be transformed during comparison

2011-11-08

It would be nice to allow expectations to be transformed before they are
compared to the actual output. The main use case for this that I can think of
is to allow the expected output to be "pretty printed" (that is, nicely
formatted) in the Falderal file, while the functionality being tested just
produces a dump. The nicely formatted expected output should be "crunched"
into the same ugly format as the dump.

This doesn't work as well the other way; although one could compose the
functionality being tested with an actual pretty-printer, that would
prescribe a certain indentation scheme etc. that the expected output would
have to match exactly. It would be rather better if the writer of the tests
could format their expected output as they find most aesthetically pleasing
in their literate tests, and have that be transformed instead.

This might be somewhat tricky, however; if the transformation applied is
too powerful, it can distort or eliminate the meaning of the test, and erode
confidence.

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

### Support cd'ing to directory containing Falderal file before running tests

2011-12-01

For Falderal files which specify implementations of functionalities by shell
commands, it is useful for them to be able to locate the shell command
they want to run. If it is not on the search path, the directory in which
`falderal` is invoked is used as the directory on which relative paths are
based.

This is less useful than using the directory that the Falderal file is in,
as the directory on which relative paths are based. So, it would be useful
to `cd` into that directory before running the tests. Add an option to
`falderal` to do this automatically, or maybe by default. This would let
the user invoke `falderal` from any location they like, so long as the
Falderal document is in the right place.

(This was written against Test.Falderal but similar considerations could
be made for py-falderal.)

### Show filenames and implementations in standard failure report

2011-12-11

To make the standard failure report more useful, it should show the source
Falderal document filename(s) in the summary, the relevant source filename
in each error, and the implementation used to test the functionality in
each error.

### Flag invalid sequences of lines as errors

2011-08-05

Currently, in `convertLinesToBlocks`, some invalid sequences of lines are
ignored. They should be flagged as errors in the test suite file.

### Understand Windows paths in shell command implementations

2012-05-13

On Windows, `ghc` is compiled with the Windows-like understanding of paths,
which encompasses backslashes, but not forward slashes. Thus, if `falderal`
is compiled with that `ghc`, it will not understand it when you say
`is implemented by shell command "./bin/foo.sh"`.

`falderal` should be able to say to itself, "oh, I'm running on Windows", and
at least convert those `/`'s to `\`'s.

(This was written against Test.Falderal but py-falderal should at least be
tested on Windows Python.)

### Haskell exceptions sometimes not caught correctly

2011-10-18

For example, you might have a function which in some cases evaluates to
error "undefined term". Your test for this function might look like

    | 71k
    ? undefined term

But the generated Haskell program which runs the tests won't recognize
that correctly, and output:

    Input:
    71k
    Expected: Exception "undefined term"
    <interactive>: undefined term

2012-05-13: Root cause is known: evaluation must be both strict *and* deep,
in order to catch exceptions thrown during recursion. Possible solution is to
use `show` as a "poor man's deepseq" to force deep evaluation.
