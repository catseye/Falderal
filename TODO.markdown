TODO
====

(collected from the Falderal issue tracker on Bitbucket and the
py-falderal issue tracker on github)

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


py-falderal
-----------

### Policy and/or options for looking at stdout and stderr

When you have a program that produces output on both stdout and stderr,
whether it fails or not, you might want to expect text on both stdout and
stderr.

Currently it expects the text on stdout if it is a `=` expectation, and on
stderr if it is a `?` expectation.

You can't work around this so well by tacking `2>&1` onto the end of the
command, because then stderr will always be empty.

We could, by default, tack `2>&1` on the end ourselves and look only at
stdout.  This might be the simplest approach.

We might want to add options that avoid doing that, but if so, what should
they be?  Should each test be able to configure this?  Should a single test
be able to have both `=` and `?` expectations, each for each stream?

This is complicated by the presence of `%(output-file)`; currently, if that
is given, stdout is ignored in preference to it (but stderr is still
checked, if the command failed.  There should probably be a corresponding
`%(error-file)` variable.)

I think the current behaviour could work, with the following policy:

If the command succeeds, your `=` expectation will be matched against
stdout only.  If you wish to match against both stdout and stderr in these
cases, add `2>&1` to your shell command.

If the command fails, your `?` expectation will be matched against stderr
only.  If you wish to match against both stdout and stderr in these cases,
add `1>&2` to your shell command.

Either way, it's still worth investigating whether it's worthwhile to have
both `=` and `?` expectations on a single test.  (I can't convince myself
that stdout and stderr will always be combined deterministically, and
having both kinds of expectations would allow non-deterministic combinations
of the two to be matched.)

### Split InterveningMarkdown blocks to make nice test descriptions

For example, if we have

    ...test #1...
    Some text.
    Heading
    -------
    More text
    ...test #2...

The description for test #2 should consist of "More text"; possibly also
the heading, but not "Some text".  This can take place in a pre-processing
phase which simply splits every `InterveningMarkdown` block into multiple
blocks, at its headers.  It should understand both underlined and atx-style
headers.

### option to colourize test result output

Using one of the approaches listed here:

http://stackoverflow.com/questions/287871/print-in-terminal-with-colors-using-python

...py-falderal ought to provide an option (not default, of course, and not
if stdout is not a tty) to colorize the output with, of course, pass=green,
fail=red.

OTOH, you'd often want to pipe the output to less, which will disable
colourization anyway, on the tty check.  So maybe look at how `cdiff` allows
colourized text to be paged, first.

### Flag invalid sequences of lines as errors

2011-08-05

Currently, in `convertLinesToBlocks`, some invalid sequences of lines are
ignored. They should be flagged as errors in the test suite file.

(This was written against Test.Falderal but similar considerations could
be made for py-falderal.)
