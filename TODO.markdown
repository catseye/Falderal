TODO
====

(collected from the Falderal issue tracker on Bitbucket and the
py-falderal issue tracker on github)

Falderal Literate Test Format
-----------------------------

### Policy for expecting both errors and output, success and failure

Policy should be this, by example:

    | test
    = foo

Means: I expect this to succeed and to produce `foo` on stdout, and
I don't care what's on stderr (or — stderr should be empty?)
    
    | test
    = foo
    ? bar

Means: I expect this to succeed, to produce `foo` on stdout, and to
produce `bar` on stderr.

    | test
    ? foo

Means: I expect this to fail, and to produce `foo` on stderr.
And to not care about stdout (or expect it to be empty.)

    | test
    ? foo
    = bar

Means: I expect this to fail, to produce `foo` on stderr, and to
produce `bar` on stdout.

In other words, an error expectation may follow an output expectation
and vice versa.  Error expectations always match stderr, output expectations
always match stdout.  Which one's first should dictate whether we expect
the command to succeed or fail.

What's after here hasn't been re-edited yet.

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

### Allow expectations to be transformed during comparison

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

Likely by way of regexps. This would be particularly valuable in
exception-expecting tests, where we don't care about details such as the
line number of the Haskell file at which the exception occurred.

### Allow equivalency tests to be defined and run.

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

### Test report accumulation

Multiple runs of `falderal` ought to be able to accumulate their results
to a temporary file.  No report should be generated if this is selected.
At the end, the report can be generated from the file.

    rm -f FILE
    falderal --record-to FILE tests1.markdown
    falderal --record-to FILE tests2.markdown
    falderal --report-from FILE

### Support 'weak' testing

In which we only care about whether the command succeeded or failed.
In practice, this could be useful for testing the parser (just test
if these forms parse.)  Or, if not this, then think of something that
would make just testing parsers more useful.

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

But, you'd often want to pipe the output to `less`, which by default makes
control characters visible, defeating colourization.  But there is a flag
to less, `less -R`, which retains colourization.  So use that.

### Flag invalid sequences of lines as errors

2011-08-05

Currently, in `convertLinesToBlocks`, some invalid sequences of lines are
ignored. They should be flagged as errors in the test suite file.

(This was written against Test.Falderal but similar considerations could
be made for py-falderal.)
