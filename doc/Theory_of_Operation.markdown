Theory of Operation
===================

The `falderal` tool from the `Test.Falderal` implementation of the
Falderal Literate Test Format allows the user to format Falderal tests
to different formats, and to run those tests and generate a report.

This document briefly describes how it works internally.

When `falderal` is asked to run a set of tests, first it formats them
to a set of programs which run the functionalities being tested with the
input text of the tests.  These programs are called *results generators*.
Since each test may have one or more implementations, multiple results
generators may be generated, one for each implementation language
(currently Haskell and Bourne shell).

Each results generator runs many functions in a batch, for efficiency.
The results of running the functions are written to standard output
(which is redirected to a temporary file by `falderal`) in an intermediate
format.  `falderal` then reads these temporary files, parses the
intermediate format, checks which of the test results do not match the
expected output, and generates a test report based on that.

The intermediate format is a simple text-based format containing groups of
lines.  A single group may look like the following.

    output
    4
    2
    Test with ID 4 generated
    two lines of output.

The first line is either `output` or `exception`.  The next line contains
the ID of the test that generated this result.  The line following that
contains the number of lines of text that the test generated (call it _n_).
The next _n_ lines contain the actual text generated.  (If _n_ = 0, there
will be no such lines.)  Immediately following this group will be either
another group, or the end-of-file.

The second and third lines in a group contain natural numbers; they may
contain arbitrary text after the final digit of the natural number, which is
ignored.  (This is to simplify their generation from shell scripts, where
`wc -l` is used to produce the number of lines of output text, and where
`wc` also outputs the filename.)
