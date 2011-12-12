Falderal Literate Test Format
=============================

This document describes the proposed Falderal Literate Test Format.

Status
======

This document is a *draft*.  It is nominally "version 0.4" because it
describes something that version 0.4 of `Test.Falderal` mostly implements.
We will deign to note which sections of this document the current released
version of `Test.Falderal` implements, and which it does not.  However,
this document is a work in progress, subject to change, and subject to get
out of sync with `Test.Falderal`.  It should not be considered to be
anything except a draft until it is described as "version 1.0".

Overview
========

A Falderal Literate Test Suite is a text file where some of the lines
have special meaning to the Falderal Literate Test Format.  Certain
groupings of the lines defined by this format are intended to denote tests.
A tool which claims to understand this format may chose to extract these
lines, interpret them as tests, and execute these tests and report which
and how many of those tests passed or failed.  It may also reformat these
lines to produce an output file which is no longer in the Falderal
Literate Test Format, but in some other format, generally to present the
tests and their descriptions in a nicely human-readable fashion.

Syntax
======

Lines which have special meaning to the Falderal Literate Test Format
always begin (in the leftmost column) with a series of special characters,
called an //introducer//.  The introducers which have meaning to the
Falderal Literate Test Format are as follows:

* `->` (hyphen, greater-than sign): pragma
* `| ` (vertical bar, space): input text
* `= ` (equals sign, space): expected output text
* `? ` (question mark, space): expected error text
* `> ` (greater-than sign, space): Bird-style embedded code

If the same introducer occurs on multiple adjacent lines, the text after
each introducer is concatenated to form one chunk of relevant text.  This
allows, for example, multi-line text to be given as input or expected
output of a test.

There are some restrictions on the order in which introducers can sensibly
occur.  Expected output or error text must follow input text, with no
intervening introducers or non-introducer lines.  See the sections for
these introducers, below, for more details.

Bird-style embedded code is not considered part of a test by the Falderal
Literate Test Format, but may be recognized as such by a formatting tool,
for purposes of formatting.

Lines which do not begin with an introducer are classified as either blank
or non-blank.  A line is blank if it contains no characters, or if it
contains only whitespace.  A group of non-blank lines is referred to as a
_paragraph_.

Pragmas
=======

Functionality-definition
------------------------

The Functionality-definition pragma allows a Falderal file to describe
ways in which a functionality being tested is implemented.  It has the
following syntax:

    -> Functionality /functionality-name/ is implemented by /functionality-type/ /functionality-specifier/

_functionality-type_ may be either `Haskell function` or `shell command`.
The format of the _functionality-specifier_ differs with each of these.
The _functionality-name_ is arbitrary text enclosed within double quotes,
which may be referenced in a later Tests-for pragma.

Note that the Functionality-definitions given in a Falderal file should
not be considered exhaustive, or even requisite, by a tool.  The tool may
accept additional definitions of the name of a functionality, from an
external source such as the command line or a configuration file, and may
be instructed to ignore certain Functionality-definitions in a Falderal
file (if, for example, certain implementation are not currently available
or of interest to the user.)  Indeed, the functionality referred to by a
_functionality-name_ in a Tests-for pragma need not be defined by any
Functionality-definition pragma in the same Falderal file, and this
situation requires the functionality to be specified to the tool in some
other manner.

Haskell functions
-----------------

For Haskell functions, the _functionality-specifier_ is in the format
`Module:functionName`.  The function should have a signature of
`String -> String`.  If the function raises an exception, the text of that
exception will be compared against expected error output, if any.  For
example:

    -> Functionality 'Reversing a string' is implemented by Haskell function Data.Backwards:reverseString

Shell commands
--------------

For shell commands, the _functionality-specifier_ is in the format
`"command arg1 arg2 ... argn"`.  Any line of legal Bourne shell syntax may
be used, so pipes, redirection, etc., are supported.  The sequence
`%input`, if it appears in the string, is replaced by the name of a
temporary file which is to be created to hold the input text.  If it does
not appear, the input text will be provided on the standard input of the
shell command.  The sequence `%output`, if it appears in the string,
is replaced by the name of a temporary file from which the output text
will be read.  If it does not appear, the output text will be read from
the standard output.  Shell commands do not (yet) support expected error
output.

For example:

    -> Functionality 'Prepending foo.txt' is implemented by shell command "cat foo.txt %input > %output"

`Test.Falderal` version 0.4 implements shell commands, but does not yet
support the `%input` and `%output` sequences.

Tests-for
---------

The Tests-for pragma determines what functionality will be used to run all
following tests, until the next Tests-for pragma.  It has the following
syntax:

    -> Tests for _functionality-name_

The _functionality-name_ refers to a functionality, which may be specified
by a Functionality-definition pragma elsewhere in the Falderal file.

For example:

    -> Tests for 'Reversing a string'

Alternatively, the following direct way of associating tests with an
implementation of a functionality, may be used.  However, this direct way
of specifying a functionality is discouraged when there may be conceivably
be multiple implementation of the functionality.

    -> Tests for _functionality-type_ _functionality-specifier_

_functionality-type_ and _functionality-specifier_ have the same meaning
as given in the description of the Functionality-definition pragma.

For example:

    -> Tests for Haskell function Data.Backwards:reverseString

Input and Expected Text
=======================

Each section of input text should be followed immediately by either a
section of expected output or expected error output.  It may also be
preceded by a paragraph of text; this paragraph should be displayed along
with the input text and expected text, in a test report.

Discussion
==========

(This section is non-normative, and possibly out-of-date.)

The format of the lines which comprise the Falderal Literate Test Format
was chosen to not conflict with many other common text formats (including
but not limited to Bird-style Literate Haskell, and Markdown); thus
literate test suites may be embedded in a wide variety of other formats.
However, there are inevitably some conflicts with some textual formats;
for example, when embedded in C code and many other languages, Falderal
entries should be preceded by `/*` and followed by `*/`, to ensure that
they are regarded as comments.  Also, reStructuredText uses the `|` line
prefix to denote preformatted plain text.

The format of pragmas was chosen such that they could be read literately,
and as such, a formatting tool could format them in the output document
with little if any change.

The Haskell function functionality type is essentially an optimization,
and a convenience to avoid writing Haskell code for a system program which
reads strings, passes them to a function, and outputs the result.  However,
restricting oneself to Haskell functions provides a fairly good guarantee
that the tests are idempotent; shell commands may change the state of the
system.  (It is of course recommended that when shell commands are used,
care is taken to leave the system in the same state as it was when testing
started.)

Here is why directly specifying the functionality implementation in a
Tests-for pragma is discouraged.  Saying `-> Tests for Haskell function
Foo:blah` ties a set of tests to a particular function which is being
tested, but a large part of the point of Falderal is to let you write
tests for a *language*, and that language might have many implementations.
Decoupling them allows you to change what actual functions or programs
are being tested, and basically allow you to have multiple implementations
of a language and use the same tests for all of them.
