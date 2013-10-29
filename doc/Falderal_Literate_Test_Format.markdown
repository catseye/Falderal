Falderal Literate Test Format
=============================

This document describes the proposed Falderal Literate Test Format.

Status
------

This document is a *draft*.  It is nominally "version 0.9" because it
describes something that version 0.9 of `py-falderal` mostly implements.
We will deign to note which sections of this document the current released
version of `pt-falderal` implements, and which it does not.  However,
this document is a work in progress, subject to change, and subject to get
out of sync with `py-falderal`.  It should not be considered to be
anything except a draft until it is described as "version 1.0".

Overview
--------

A Falderal Literate Test Suite is a text file where some of the lines
have special meaning to the Falderal Literate Test Format.  Certain
groupings of the lines defined by this format are intended to denote tests.

A tool which claims to understand this format may choose to extract these
tests, run them, and report which and how many of those tests passed or
failed.  That would be a typical application for this format.  However, it is
not required to do this with the tests; it may, for example, reformat the
tests to produce an output file in a different format.

In the syntax templates below, words in `/slashes/` indicate a variable
rather than literal text.

Syntax
------

Lines which have special meaning to the Falderal Literate Test Format
always begin with an indent of four (4) spaces from the leftmost column
followed immediately by a series of distinguished characters, called an
_introducer_.  The introducers which have meaning to the Falderal Literate
Test Format are as follows:

* `->` (hyphen, greater-than sign): pragma
* `| ` (vertical bar, space): test body text
* `+ ` (plus sign, space): test input text
* `= ` (equals sign, space): expected output text
* `? ` (question mark, space): expected error text
* `> ` (greater-than sign, space): Bird-style embedded code

If the same introducer occurs on multiple adjacent lines, the text after
each introducer is concatenated to form one chunk of relevant text.  This
allows, for example, multi-line text to be given as the body, the input,
or the expected output of a test.  Lines without introducers are called
_intervening text_.

There are some restrictions on the order in which introducers can sensibly
occur:
    
*   Test body text should occur after intervening text.
*   Test body test may be optionally followed by test input text.
    Test input text must be immediately preceded by test body text,
    with no intervening text.
*   Test body text must be followed by either test input text, expected
    output text, or expected error text, with no intervening text.
*   Either expected output or error text must follow either test body
    text or test input text, with no intervening text.

See the sections for these introducers, below, for more details.

Bird-style embedded code is not considered part of a test by the Falderal
Literate Test Format, but may be recognized as such by a formatting tool,
for purposes of formatting.

Lines of intervening text are classified as either blank or non-blank.  A
line is blank if it contains no characters, or if it contains only whitespace.
A group of non-blank lines is referred to as a _paragraph_.

Pragmas
-------

### encoding ###

The encoding pragma allows a Falderal file to specify what encoding is
used for the characters in it.  An implementation of Falderal is not
expected to be able to handle any coding other than UTF-8, however,
this pragma is included for the benefit of text editors and other tools,
to indicate that the document is in fact in UTF-8 encoding.

Example:

    -> encoding: UTF-8

### Functionality-definition ###

The Functionality-definition pragma allows a Falderal file to describe
ways in which a functionality being tested is implemented.  It has the
following syntax:

    -> Functionality /functionality-name/ is implemented by /functionality-type/ /functionality-specifier/

_functionality-type_ must at present be `shell command`.  The format of the
_functionality-specifier_ differs according to the functionality-type.
The _functionality-name_ is arbitrary text enclosed within double quotes,
which may be referenced in a later Tests-for pragma.

Note that the Functionality-definitions given in a Falderal file should
not be considered exhaustive, or even requisite, by a tool.  The tool may
accept additional definitions of a functionality, referencing it by its
name, from an external source such as the command line or a configuration
file, and may be instructed to ignore certain Functionality-definitions in
a Falderal file (if, for example, certain implementation are not currently
available or of interest to the user.)  Indeed, the functionality referred
to by a _functionality-name_ in a Tests-for pragma need not be defined by any
Functionality-definition pragma in the same Falderal file, and this
situation requires the functionality to be specified to the tool in some
other manner.

#### Shell commands ####

For shell commands, the _functionality-specifier_ is in the format
`"command arg1 arg2 ... argn"`.  Any line of legal Bourne shell syntax may
be used, so pipes, redirection, etc., are supported.  Note that the double
quotation mark characters used to enclosed the command have meaning only to
the Falderal format — they are not part of the command, are not passed to the
shell, and do not require double quotation mark characters that are enclosed
by them to be escaped with a backslash.

Certain subsequences, called _variables_, if present in the command string,
will be expanded before execution, and will alter how the command reads the
text of the test and produces its output, to be compared with the expected
output.

##### `%(test-body-file)` #####

The variable `%(test-body-file)` will be replaced by the name of a file which
contains the text of the test body.  This may be a temporary file created
solely for this purpose by the Falderal implementation.

##### `%(test-body-text)` #####

The variable `%(test-body-text)` will be replaced by the actual text of the
test body.  It is assumed that `%(test-body-text)` will appear inside single
quotes in the command string, so any single quotes in the text of the test will
be escaped by the Falderal implementation by preceding them with backslashes.

##### `%(test-input-file)` #####

The variable `%(test-input-file)` will be replaced by the name of a file which
contains the text of the test input.  This may be a temporary file created
solely for this purpose by the Falderal implementation.

##### `%(test-input-text)` #####

The variable `%(test-input-text)` will be replaced by the actual text of the
test input.  It is assumed that `%(test-input-text)` will appear inside single
quotes in the command string, so any single quotes in the text of the test will
be escaped by the Falderal implementation by preceding them with backslashes.

If neither of the variables `%(test-body-file)` nor `%(test-body-text)` appear
in the command string, the test body text will be provided on the standard
input of the shell command.

If neither of the variables `%(test-input-file)` nor `%(test-input-text)` appear
in the command string, the test input text will be provided on the standard
input of the shell command.

If both the test body text and the test input text are slated to appear on the
input of the shell command, then the behaviour is (presently) undefined.

##### `%(output-file)` #####

The variable `%(output-file)` will be replaced by the name of a file
(temporary file) to which the test results will be written.  If it does
not appear in the command string, the output text will be read from
the standard output of the command.

How shell commands support error output is not yet standardized.

For example:

    -> Functionality 'Prepending foo.txt' is implemented by shell command "cat foo.txt %(test-file) > %(output-file)"

### Tests-for ###

The Tests-for pragma determines what functionality will be used to run all
following tests, until the next Tests-for pragma.  It has the following
syntax:

    -> Tests for /functionality-name/

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

Test Body, Test Input and Expected Text
---------------------------------------

Each section of test body text may or may not be followed by a section of
test input text; either way it must then be followed immediately by either
and expected output section or expected error section.

Valid examples:
    
    | thing to test
    = output to expect

    | thing to test
    ? error to expect

    | thing to test
    + input to give it
    = output to expect

    | thing to test
    + input to give it
    ? error to expect

Invalid examples:

    | thing to test

...needs an expectation.

    + input to give it
    = output to expect

...test input must be preceded by a test body.

    ? error to expect

...expectation must be preceded by either test input or test body.

A test body section may also be preceded by a paragraph of text; the
intent of the Falderal Literate Test Format is that this text should
describe the test, or rather, the aspect of the behaviour of the system
that the test is meant to check.  It is therefore reasonable that this
text should be displayed along with the contents (test body text and
expected output or error) of the test, in, for example, a test result
report.

Discussion
==========

(This section is non-normative.)

Typically, a file in Falderal Literate Test Format will also be in
Markdown format.

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

Here is why directly specifying the functionality implementation in a
Tests-for pragma is discouraged.  Saying `-> Tests for shell command
"flang %(test-body-text)"` ties a set of tests to a particular executable,
that is to say a particular *implementation* of a language, which is being
tested, but a large part of the point of Falderal is to let you write
tests for a *language*, and that language might have *many* implementations.
Decoupling them allows you to change what actual functions or programs
are being tested, and basically allow you to have multiple implementations
of a language and use the same tests for all of them.
