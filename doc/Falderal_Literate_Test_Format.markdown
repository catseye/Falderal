Falderal Literate Test Format
=============================

This document describes the proposed Falderal Literate Test Format.

Status
------

This document is a *draft*.  It is nominally "version 0.12" because it
describes something that version 0.12 of `py-falderal` mostly implements.
We will deign to note which sections of this document the current released
version of `py-falderal` implements, and which it does not.  However,
this document is a work in progress, subject to change, and subject to get
out of sync with `py-falderal`.  It should not be considered to be
anything except a draft until it is described as "version 1.0".

Overview
--------

A Falderal Literate Test Suite is a plain text file where some of the lines
have special meaning to the Falderal Literate Test Format.  Certain
groupings of the lines defined by this format are intended to denote tests,
while others denote pragmas which specify how those tests should be run.
Outside of these groupings, lines have no meaning to the Falderal Literate
Test Format other than that they are presumed to be mainly descriptive text.

The plain text file may also be formatted in some other format, such as
Markdown, although this is not required.

A tool which claims to understand the Falderal Literate Test Format may
choose to extract the tests from such a document, run them, and report which
and how many of them passed or failed.  That would be a typical application
for this format.  However, no tool is strictly required to do this with the
tests found in a document; a tool may, for example, simply reformat the
tests to produce an output file in a different format.

In the syntax templates below, words in `/slashes/` indicate a variable
rather than literal text.

Syntax
------

Each grouping of lines which has special meaning to the Falderal Literate
Test Format always begins with an indent of four (4) spaces from the
leftmost column, preceded by non-indented text, and followed by either
non-indented text or the end of the file.  Such a grouping of lines is
called a _block_.

There are two general formats to any block.  In the first, "verbose" format,
each indent of 4 spaces is followed immediately on that line by distinguished
sequence of characters, called an _introducer_.  The introducers which have
meaning to the Falderal Literate Test Format are as follows:

*   `->` (hyphen, greater-than sign): pragma
*   `| ` (vertical bar, space): test body text
*   `+ ` (plus sign, space): test input text
*   `= ` (equals sign, space): expected output text
*   `? ` (question mark, space): expected error text

If the same introducer occurs on multiple adjacent lines, the text after
each introducer is concatenated to form one chunk of relevant text.  This
allows, for example, multi-line text to be given as the body, the input,
or the expected output of a test.

There are some restrictions on the order in which introducers can sensibly
occur in a block:

*   Test body text should occur at the start of a block.
*   Test body test may be optionally followed by test input text.
*   The first test input text must be immediately preceded by test body text.
*   Subsequent test input texts need not be preceded by a test body text;
    in this case, the previously-appearing test body text will be used again.
*   Test body text must be followed by either test input text, expected
    output text, or expected error text, with no intervening text.
*   Either expected output or error text must follow either test body
    text or test input text, with no intervening text.

See the sections for these introducers, below, for more details.

In the other, "freestyle" format, not all lines in a block require
introducers.  A freestyle format block is indentified as such if one or
more of the final lines of the block begin with any of the following
introducers:

*   `=> `: expected output text
*   `==> `: expected output text
*   `===> `: expected output text
*   `?> `: expected error text
*   `??> `: expected error text
*   `???> `: expected error text

In addition, the following introducers may be used to mark a section
of test input text on the first of the final lines (but may not be
used to end a block):

*   `<= `: test input text
*   `<== `: test input text
*   `<=== `: test input text

If a block is identified as a freestyle block, all lines preceding the
first final line appearing with one of these introducers, are interpreted
as having no introducer at all (even if they begin with `| ` or some other
sequence already mentioned) and are used as the test body block.

Lines without introducers are called _intervening text_.
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

Test Body, Test Input and Expected Text
---------------------------------------

Each section of test body text may or may not be followed by a section of
test input text; either way it must then be followed immediately by either
and expected output section or expected error section.

Valid examples in the "verbose" format:
    
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

    + different input to give the immediately previously defined test body
    ? different error to expect

Valid examples in the "freestyle" format:

    thing to test
    => output to expect

    thing to test
    ==> output to expect

    thing to test
    ===> output to expect

    thing to test
    ?> error to expect

    thing to test
    ??> error to expect

    thing to test
    ???> error to expect

    thing to test
    <=== input to give it
    ===> output to expect

    thing to test
    <=== input to give it
    ???> error to expect

Invalid examples:

    | thing to test

...needs an expectation.

    + input to give it
    = output to expect

...test input must be preceded by a test body, if this is the first test.

    <=== input to give it
    ???> output to expect

...test input must be preceded by a test body always, in freestyle format.

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
