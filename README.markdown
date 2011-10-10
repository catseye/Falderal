Falderal
========

Falderal is a file format for literate test suites.  It is particularly
suited for testing and documenting functions which interpret or compile
programming languages. The dumbed-down sound-bite version: "doctests for
DSLs".  `Test.Falderal` is the reference implementation, in Haskell, of
tools for formatting and running tests written in Falderal.

Here's the scenario:

* You have a file format, or a language (perhaps a programming language)
  and some rules for processing it (interpreting it, compiling it, etc.)
* You have functions written (currently) in Haskell which implement these
  rules.
* You have some tests for those rules (e.g. program A always produces
  output B) that you want to present in a nicely readable fashion,
  possibly included right in your literate programming source file.

If this describes you, then Falderal might help.  The particular need I had
that encouraged me to write it is having implemented several esoteric
programming languages in Haskell, and wanting to write nicely formatted
tests suites for them.

History
-------

The current released version of Falderal is 0.3.  It introduced the following
features:

* The definition of a Falderal Literate Test Format, distinct from the
  reference implementation of tools for it in Haskell (`Test.Falderal`).
* The ability to format a Falderal file to different formats, including
  Markdown and Haskell.
* Running tests is now a matter of formatting a Falderal file as a Haskell
  module and running the function `testModule` in that module.

The previous released version of Falderal is 0.2.  It introduced the following
features:

* Added a test harness for Falderal itself, in the form of a simple shell
  script which diffs the output of Test.Falderal.Demo against a text file
  containing the expected content.
* Improved formatting of failure reports.  Multi-line input text or expected
  output is always presented starting on its own line.
* Tests may be organized into groups; the entire group is preceded by some
  literal text, but there is no literal text between the tests in the group.
  When one of these tests fails, the literal text for the group is reported,
  along with the number of the test within the group.
* Fixed a bug where exception text which extended over multiple lines
  could not be expected correctly.

The current version under development is 0.4.  It is introducing the
following features:

* For ease of installation, the project is presented as a Cabal package.
* A driver executable, `falderal`, is built as part of installing the
  Cabal package.  It provides a command-line interface to formatting
  Falderal files and, in a limited fashion, running the tests in them.
* A shell script formatter has been written, enabling testing of shell
  commands.  One caveat is that reporting for these tests is nowhere near
  as nice as for Haskell functions, but that will change in the next version.
* The Functionality-definition pragma has been implemented, making it
  possible to write tests in a more abstract, implementation-independent
  fashion.

The API should not be expected to be stable through the 0.x series.

Development
-----------

Falderal development is hosted on Bitbucket:

    https://bitbucket.org/catseye/falderal/

Bugs may be reported (and features requested) on the Issue Tracker:

    https://bitbucket.org/catseye/falderal/issues

Official release distfiles are available on the [Falderal
project page][] at [Cat's Eye Technologies'][].

For Further Information
-----------------------

Please see the [Falderal wiki][] on Bitbucket.

[Falderal project page]: http://catseye.tc/projects/falderal/
[Cat's Eye Technologies]: http://catseye.tc/
[Falderal wiki]: https://bitbucket.org/catseye/falderal/wiki/
