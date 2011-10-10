Falderal
========

Falderal is a file format for literate test suites.  It is particularly
suited for documenting programming languages (or other specifications of
ways to transform text) and testing their implementation(s) in a
language-agnostic fashion.  The dumbed-down sound-bite version: "doctests
for DSLs".  `Test.Falderal` is the reference implementation, in Haskell,
of tools for formatting and running tests written in Falderal.

Here's the scenario:

* You have a file format, or a language (perhaps a programming language.)
  Like all languages, it establishes a certain set of rules about its
  syntax, semantics, and so forth.
* You have one or more implementations of this language.
* You have some example programs in this language, and some expectations
  about what should happen when they are run (possibly including producing
  an error.)
* You want to be able to present those example programs in a nicely readable
  fashion, perhaps interleaved with some descriptive prose, perhaps
  formatted into a document format such as HTML.
* You want to run those example programs to make sure they do what you
  expect, to find flaws that may be lurking in either those programs, or in
  an implementation of the language.
* You want to be able to embed those example programs and that documentation
  in another source file, perhaps an implementation of the very language
  they describe in literate Haskell.

If this describes you, then Falderal might help.  The particular need I had
that encouraged me to write it is having implemented several esoteric
programming languages in Haskell, and wanting to write nicely formatted
tests suites for them.

History
-------

The current version under development is 0.4.  It introduced the
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

The previous released version of Falderal was 0.3.  It introduced the
following features:

* The definition of a Falderal Literate Test Format, distinct from the
  reference implementation of tools for it in Haskell (`Test.Falderal`).
* The ability to format a Falderal file to different formats, including
  Markdown and Haskell.
* Running tests is now a matter of formatting a Falderal file as a Haskell
  module and running the function `testModule` in that module.

Falderal files written to work with Falderal 0.3 should still work with
Falderal 0.4, but you are encouraged to use the Functionality-definition
pragma introduced in 0.4 to make your tests more implementation-independent.

The API should not be expected to be stable through the 0.x series.

Development
-----------

Falderal development is hosted on Bitbucket:

    https://bitbucket.org/catseye/falderal/

Bugs may be reported (and features requested) on the Issue Tracker:

    https://bitbucket.org/catseye/falderal/issues

Official release distfiles are available on the
[Falderal project page](http://catseye.tc/projects/falderal/) at
[Cat's Eye Technologies'](http://catseye.tc/).

For Further Information
-----------------------

Please see the [Falderal wiki](https://bitbucket.org/catseye/falderal/wiki/)
on Bitbucket.
