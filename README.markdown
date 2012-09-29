Falderal
========

Falderal is a file format for literate test suites.  It is particularly
suited for documenting programming languages (or other specifications of
ways to transform text) and testing their implementation(s) in a
language-agnostic fashion.  The dumbed-down sound-bite version: "doctests
for DSLs".  `Test.Falderal` is the reference implementation, in Haskell,
of tools for formatting and running tests written in Falderal.

Motivation
----------

Here's the scenario:

* You have a file format, or a language (perhaps a programming language.)
  Like all languages, it establishes a certain set of rules about its
  syntax, semantics, and so forth.
* You have one or more implementations of this language.
* You have some example programs in this language, and some expectations
  about what should happen when they are run (possibly including producing
  an error.)
* You want to be able to present those example programs in a nicely readable
  fashion, interleaved with some descriptive prose, perhaps ultimately
  formatted into a document format such as HTML.  (This is certainly a
  reasonable way to document a language; most people are good at learning
  from examples.)
* You want to run those example programs to make sure they do what you
  expect, to find flaws that may be lurking in either those programs, or in
  an implementation of the language.
* You want the option of embedding those example programs and that
  documentation in a source code file -- perhaps a literate implementation
  of the very language they are testing.

If this describes you, then Falderal might help.  I wrote it because I was
designing yet another esoteric programming language, and while working on it
I realized I was rebuilding yet another ad-hoc unit test suite, like I had
done a half-dozen times before.  I didn't want to keep doing this for every
language I designed, and I realized that literate test suites could serve as
documentation as well; the result was Falderal.

Features
--------

The current version of Falderal is described in the [Falderal Literate Test
Format](docs/Falderal_Literate_Test_Format.markdown).  The current version
of `Test.Falderal` is 0.7 "Ogden Avenue".  Neither the file format
specification, nor the `Test.Falderal` API, should be expected to be stable
through the 0.x series.

Currently supported features of the framework are:

* Writing literate test suites.  These may be embedded in other kinds of
  textual documents, such as Markdown, literate Haskell source code, and so
  forth.
* Formatting these test suites (i.e. converting them to other file formats,
  such as Markdown).
* Running these test suites.  Running is accomplished by formatting the tests
  to an executable format (such as a Haskell source file), running that,
  and collecting the results from it.  This sequence of steps is done
  automatically with the `falderal test` command.
* Producing readable failure reports.  Each test or group of test may be
  preceded by descriptive text, and this will be displayed above every
  failing test, along with the expected and actual output.
* Testing text-processing functionalities.  A test specifies textual input
  to the function, and may expect a particular textual output, or that a
  particular error was encountered.  Functionalities are abstract: each
  functionality defined in a Falderal file can be implemented in multiple
  ways.  Thus the same tests can be run multiple times, once for each
  implementation of the functionality they test.
* Specifying that a functionality is implemented by a Haskell function of
  type `String -> String`.
* Specifying that a functionality is implemented by a shell command.  The
  shell command can invoke arbitrary executables, allowing you to test
  implementations of your language written in essentially any language.

Development
-----------

Falderal development is
[hosted on Bitbucket](https://bitbucket.org/catseye/falderal/).
Bugs may be reported (and features requested) on the
[Issue Tracker](https://bitbucket.org/catseye/falderal/issues) there.

There is also a
[git mirror of the repository on Github](https://github.com/catseye/Falderal).

Official release distfiles are available on the
[Falderal project page](http://catseye.tc/projects/falderal/) at
[Cat's Eye Technologies](http://catseye.tc/).

Contents of the Wiki
--------------------

(This will be rewritten at some point)

### Falderal ###

Welcome to the Falderal wiki!  We've pared it down recently, as there's no
sense in maintaining multiple copies of the documentation.

### Installation ###

The current released version of `Test.Falderal` is **0.7 "Ogden Avenue"**.
To install:

    hg clone https://bitbucket.org/catseye/falderal/

Then put `falderal/bin` on your `$PATH`.

### Documentation ###

* To quickly get started using Faderal, see the file `Quick_Start.markdown`
  in the `doc` directory.
* For a description of the Falderal Literate Test Format, see the file
  `Falderal_Literate_Test_Format.markdown` in the `doc` directory.
* For some insight into how `Test.Falderal` implements Falderal, see the
  file `Theory_of_Operation.markdown`.

### Projects using Falderal ###

Actually, I'm sure this information can be extracted from Chrysoberyl
somehow, so TODO just link to that here.

* [[http://catseye.tc/projects/flobnar/|The Flobnar Programming Language]] (Haskell)
* [[http://catseye.tc/projects/iphi/|The Iphigeneia Programming Language]] (Haskell)
* [[http://catseye.tc/projects/madison/|The Madison Proof-Checking Language]] (TDLD)
* [[http://catseye.tc/projects/pail/|The Pail Programming Language]] (Haskell)
* [[https://bitbucket.org/catseye/pixley/|The Pixley Programming Language]] (Pixley, Scheme)
* [[https://bitbucket.org/catseye/pl-goto-.net/|PL-{GOTO}.NET]] (Haskell)
* [[https://bitbucket.org/catseye/robin/|The Robin Programming Language]] (Haskell)
* [[http://catseye.tc/projects/quylthulg/|The Quylthulg Programming Language]] (Haskell)
* [[http://catseye.tc/projects/xoomonk/|The Xoomonk Programming Language]] (TDLD)

Pail, Xoomonk, and Madison are better examples of how a literate test suite can be useful in both describing a programming language through examples and testing that an implementation of the language does not violate the language specification.  Xoomonk and Madison, in fact, are experiments in //test-driven language design// (TDLD), where the tests were written as part of designing the language, before any attempt at implementation; the others are more like traditional test suites.
