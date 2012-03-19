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
of `Test.Falderal` is 0.6 "Streeterville".  Neither the file format
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

History
-------

Version 0.7 "Ogden Avenue" (current version under development):

* Addition of `-b` command-line option, which considers a test to
  have passed if the expected exception message is a substring (rather
  than an exact match) of the actual produced exception message.
  TODO: write test for this.

Version 0.6 "Streeterville" (current released version):

* Variables may be included in the specification of a shell command
  implementation; these will be expanded before generating the results
  generator.  These variable include `%(test-text)`, `%(test-file)`,
  and `%(output-file)`.
* `falderal` now lets the user specify, on the command line, the
  implementations for a named functionality.  Supplying `-f 'foo:shell
  command "foo.sh %(test-file)"'` has the same effect as including the
  pragma `Functionality "foo" is implemented by shell command "foo.sh
  %(test-file)"` in the Falderal file.  Note that this is in addition
  to the Functionality-definition pragmas given in the Falderal file;
  to replace them, you must first clear the ones with that name from
  the file by supplying `-c foo` on the command line.
* `falderal` also allows tests for named functionalities to be skipped
  completely, by passing the name of the functionality to be skipped
  after a `-k` flag on the command line.
* The Markdown formatter now formats Bird-style embedded code with
  HTML embedded in the Markdown document.  This is so that it can
  be styled independently from, and thus distinguished from, any plain
  Markdown indented code blocks which may appear in the literate portion
  of the source code.
* In failure reports, the implementation of the functionality of the test
  that failed is now reported in each failure.
* A race condition(?) that could occur when testing multiple implementations
  of a functionality, of different kinds (Haskell and shell), has been
  prevented.  Both tests were writing to `results.txt` and immediately
  deleting it, and this would sometimes confuse `falderal` into thinking
  one had produced no results (perhaps a result of some creative
  scheduling by `ghc`, although really, I haven't a clue.)  Results are
  now written to different temporary files with different, generated
  names.
* Previously, if the output of a shell command being tested did not end
  with a newline, the intermediate results file was not being generated
  correctly, resulting in failures being misreported.  This has been
  recitified.
* Previously, if there were tests given in a Falderal file before any
  Tests-for pragma was specified, those tests would just be ignored.
  An error message is now issued, and no testing takes place.
* Previously, if multiple Falderal files were given on the command
  line, they were simply concatenated when loaded, the result being
  that Functionality-definitions from the first file were visible in
  the second file, and that any Tests-for in effect at the end of the
  first file would be in effect at the start of the second file.
  Files are now loaded and processed seperately.

Version 0.5 "The Loop":

* The command-line format of `falderal` has changed, for the better.
  The `test` subcommand no longer requires that the failure reporting
  style be specified; instead, it defaults to `standard`, and can be
  changed with a command-line option.  There are also command-line
  options for selecting the programs to run results generators, and to
  keep generated files around instead of cleaning them up after testing.
* The dependency on `ghc` for running Haskell results generators has been
  removed; these can be run by `runhaskell` now, and are run by
  `runhaskell` by default.
* Failure reporting is now consistent across languages; both Haskell and
  Bourne shell results generators generate an intermediate format, which
  `falderal` digests.
* A new pragma `encoding:` was added, so that this directive can be
  embedded in your Falderal document (for the benefit of your text editor)
  without necessarily appearing in the formatted document.
* We began giving release milestones colorful names.  The naming
  convention is to choose names of Chicagoland neigborhoods, suburbs,
  landmarks, and institutions.  Version 0.5 was named after The Loop in
  recognition of its ability to shuttle test results between `falderal`
  and the various results generators implemented in different languages.
  Previous versions of `Test.Falderal` were retroactively given milestone
  names during this release.

Version 0.4 "Blackstone Hotel":

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
* Falderal files written to work with Falderal 0.3 should still work with
  Falderal 0.4, but you are encouraged to use the Functionality-definition
  pragma introduced in 0.4 to make your tests more implementation-independent.

Version 0.3 "Chicago Board of Trade":

* The definition of a Falderal Literate Test Format, distinct from the
  reference implementation of tools for it in Haskell (`Test.Falderal`).
  This represented a fairly substantial departure from how previous versions
  of Falderal worked.
* The ability to format a Falderal file to different formats, including
  Markdown and Haskell.
* Running tests is now a matter of formatting a Falderal file as a Haskell
  module and running the function `testModule` in that module.

Version 0.2 "Dearborn Station":

* Added a test harness for Falderal itself, in the form of a simple shell
  script which diffs the output of `Test.Falderal.Demo` against a text file
  containing the expected content.
* Improved formatting of failure reports.  Multi-line input text or expected
  output is always presented starting on its own line.
* Tests may be organized into groups; the entire group is preceded by some
  literal text, but there is no literal text between the tests in the group.
  When one of these tests fails, the literal text for the group is reported,
  along with the number of the test within the group.
* Fixed a bug where exception text which extended over multiple lines
  could not be expected correctly.

Version 0.1 "Haymarket Square":

* Provision of a framework for writing and running literate tests which may
  be embedded in literate Haskell source code.
* Testing Haskell functions of type `String -> String`.  A test specifies input
  to the function, and may expect a particular output, or that a particular
  exception is thrown.
* Through simple adapters, testing functions of other types such as
  `(Show a) => String -> a`.

Prehistory:

* Falderal started life as a Haskell-specific hack that could be embedded
  in a Bird-style Literate Haskell source file.  I took a framework for
  literate tests I had already written in a project called _Rho_, and used it
  as the basis of this code.

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

For Further Information
-----------------------

Please see the [Falderal wiki](https://bitbucket.org/catseye/falderal/wiki/)
on Bitbucket.
