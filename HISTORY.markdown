History
=======

Version 0.10-PRE "Shedd Aquarium" (current development version):

*   Removed the old, bitrotting Haskell implementation.
*   Removed deprecated `%(test-text)` and `%(test-file)` variables
    (`%(test-body-text)` and `%(test-body-file)` should be used now.)

Version 0.9-2014.0525 "Municipal Pier #2, May 25th 2014" (current released
version):

*   Shell implementations assume input and output is encoded UTF-8; any
    malformed UTF-8 is simply ignored (for better or worse, for now.)
    Falderal documents are assumed to be encoded in UTF-8; this includes
    test bodies, expectations, etc.  Internally, in the reference
    implementation, all strings are stored as Unicode.

Version 0.9-2014.0427 "Municipal Pier #2" (previous released version):

*   For a test of a shell command implementation that expects a failure,
    if no output was found on standard error, the output of standard output
    is checked against the failure expectation instead.  The primary use case
    for this is Hugs, which (unlike `ghc`) displays error messages on stdout
    rather than stderr.  "Municipal Pier #2" was the original name of Navy Pier.

Version 0.9 "Navy Pier":

* `py-falderal` now insists that blocks are indented four spaces, as the
  specification says.  It ignores blocks that are not indented.
* In an error report `py-falderal` reports the location (filename and line
  number of the beginning of the test text) of each test that failed, as
  well as the name of the functionality that was being tested.
* Several internal renamings of class and variable names for greater clarity.
* Trying to standardize on the terminology "test body" for the body of a
  test (as opposed to its "input", and opposed to it being passed to a shell
  command line "as text".)
* Support for specifying a block of text to be used as input for a test.
* Dropped support for "implemented by Haskell function" backwards-compatibility.
* Temporary files are no longer always created in the current directory.  That
  was a workaround to support `ghc` from Haskell Platform under Cygwin.  They
  are now created where the system wants them to be created (but you can set
  the `TMP` environment variable to "." if you really want them to be created
  in the current directory.)
* `py-falderal` now complains and stops early if there were no documents
  specified, no functionality definitions were found in any of the documents,
  no implementations were found for some defined functionality, or if no
  tests were found in any of the documents, unless `--cavalier` is passed on
  the command line.
* In `--verbose` mode, `py-falderal` will issue warnings about any test that
  has been run twice (i.e. more than run on identical test body text and
  implementation.)
* If `py-falderal` catches a `KeyboardInterrupt` exception while running tests,
  it shows a report of all the tests that it did manage to run until that
  point, and a warning message to the effect that not all tests were run.

Version 0.8 "Ukrainian Village":

* The reference implementation is now `py-falderal`.
* Blocks *must* now be indented four spaces.  That Falderal test documents
  will be embedded in Markdown is generally assumed (although not strictly
  required) and they generally have the file extension `.markdown` so that
  other software (e.g. Bitbucket and Github) knows how to pretty-display
  them.
* Various documentation which was on Bitbucket (wiki, issues) and Github
  (py-falderal issues) have been moved into Markdown documents in the
  distribution, which have themselves been re-organized.
* Added the `-v`/`--verbose` flag and made it and the `-d`/`--dump` flag
  do what you would expect from them.
* The `-f`/`--functionalities` and `-c`/`--clear-functionalities` flags
  have been *removed*.  The rationale is that implementations of
  functionalities are clumsy to express on the command line (and anyway
  the syntax would have to be different) while it is not difficult to
  create a "configuration" Falderal file which specifies *only* the
  desired implementations of the desired functionalities, and to load
  it just before loading a "tests" Falderal file which specified the
  functionalities being tested, but not their implementations.

Version 0.7 "Ogden Avenue":

* **Note that this is the final version of Falderal where `Test.Falderal`
  is the reference implementation.**  In subsequent versions,
  `py-falderal` (written in Python) will be the reference implementation.
  `Test.Falderal` will still be in this repository (under the `impl`
  directory) but will no longer be definitive, and will go essentially
  unmaintained.
* Fixed a bug where number of lines in the intermediary results file
  was sometimes being counted incorrect.
* Slightly improved support for running under Cygwin.
* Tests that are implemented by a shell command are now run directly
  from the `falderal` process (instead of creating an intermediary
  shell script and running it.)  This resulted in better performance.
* Addition of `-b` command-line option, which considers a test to
  have passed if the expected exception message is a substring (rather
  than an exact match) of the actual produced exception message.
  TODO: write test for this.
* Blocks may now be indented four spaces, to allow them to be
  embedded directly in Markdown files as pre-formatted text.
* A script to just build the binary, without installing it, was added.

Version 0.6 "Streeterville":

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

Falderal started life as a Haskell-specific hack that could be embedded
in a Bird-style Literate Haskell source file.  I took a framework for
literate tests I had already written in a project called _Rho_, and used it
as the basis of this code.

I wrote it because I was designing yet another esoteric programming
language, and while working on it I realized I was rebuilding yet another
ad-hoc unit test suite, like I had done a half-dozen times before.  I didn't
want to keep doing this for every language I designed, and I realized that
literate test suites could serve as documentation as well; the result was
Falderal.
