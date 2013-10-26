Falderal
========

Version 0.8-pre "Ukranian Village"

This is the reference distribution of Falderal, a file format for literate
test suites.  Falderal is particularly suited for:

*   documenting programming languages with prose and examples
*   testing multiple implementations of a programming language
*   performing Test-Driven Language Design (TDLD)
*   being embedded in Markdown documents

Falderal in three words: "Doctests for DSLs".

Motivation
----------

Say you have a programming language.  You probably have a document describing
the language.  That document probably includes examples.  Those examples are
probably intended to produce some kind of output.  That output is probably
expected to be a certain, predicted thing, and if it's not that thing, the
implementation is probably considered incorrect

So why not write those examples in a format that can be tested?

You could write a bunch of standalone test sources, and store the output you
expect from them in a bunch of other files, and write a shell script that runs
each program and `diff`s the output with the expected output.  But this is a
lot of clutter — finding a particular example might not be so easy.  Each
test source exists in a void, not necessarily logically grouped with other,
similar tests.  And any text you write describing a test needs to be in the
comment syntax of your programming language (if your programming language
supports comments) and is also detached from all the other test descriptions.

You could write doctests, but if your language isn't implemented in Python
it's awkward, and there can be awkward quoting issues with how you embed your
test sources inside that big Python string.

You could write unit tests in the unit test framework of your choice, but
if your programming language has more than one implementation one day (and
you should really consider that possibility) then you might not be able to
re-use it so easily for other implementations in other languages.

Or... you could write a Markdown document with beautiful yet precise prose
describing your wonderful language, alternating with example code (in the
form of embedded Falderal tests) clarifying each of the points you are
making; then you could use a Falderal-speaking tool to run each of these tests
against any implementation of your language which exists or will exist in
the future.

*And* you could even write this document *before* you even start implementing
your language; then when it is all clear "on paper", you have a target at
which you can aim while writing your language.  As you implement more and more
of it, more and more tests in your test suite will pass.  This is the essence
of Test-Driven Language Design (TDLD).

Features of the Format
----------------------

Falderal is just a file format; it does not specify exactly what tools must
do with the tests they extract.  However, it is generally assumed that most
tools will want to, at the very least,

*   Run tests from one or more documents.
*   Report the results, with some given level of detail.

Each Falderal test is for some abstract _functionality_, and each
functionality may have multiple concrete _implementations_.  Thus the same
tests can be run multiple times, once for each implementation of the
functionality they test.

Directives in the Falderal document may assign functionalities to tests,
and may define implementations for given functionalities.  Implementations
may be defined outside of any document, as well.  Falderal defines one kind
of implementation, implementation by Bourne shell command, but is not
inherently restricted from supporting other kinds of implementations.

Inherent Limitations
--------------------

Being tests, rather than proofs, Falderal tests cannot tell you that your
implementation of a language is correct.  If one or more tests fail, that's
an indication that your implementation is not correct; but even if all tests
pass, you have no guarantee that the implementation doesn't do something
contrary to the spec in one of the infinite number of cases that you have not
enumerated.

There is also no way to test that certain programs represent infinite loops,
for reasons that should be obvious.

Contents of this Distribution
-----------------------------

This distribution contains:

*   `doc` — contains documents about Falderal.  For the specification of
    the file format, see `doc/Falderal_Literate_Test_Format.markdown`.
    (Note that this specification should not be expected to remain stable
    through the 0.x version series.)  There are other documents in there too.
*   `bin/falderal` — the reference implementation of Falderal, written in
    Python and sometimes referred to as "py-falderal".  It imports the
    sources in `src/falderal`.  You don't need to install it; just add
    the `bin` directory of this distribution to your `$PATH`.  This
    implementation is (somewhat) documented in `doc/py-falderal.markdown`.
*   `impl/Test.Falderal` — a (lagging, and not conformant) implementation of
    Falderal in Haskell.
*   `tests` — a set of tests for Falderal itself.  (Note that these are not
    written in Falderal, as that would just be too confusing.)
*   `HISTORY.markdown` — changelog for releases of Falderal.
*   `TODO.markdown` — areas where Falderal and its implementations could be
    improved.

Development
-----------

Falderal development is
[hosted on Bitbucket](https://bitbucket.org/catseye/falderal/) with a
[git mirror of the repository on Github](https://github.com/catseye/Falderal).

Official release distfiles are available on the
[Falderal project page](http://catseye.tc/node/Falderal) at
[Cat's Eye Technologies](http://catseye.tc/).

Projects using Falderal
-----------------------

(NOTE Actually, I'm sure this information can be extracted from Chrysoberyl
somehow, so in the future, just link to that here.)

Exanoke, Flobnar, Hev, Iphigeneia, Madison, Pail, Pixley, PL-{GOTO}.NET, Robin,
Quylthulg, Velo, and Xoomonk.

Xoomonk, Madison, Velo, and Exanoke are good examples of how a literate
test suite can be useful in both describing a programming language through
examples and testing that an implementation of the language does not violate
the language specification.

Xoomonk, Madison, Velo, and Exanoke are, in fact, exercises in Test-Driven
Language Design (TDLD), where the tests were written as part of designing the
language, before any attempt at implementation; the others are more like
traditional test suites, written after-the-fact.
