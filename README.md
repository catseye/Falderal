Falderal
========

Version 0.14 "Jean Baptiste Point DuSable Lake Shore Drive"

This is the reference distribution of Falderal, a file format for literate
test suites.  What sets Falderal apart from most other test frameworks is
that it recognizes that **your tests are for a functionality, not a particular
implementation of that functionality**.

Falderal is particularly suited for:

*   documenting programming languages with prose and examples
*   testing multiple implementations of a programming language
*   performing Test-Driven Language Design (TDLD)
*   being embedded in Markdown documents

Falderal in three words: "Doctests for DSLs".

If you're more interested in running the tools than learning about the format
itself, skip down to [Implementation](#implementation).

Motivation
----------

Say you have a programming language.  You probably have a document describing
the language.  That document probably includes examples.  Those examples are
probably intended to produce some kind of output.  That output is probably
expected to be a certain, predicted thing, and if it's not that thing, the
implementation is probably considered incorrect.

So why not write those examples in a format that can be run and tested?

You could write a bunch of standalone test sources, and store the output you
expect from them in a bunch of other files, and write a shell script that runs
each program and `diff`s the output with the expected output.  But this is a
lot of clutter — finding a particular example might not be so easy.  Each
test source exists in a void, not necessarily logically grouped with other,
similar tests.  And any text you write describing a test needs to be in the
comment syntax of your programming language (if your programming language
supports comments) and is also detached from all the other test descriptions.

You could write unit tests in the unit test framework of your choice, but
if your programming language has more than one implementation one day (and
you should really consider that possibility) then you might not be able to
re-use it so easily for other implementations in other languages.

In a language like Python, you could write doctests, but that also ties your
tests to one implementation of your language.  There can be awkward
quoting issues with how you embed your test sources inside those embedded
strings that comprise your doctests, as well.

Or... you could write a Markdown document with beautiful yet precise prose
describing your wonderful language, alternating with example code (in the
form of embedded Falderal tests) clarifying each of the points you are
making; then you could use a Falderal-comprehending tool to run each of these
tests against any implementation of your language which exists or will exist
in the future.

*And* you could even write this document *before* you even start implementing
your language; then when it is all clear "on paper", you have a target at
which you can aim while writing your language.  As you implement more and more
of it, more and more tests in your test suite will pass.  This is simply the
idea behind Test-Driven Development (TDD) applied to language design, which we
will call Test-Driven Language Design (TDLD).

Features of the Format
----------------------

Falderal is just a file format; it does not specify exactly what tools must
do with the tests they extract.  However, it is generally assumed that most
tools will want to, at the very least,

*   Run tests from one or more documents.
*   Report the results, with some given level of detail.

There is, of course, a reference implementation which does both of these
things.  It is called `falderal` and it is written in Python 3.x.  More
about it can be read in the [`falderal` document](doc/falderal.md).

Each Falderal test is for some abstract _functionality_, and each
functionality may have multiple concrete _implementations_.  Thus the same
tests can be run multiple times, once for each implementation of the
functionality they test.

Directives in the Falderal document may assign functionalities to tests,
and may define implementations for given functionalities.  Implementations
may be defined outside of any document, as well.  Falderal defines one
general kind of implementation, implementation by Bourne shell command, but
is not inherently restricted from supporting other kinds of implementations.

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
    the file format, see
    [`doc/Falderal Literate Test Format.md`](doc/Falderal%20Literate%20Test%20Format.md).
    (Note that this specification should not be expected to remain stable
    through the 0.x version series.)  There are other documents in there too.
*   `bin/falderal` — the reference implementation of Falderal.
    See "Implementation", below, for details.
*   `script` — miscellaneous small tools intended to be used in tests.
*   `src` — source code for the `falderal` tool.
*   `tests` — a set of tests for Falderal itself.  (Note that these are not
    written in Falderal, as that would just be too confusing.)
*   `HISTORY.md` — changelog for releases of Falderal.
*   `TODO.md` — areas where Falderal and its implementations could be
    improved.

Implementation
--------------

This distribution contains `falderal`, which is the reference implementation
of Falderal, written in Python and sometimes referred to as "py-falderal".

To use it, you can clone this repository and run it as `bin/falderal`
from the directory of your clone, or you can put the `bin` directory
on your executable search path, and run it as `falderal` anywhere.

Or you can install it using `pip`:

    pip install -e Falderal==0.14

(Depending on your needs, you may wish to establish a virtual environment
first.  Describing how to do so is outside the scope of this document.)

The implementation is (somewhat) documented in
[`doc/falderal.md`](doc/falderal.md).

The implementation has its own test suite (not written in Falderal, as
that might be too confusing).  It can be run by running `./test.sh`.

Development
-----------

The git repository for the Falderal distribution can be found on GitHub at
[https://github.com/catseye/Falderal](https://github.com/catseye/Falderal).

Official release distfiles are available via the
[Falderal project page](http://catseye.tc/node/Falderal) at
[Cat's Eye Technologies](http://catseye.tc/).

Projects using Falderal
-----------------------

*   [ALPACA](https://catseye.tc/node/ALPACA)
*   [Castile](https://catseye.tc/node/Castile)
*   [Equipage](https://catseye.tc/node/Equipage)
*   [Exanoke](https://catseye.tc/node/Exanoke)
*   [Flobnar](https://catseye.tc/node/Flobnar)
*   [Hev](https://catseye.tc/node/Hev)
*   [hatoucan](https://catseye.tc/node/hatoucan)
*   [Iphigeneia](https://catseye.tc/node/Iphigeneia)
*   [Madison](https://catseye.tc/node/Madison)
*   [Nhohnhehr](https://catseye.tc/node/Nhohnhehr)
*   [Pail](https://catseye.tc/node/Pail)
*   [Pixley](https://catseye.tc/node/Pixley)
*   [PL-{GOTO}.NET](https://catseye.tc/node/PL-{GOTO}.NET)
*   [Quylthulg](https://catseye.tc/node/Quylthulg)
*   [Robin](https://catseye.tc/node/Robin)
*   [Samovar](https://catseye.tc/node/Samovar)
*   [SixtyPical](https://catseye.tc/node/SixtyPical)
*   [Tamsin](https://catseye.tc/node/Tamsin)
*   [Velo](https://catseye.tc/node/Velo)
*   [Xoomonk](https://catseye.tc/node/Xoomonk)
*   [Yolk](https://catseye.tc/node/Yolk)
*   [ZOWIE](https://catseye.tc/node/ZOWIE)

Xoomonk, Madison, Velo, and Exanoke are good examples of how a literate
test suite can be useful in both describing a programming language through
examples and testing that an implementation of the language does not violate
the language specification.  They are, in fact, exercises in Test-Driven
Language Design (TDLD), where the tests were written as part of designing the
language, before any attempt at implementation; the others are more like
traditional test suites, written after-the-fact.
