`py-falderal`
=============

`py-falderal` is an implementation of Falderal in Python 2.5.x.

Motivation
----------

The original reference implementation of Falderal was written in Haskell.
There are a few reasons I had for re-implementing it in Python:

*   The original Falderal implementation grew out of a Haskell-specific hack,
    and it showed in how it was written.

*   Fewer discrepancies between platforms.  In particular, `ghc` for Windows
    likes to operate in terms of MS-DOS end-of-lines (`CR`, `LF`), but I tend
    to use it under Cygwin using Unix end-of-lines (`LF`).

*   Smaller install burden: Python sometimes comes bundled with the operating
    system; Haskell rarely does.

*   Haskell, being lazy, makes it harder to deal with exceptions; unless the
    Haskell expression is being evaluated both strictly and deeply, exceptions
    can slip by.  Haskell's facilities for forcing an expression to be
    evaluated strictly and deeply are rather hacky (last I checked, the most
    portable way to do so was to use `show`.)  For Falderal's purposes, this
    limitation seems artificial, at best.

*   Relatedly, Python (or CPython, anyway) has better error behavior than
    Haskell (or `ghc`, anyway); when it crashes, it dumps a backtrace (which I
    can then analyze), instead of just saying something pithy like `Prelude:
    empty list` (which I can't.)

Features
--------

`py-falderal` implements a slightly different subset of the
[Falderal Literate Test Format](Falderal_Literate_Test_Format.markdown) than
the Haskell implementation did.

In particular,

*   It mainly implements `shell command` implementations.  In practice, partly
    due to the "strict & deep" evaluation problem mentioned above, that's how
    I've been using Falderal anyway; also, its approach makes it somewhat more
    useful for "end-to-end" testing of compilers and interpreters, than for
    unit-testing individual text-processing functions inside a program.
    (Technically, it implements `Haskell function` implementations too, but it's
    something of a hack that uses `ghc -e`.)

*   I plan for it to *only* understand indented Falderal blocks.  The idea is
    that the Falderal tests will almost certainly be embedded in a Markdown
    document (possibly with Bird-style literate code also embedded therein,)
    and no extra processing should be required to format something readable
    from that Markdown.  The four-space-indented Falderal blocks come out as
    preformatted blocks, which is quite good enough.  Relatedly,

*   It does no formatting.  There are a set of classes for parsing Falderal
    files (`Document`s) into blocks (`Block`s), and methods for formatting
    blocks into text.  But aside from reporting the results of a test run,
    they're not used by the utility to format Falderal files.  If you want to
    do extra processing on your Falderal/Markdown file so that Falderal
    blocks are prettier, you certainly can do that, but you'll need to write
    your own script which uses these classes, for it is outside the scope of
    `py-falderal`.

*   I'm not so sure about `-> Functionality "blah" is implemented by shell
    command "flargh"` anymore.  I mean, it should certainly be the case that
    functionalities can have multiple implementations, but
    
    *   Perhaps implementations should not be specified in Falderal documents
        at all — that approach is more abstract.  But it requires them to be
        specified on the tool's command line, which in practice requires there
        to be a driver script to run the tests, for a particular implementation;
        but this is not necessarily a bad thing.  (The effective practice has
        been, actually, to write out a bit of "configuration Markdown" with
        that "Functionality is implemented by" snippet in it, and to load that
        before loading the main tests.  Rather than to configure the functionality
        implementation(s) on the command line itself.)
    *   When `falderal` is run on more than one input file, what is the scope
        of a functionality, and what is the scope of the implementations of a
        functionality?  Currently, in the Falderal semantics, that scope is
        global, and maybe that is appropriate; but maybe finer-grained control
        would be nice.

*   `py-falderal` also does not try to optimize the test runs into a block of
    test runs (which didn't work out so well in the Haskell implementation,
    for `shell command`s, anyway.)
