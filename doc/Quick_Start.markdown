Quick Start
===========

First, install `Test.Falderal`.

    % hg clone https://bitbucket.org/catseye/falderal/ -r rel_0_4
    % cd falderal
    % cabal install --prefix=$HOME --user

Define a programming language, or some other file format -- basically,
anything you can model as a function which takes strings to strings.  In
Falderal terminology, this is a "functionality".  Implement your
functionality in any programming language for which you can produce
executables for your system.  (If you implement it in Haskell, you get
some side benefits, but it's not necessary.)

Often, depending on the syntax of your implementation language, you can
place your literate tests in the same file as your code.  We'll use
Bird-style literate Haskell in this example.

    module Gobvert

    This is some kind of really trivial little language.

    > gobvert "A" = "Z"
    > gobvert "Z" = "A"

Then give your functionality a name, and write some tests for your
functionality.  You use a Falderal pragma to identify which functionality
these tests are for.

    -> Functionality "Gobvert a string" is implemented by
    -> Haskell function Gobvert:gobvert

    -> Tests for functionality "Gobvert a string"

    The gobversion of A is Z.

    | A
    = Z

    The gobversion of Z is A.

    | Z
    = A

    The gobversions of other letters are not defined.

    | Q
    ? Not matched

Then, use the `falderal` tool to run these tests:

    % falderal test Gobvert.lhs

All failures will be listed in a nicely-formatted report, including the
literate description that appears before each failing test.

You can also use the `falderal` tool to format your literate Haskell
file, including embedded tests, to a document format such as Markdown:

    % falderal format markdown Gobvert.lhs >Gobvert.markdown
