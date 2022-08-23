`py-falderal`
=============

`py-falderal` is an implementation of Falderal in Python 3.x.  It should
also function under Python 2.7.x, but as 2.7.x is a legacy version of Python,
such support should not be relied upon.

`py-falderal` implements the
[Falderal Literate Test Format](Falderal%20Literate%20Test%20Format.md) as
a runner of the tests embedded in a Falderal Test Suite.

`py-falderal` accepts multiple input files.  The Functionality-definition
pragmas in these files apply to all the Tests-for pragmas in all of the
files.  This permits a somewhat modular usage, where the inclusion (or
omission) of a file from the list of files on the command line, can
determine what definitions of a functionality will be tested (or not tested).

Historically, the first implementation of the Falderal Literate Test Format
was in Haskell.  It followed an early version of the Falderal Literate Test Format
which looked somewhat different than it does today (at version 0.14).  This
document described some of the differences between `py-falderal` and that
original implementation.  But since much of that information is merely of
historical interest, it has been stripped from this document.  It is still
available in version control history and/or archived distfiles of previous
versions of the Falderal distribution.
