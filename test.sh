#!/bin/sh

# A tiny test harness for Falderal itself.
# You'll want to make sure the version you're testing is actually
# installed via Cabal first:
# $ cabal clean && cabal install --prefix=$HOME --user

falderal format identity eg/LiterateHaskellDemo.lhs >formatted.txt
diff -u eg/LiterateHaskellDemo.lhs formatted.txt
E1=$?
rm -f formatted.txt

cat >expected.txt <<EOF
--------------------------------
Total tests: 17, failures: 5
--------------------------------

FAILED:
This is an intentionally failing test, to demonstrate how Falderal will
present it.

Input   : Something
Expected: Output "Anything"
Actual  : Output "oehn"

FAILED:
Another intentionally failing test to demonstrate how Falderal will
present expecting an exception and not getting one.

Input   : ridiculous
Expected: Exception "Prelude.head: empty list"
Actual  : Output "iiuos"

FAILED:
An intentionally failing test to demonstrate that it is important
to get the formatting of the output right, when testing with show.

Input   : 01
Expected: Output "[False, True]"
Actual  : Output "[False,True]"

FAILED:
An intentionally failing test to demonstrate show what a failure
looks like on multi-line input.

Input:
01
10
Expected: Output "[False,False,True,True]"
Actual  : Output "[False,True,True,False]"

FAILED:
(#3) If we have a set of tests where the tests after the first one have no
descriptions of their own, we can take this to suggest they are all
testing the same thing.  In this case, the literal text that is displayed
when any of them fails is the text that comes before the first of them,
annotated with the number of the test in the set that failed.  The
intentionally-failing third test below demonstrates this.

Input   : 0000
Expected: Output "[False,False,False,Flse]"
Actual  : Output "[False,False,False,False]"

EOF
cd eg
falderal test LiterateHaskellDemo.lhs >../actual.txt
E2=$?
cd ..
rm -f expected.txt actual.txt

if [ $E1 != 0 -o $E2 != 0 ]
  then
    exit 1
  else
    exit 0
fi
