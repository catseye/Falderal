#!/bin/sh

# A tiny test harness for Falderal itself.
# You'll want to make sure the version you're testing is actually
# installed via Cabal first:
# $ cabal clean && cabal install --prefix=$HOME --user

echo 'Testing formatting...'

falderal format identity eg/LiterateHaskellDemo.lhs >formatted.txt
diff -u eg/LiterateHaskellDemo.lhs formatted.txt
E1=$?
rm -f formatted.txt

echo 'Testing LiterateHaskellDemo...'

cat >expected.txt <<EOF
--------------------------------
Total tests: 17, failures: 5
--------------------------------

FAILED:
This is an intentionally failing test, to demonstrate how Falderal will
present it.

Impl    : Haskell function LiterateHaskellDemo:everySecond
Input   : Something
Expected: Output "Anything"
Actual  : Output "oehn"

FAILED:
Another intentionally failing test to demonstrate how Falderal will
present expecting an exception and not getting one.

Impl    : Haskell function LiterateHaskellDemo:everySecond
Input   : ridiculous
Expected: Exception "Prelude.head: empty list"
Actual  : Output "iiuos"

FAILED:
An intentionally failing test to demonstrate that it is important
to get the formatting of the output right, when testing with show.

Impl    : Haskell function LiterateHaskellDemo:showParseBits
Input   : 01
Expected: Output "[False, True]"
Actual  : Output "[False,True]"

FAILED:
An intentionally failing test to demonstrate show what a failure
looks like on multi-line input.

Impl    : Haskell function LiterateHaskellDemo:showParseBits
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

Impl    : Haskell function LiterateHaskellDemo:showParseBits
Input   : 0000
Expected: Output "[False,False,False,Flse]"
Actual  : Output "[False,False,False,False]"

EOF
cd eg
falderal test LiterateHaskellDemo.lhs >../actual.txt
cd ..
diff -u expected.txt actual.txt
E2=$?
rm -f expected.txt actual.txt

echo 'Testing wc.falderal (multiple impls, var exp)...'

cat >expected.txt <<EOF
--------------------------------
Total tests: 7, failures: 1
--------------------------------

FAILED  : An intentionally failing test.

Impl    : Shell command "wc -w"
Input   : Not four words!
Expected: Output "4"
Actual  : Output "3"

EOF
cd eg
falderal test wc.falderal >../actual.txt
cd ..
diff -u expected.txt actual.txt
EWC=$?
rm -f expected.txt actual.txt

echo 'Testing echo.falderal (test-text var, missing newline)...'

cat >expected.txt <<EOF
--------------------------------
Total tests: 6, failures: 0
--------------------------------

EOF
cd eg
falderal test echo.falderal >../actual.txt
cd ..
diff -u expected.txt actual.txt
EECHO=$?
rm -f expected.txt actual.txt

echo 'Testing Erroneous.falderal...'

cat >expected.txt <<EOF
--------------------------------
Total tests: 3, failures: 2
--------------------------------

NOT RUN : (#2) 

Impl    : Haskell function Erroneous:countLines
Input:
These are eight words
that span two lines.
Expected: Output "2"

NOT RUN : (#3) 

Impl    : Haskell function Erroneous:countLines
Input:
These are eight words
that span
three lines.
Expected: Output "3"

EOF
cd eg
falderal test Erroneous.falderal >../actual.txt 2>/dev/null
cd ..
diff -u expected.txt actual.txt
E3=$?
rm -f expected.txt actual.txt

echo 'Testing functionality definition on command line...'
cat >expected.txt <<EOF
falderal: Can't find Count lines in []
EOF
cd eg
falderal test Underspecified.falderal >../actual.txt 2>&1
cd ..
diff -u expected.txt actual.txt
E4=$?
rm -f expected.txt actual.txt

cat >expected.txt <<EOF
--------------------------------
Total tests: 1, failures: 0
--------------------------------

EOF
cd eg
falderal test --functionality 'Count lines:shell command "wc -l"' Underspecified.falderal >../actual.txt 2>&1
cd ..
diff -u expected.txt actual.txt
E5=$?
rm -f expected.txt actual.txt

if [ $E1 != 0 -o $E2 != 0 -o $E3 != 0 -o $E4 != 0 -o $E5 != 0 -o $EWC != 0 -o $EECHO != 0 ]
  then
    echo "Internal tests failed!"
    exit 1
  else
    echo "All tests passed."
    exit 0
fi
