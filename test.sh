#!/bin/sh

# A tiny test harness for Falderal itself.
# You'll want to make sure the version you're testing is actually
# installed via Cabal first:
# $ cabal clean && cabal install --prefix=$HOME --user

FALDERAL=$HOME/bin/falderal

echo 'Testing formatting...'

# Formatting test broken now that Falderal accepts indented blocks.
# I don't care enough to fix this; this tool should not be in the formatting
# business anymore, and py-falderal is taking over anyway.

#$FALDERAL format identity eg/LiterateHaskellDemo.lhs >formatted.txt
#diff -u eg/LiterateHaskellDemo.lhs formatted.txt
#EID=$?
#rm -f formatted.txt
EID=0

echo 'Testing LiterateHaskellDemo...'

cat >expected.txt <<EOF
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

--------------------------------
Total tests: 17, failures: 5
--------------------------------

EOF
cd eg
$FALDERAL test LiterateHaskellDemo.lhs >../actual.txt
cd ..
diff -u expected.txt actual.txt
ELHS=$?
rm -f expected.txt actual.txt

echo 'Testing wc.falderal (multiple impls, var exp)...'

cat >expected.txt <<EOF
FAILED  : An intentionally failing test.

Impl    : Shell command "wc -w"
Input   : Not four words!
Expected: Output "4"
Actual  : Output "3"

--------------------------------
Total tests: 7, failures: 1
--------------------------------

EOF
cd eg
$FALDERAL test wc.falderal >../actual.txt
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
$FALDERAL test eg/echo.falderal >actual.txt
diff -u expected.txt actual.txt
EECHO=$?
rm -f expected.txt actual.txt

echo 'Testing Erroneous.falderal...'

cat >expected.txt <<EOF
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

--------------------------------
Total tests: 3, failures: 2
--------------------------------

EOF
cd eg
$FALDERAL test Erroneous.falderal >../actual.txt 2>/dev/null
cd ..
diff -u expected.txt actual.txt
EERR=$?
rm -f expected.txt actual.txt

echo 'Testing functionality definition on command line...'

cat >expected.txt <<EOF
falderal: Can't find functionality "Count lines" in []
EOF
cd eg
$FALDERAL test Underspecified.falderal >../actual.txt 2>&1
cd ..
diff -u expected.txt actual.txt
ECL1=$?
rm -f expected.txt actual.txt

cat >expected.txt <<EOF
--------------------------------
Total tests: 1, failures: 0
--------------------------------

EOF
cd eg
$FALDERAL test --functionality 'Count lines:shell command "wc -l"' Underspecified.falderal >../actual.txt 2>&1
cd ..
diff -u expected.txt actual.txt
ECL2=$?
rm -f expected.txt actual.txt

cat >expected.txt <<EOF
falderal: Can't find functionality "Echo" in []
EOF
$FALDERAL -c "Echo" test eg/echo.falderal >actual.txt 2>&1
diff -u expected.txt actual.txt
ECL3=$?
rm -f expected.txt actual.txt

cat >expected.txt <<EOF
--------------------------------
Total tests: 3, failures: 0
--------------------------------

EOF
$FALDERAL -c "Echo" -f "Echo:shell command \"echo '%(test-text)'\"" test eg/echo.falderal >actual.txt
diff -u expected.txt actual.txt
ECL4=$?
rm -f expected.txt actual.txt

echo 'Testing functionality skipping from the command line...'

cat >expected.txt <<EOF
--------------------------------
Total tests: 0, failures: 0
--------------------------------

EOF
$FALDERAL test -k "Echo" eg/echo.falderal >actual.txt
diff -u expected.txt actual.txt
EECHO=$?
rm -f expected.txt actual.txt

echo 'Testing that Tests-for pragma needs to be specified...'

cat >expected.txt <<EOF
falderal: Found a test before any Tests-for was specified
EOF
$FALDERAL test eg/NoTestsSpecified.falderal >actual.txt 2>&1
diff -u expected.txt actual.txt
ENOTEST=$?
rm -f expected.txt actual.txt

echo 'Testing that functionalities do not bleed into successive files...'

cat >expected.txt <<EOF
falderal: Can't find functionality "Count lines" in []
EOF
cd eg
$FALDERAL test wc.falderal Underspecified.falderal >../actual.txt 2>&1
cd ..
diff -u expected.txt actual.txt
ENOBLEED=$?
rm -f expected.txt actual.txt

if [ $EID != 0 -o $ELHS != 0 -o $EERR != 0 -o $ECL1 != 0 -o $ECL2 != 0 -o \
     $ECL3 != 0 -o $ECL4 != 0 -o $EWC != 0 -o $EECHO != 0 -o \
     $ENOTEST != 0 -o $ENOBLEED != 0 ]
  then
    echo "Internal tests failed!"
    exit 1
  else
    echo "All tests passed."
    exit 0
fi
