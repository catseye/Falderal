#!/bin/sh

# Really crude test harness for py-falderal itself...

bin/falderal -t || exit 1

cd tests
for TEST in test1 test2 test3 test4 test5 test9 test10 test-utf8 test-crlf; do
    echo ${TEST}...
    ../bin/falderal ${TEST}.markdown > ${TEST}.actual 2>&1
    diff -u ${TEST}.expected ${TEST}.actual || exit 1
done

# two-part tests
for TEST in test6 test7 test8; do
    echo ${TEST}...
    ../bin/falderal ${TEST}a.markdown ${TEST}b.markdown > ${TEST}.actual 2>&1
    diff -u ${TEST}.expected ${TEST}.actual || exit 1
done

# special tests: -b
TEST=test-substring-error
echo ${TEST}...
../bin/falderal -b ${TEST}.markdown > ${TEST}.actual 2>&1
diff -u ${TEST}.expected ${TEST}.actual || exit 1

rm -f *.actual
echo 'All tests passed.'
