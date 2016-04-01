#!/bin/sh

# Really crude test harness for py-falderal itself...

bin/falderal -v -t || exit 1

cd tests

FIRST_TESTS="
test-pass-fail test-no-functionality test-ill-formed test-no-test-body
test-var-subst-no-eol
test-utf8 test-crlf
test-bad-indentation
test-input-sections test-shared-body
test-stdout-stderr test-err-no-stderr
"
for TEST in ${FIRST_TESTS}; do
    echo ${TEST}...
    ../bin/falderal --cavalier ${TEST}.markdown > ${TEST}.actual 2>&1
    diff -u ${TEST}.expected ${TEST}.actual || exit 1
done

# tests for linting
LINTING_TESTS="test-no-tests"
for TEST in ${LINTING_TESTS}; do
    echo ${TEST}...
    ../bin/falderal ${TEST}.markdown > ${TEST}.actual 2>&1
    diff -u ${TEST}.expected ${TEST}.actual || exit 1
done

TWO_PART_TESTS="
test-no-functionality-leak test-implementations-global test-appliances
"
for TEST in ${TWO_PART_TESTS}; do
    echo ${TEST}...
    ../bin/falderal ${TEST}-a.markdown ${TEST}-b.markdown > ${TEST}.actual 2>&1
    diff -u ${TEST}.expected ${TEST}.actual || exit 1
done

# special tests: -b
TEST=test-substring-error
echo ${TEST}...
../bin/falderal -b ${TEST}.markdown > ${TEST}.actual 2>&1
diff -u ${TEST}.expected ${TEST}.actual || exit 1

rm -f *.actual
echo 'All tests passed.'
