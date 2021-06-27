#!/bin/sh

# Really crude test harness for py-falderal itself...

run_falderal_integration_tests() {
    FALDERAL="$1"

    FIRST_TESTS="
    test-pass-fail test-no-functionality test-ill-formed test-no-test-body
    test-var-subst test-no-eol
    test-utf8 test-crlf
    test-bad-indentation
    test-input-sections test-shared-body
    test-stdout-stderr test-err-no-stderr
    test-freestyle-format
    "
    for TEST in ${FIRST_TESTS}; do
        echo ${TEST}...
        $FALDERAL --cavalier ${TEST}.markdown > ${TEST}.actual 2>&1
        diff -u ${TEST}.expected ${TEST}.actual || exit 1
    done

    # tests for linting
    LINTING_TESTS="test-no-tests"
    for TEST in ${LINTING_TESTS}; do
        echo ${TEST}...
        $FALDERAL ${TEST}.markdown > ${TEST}.actual 2>&1
        diff -u ${TEST}.expected ${TEST}.actual || exit 1
    done

    TWO_PART_TESTS="
    test-no-functionality-leak test-implementations-global test-appliances
    "
    for TEST in ${TWO_PART_TESTS}; do
        echo ${TEST}...
        $FALDERAL ${TEST}-a.markdown ${TEST}-b.markdown > ${TEST}.actual 2>&1
        diff -u ${TEST}.expected ${TEST}.actual || exit 1
    done

    # special tests: -b
    TEST=test-substring-error
    echo ${TEST}...
    $FALDERAL -b ${TEST}.markdown > ${TEST}.actual 2>&1
    diff -u ${TEST}.expected ${TEST}.actual || exit 1
}

run_falderal_tests() {
    PYTHON="$1"
    PYTHONPATH=src $PYTHON src/falderal/tests.py -v || exit 1
    ( cd tests && run_falderal_integration_tests "$PYTHON ../bin/falderal" )
}

### Main ###

if [ "x$PYTHON" != "x" ]; then
    if command -v "$PYTHON" > /dev/null 2>&1; then
        run_falderal_tests "$PYTHON" || exit 1
    else
        echo "$PYTHON not found on executable search path. Aborting."
        exit 1
    fi
else
    MISSING=""
    if command -v python2 > /dev/null 2>&1; then
        run_falderal_tests "python2" || exit 1
    else
        MISSING="${MISSING}2"
    fi
    if command -v python3 > /dev/null 2>&1; then
        run_falderal_tests "python3" || exit 1
    else
        MISSING="${MISSING}3"
    fi
    if [ "x${MISSING}" = "x23" ]; then
        echo "Neither python2 nor python3 found on executable search path. Aborting."
        exit 1
    fi
fi

rm -f *.actual
echo 'All tests passed.'
