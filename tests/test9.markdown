Falderal Test 9
---------------

When we have a test that expects a successful result, the
expected text is matched against standard output.

    -> Functionality "Succeed" is implemented by shell command
    -> "python fail.py '%(test-text)' bar 0"

    -> Tests for functionality "Succeed"

    | foo
    = foo

If you wish to match the expected result against both standard
output and standard error, it's up to you to redirect them.

    -> Functionality "Succeed/All" is implemented by shell command
    -> "python fail.py '%(test-text)' bar 0 2>&1"

    -> Tests for functionality "Succeed/All"

    | foo
    = foo
    = bar

When we have a test that expects an error result, the
expected text is matched against standard error.

    -> Functionality "Fail" is implemented by shell command
    -> "python fail.py foo '%(test-text)' 1"

    -> Tests for functionality "Fail"

    | bar
    ? bar

If you wish to match the expected error against both standard
output and standard error, it's up to you to redirect them.

    -> Functionality "Fail/All" is implemented by shell command
    -> "python fail.py foo '%(test-text)' 1 1>&2"

    -> Tests for functionality "Fail/All"

    | bar
    ? foo
    ? bar
