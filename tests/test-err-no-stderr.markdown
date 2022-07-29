Falderal Test: err-no-stderr
----------------------------

When running a failing test, if the implementation finished with a failing
exit code, but did not provide anything on `stderr`, expect the error message
to be on `stdout`.

    -> Functionality "Error on stdout" is implemented by shell command
    -> "python3 fail.py %(test-body-text) '' 1"

    -> Tests for functionality "Error on stdout"

    | this is the error message
    ? this is the error message
