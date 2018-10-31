Falderal Test: substring-error
------------------------------

When the `-b` option is passed to `falderal`, it considers
an error-expecting test successful if the error text which
was produced simply contains (rather than totally equals)
the expected error text.

    -> Functionality "Fail" is implemented by shell command
    -> "python fail.py foo %(test-body-text) 1"

    -> Tests for functionality "Fail"

    | this is the error message
    ? this is the error message

    | (file tmpgZ5W7e3, line 123): this is the error message
    ? this is the error message

    | (file tmpgZ5W7e3, line 123): an error occurred on this line.
    | It is a very pretty error, consisting of several nested
    | sub-errors, of which I know very little.  (ref #371282)
    ? an error occurred on this line.
    ? It is a very pretty error, consisting of several nested
    ? sub-errors, of which I know very little.
