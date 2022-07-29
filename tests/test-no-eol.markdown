Falderal Test: Missing EOL at end of output
----------------------------

It makes no difference whether there is an EOL at the end of
the test output or not.

    -> Functionality "Echo" is implemented by
    -> shell command "python3 echo.py %(test-body-text)"

    -> Tests for functionality "Echo"

    | hello
    = hello

    | hi
    | hi
    = hi
    = hi

    -> Functionality "Echo, no newline" is implemented by
    -> shell command "python3 echo.py -n %(test-body-text)"

    -> Tests for functionality "Echo, no newline"

    | hello
    = hello

    | hi
    | hi
    = hi
    = hi
