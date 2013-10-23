Falderal Test 5
---------------

Tests for variable substitution, and missing EOL at end
of output.

Note the use of single quotes around the `%(test-text)` variable;
without these, shell chaos is likely to result.

    -> Functionality "Echo" is implemented by
    -> shell command "python echo.py '%(test-text)'"

    -> Tests for functionality "Echo"

    | hello
    = hello

    | hi
    | hi
    = hi
    = hi

    -> Functionality "Echo, no newline" is implemented by
    -> shell command "python echo.py -n '%(test-text)'"

    -> Tests for functionality "Echo, no newline"

    | hello
    = hello

    | hi
    | hi
    = hi
    = hi

Note that single quotes needn't be supplied around `%(test-file)`
or `%(output-file)`.

    -> Functionality "Cat, from file" is implemented by
    -> shell command "python cat.py -f %(test-file)"

    -> Tests for functionality "Cat, from file"

    | hello
    = hello

    | hi
    | hi
    = hi
    = hi

    -> Functionality "Cat, to file" is implemented by
    -> shell command "python cat.py -o %(output-file)"

    -> Tests for functionality "Cat, to file"

    | hello
    = hello

    | hi
    | hi
    = hi
    = hi

    -> Functionality "Cat, to and from file" is implemented by
    -> shell command "python cat.py -f %(test-file) -o %(output-file)"

    -> Tests for functionality "Cat, to and from file"

    | hello
    = hello

    | hi
    | hi
    = hi
    = hi
