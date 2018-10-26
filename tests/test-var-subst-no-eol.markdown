Falderal Test 5
---------------

Tests for variable substitution, and missing EOL at end
of output.

Note that Falderal is responsible for quoting the substitution text
of all `%(...)` variables occurring in a shell command template;
it is not necessary to put any quotes around them in the template string.

    -> Functionality "Echo" is implemented by
    -> shell command "python echo.py %(test-body-text)"

    -> Tests for functionality "Echo"

    | hello
    = hello

    | hi
    | hi
    = hi
    = hi

    -> Functionality "Echo, no newline" is implemented by
    -> shell command "python echo.py -n %(test-body-text)"

    -> Tests for functionality "Echo, no newline"

    | hello
    = hello

    | hi
    | hi
    = hi
    = hi

Note that when variables are expanded, backslash sequences in the
replacement string ("\n", etc) are not expanded.

    -> Tests for functionality "Echo"

    | he\nl\tl\\o
    = he\nl\tl\\o

Note that single quotes needn't be supplied around `%(test-body-file)`
or `%(output-file)`.

    -> Functionality "Cat, from file" is implemented by
    -> shell command "python cat.py -f %(test-body-file)"

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
    -> shell command "python cat.py -f %(test-body-file) -o %(output-file)"

    -> Tests for functionality "Cat, to and from file"

    | hello
    = hello

    | hi
    | hi
    = hi
    = hi
