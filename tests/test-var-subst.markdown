Variable substitution
---------------------

Tests for variable substitution, and quoting rules during such.

Note that Falderal is responsible for quoting the substitution text
of all `%(...)` variables occurring in a shell command template;
it is not necessary to put any quotes around them in the template string.

...

Note that when variables are expanded, backslash sequences in the
replacement string ("\n", etc) are not expanded.

    -> Functionality "Echo" is implemented by
    -> shell command "python echo.py %(test-body-text)"

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
