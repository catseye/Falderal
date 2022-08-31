Falderal Test: Variable substitution
------------------------------------

Tests for variable substitution, and quoting rules during such.

Note that Falderal is responsible for quoting the substitution text
of all `%(...)` variables occurring in a shell command template;
it is not necessary to put any quotes around them in the template string.

    -> Functionality "Echo Body" is implemented by shell command
    -> "python3 echo.py %(test-body-text)"

    -> Tests for functionality "Echo Body"

    | foo
    + bar
    = foo

Single quotes in the test body text are single escaped.

    | don't
    + can't
    = don't

    -> Functionality "Echo Input" is implemented by shell command
    -> "python3 echo.py %(test-input-text)"

    -> Tests for functionality "Echo Input"

    | foo
    + bar
    = bar

Single quotes in the test input text are single escaped.

    | don't
    + can't
    = can't

Note that when variables are expanded, backslash sequences in the
replacement string ("\n", etc) are not expanded.

    -> Tests for functionality "Echo Body"

    | he\nl\tl\\o
    = he\nl\tl\\o

The rule that Falderal is responsible for quoting text substituted
into the command template extends to `%(test-body-file)` and
`%(test-input-file)` and `%(output-file)` as well.

    -> Functionality "Cat, from file" is implemented by
    -> shell command "python3 cat.py -f %(test-body-file)"

    -> Tests for functionality "Cat, from file"

    | hello
    = hello

    | hi
    | hi
    = hi
    = hi

    -> Functionality "Cat, to file" is implemented by
    -> shell command "python3 cat.py -o %(output-file)"

    -> Tests for functionality "Cat, to file"

    | hello
    = hello

    | hi
    | hi
    = hi
    = hi

    -> Functionality "Cat, to and from file" is implemented by
    -> shell command "python3 cat.py -f %(test-body-file) -o %(output-file)"

    -> Tests for functionality "Cat, to and from file"

    | hello
    = hello

    | hi
    | hi
    = hi
    = hi

    -> Functionality "Cat input, from file" is implemented by
    -> shell command "python3 cat.py -f %(test-input-file)"

    -> Tests for functionality "Cat input, from file"

    | hekko
    + hello
    = hello

    | hj
    | hj
    + hi
    + hi
    = hi
    = hi
