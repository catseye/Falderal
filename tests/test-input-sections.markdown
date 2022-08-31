Falderal Test: Input sections
-----------------------------

Test tests that have test input sections.

    -> Functionality "Silly Interpreter" is implemented by
    -> shell command "python3 silly-interpreter.py %(test-body-file)"

    -> Tests for functionality "Silly Interpreter"

    | print x
    = 

    | read x
    | print x
    + meow
    = meow

    | read x
    | print x
    + purr
    = purr

    | read x
    | print x
    | read y
    | print y
    + meow
    + purr
    = meow
    = purr

    | read x
    | read y
    | print y
    | print x
    + meow
    + purr
    = purr
    = meow

If the input section appears first (i.e. there is no test body),
the previous test body is re-used.

    + zing
    + zang
    = zang
    = zing
