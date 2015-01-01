Falderal test shared test bodies
--------------------------------

Test that tests with input sections can re-use the previous test's test body.

    -> Functionality "Silly Interpreter" is implemented by
    -> shell command "python silly-interpreter.py %(test-body-file)"

    -> Tests for functionality "Silly Interpreter"

    | print x
    = 

    | read x
    | print x
    + meow
    = meow

    + purr
    = purr

    | read x
    | read y
    | print y
    | print x
    + meow
    + purr
    = purr
    = meow

    + dog
    + cat
    = cat
    = dog
