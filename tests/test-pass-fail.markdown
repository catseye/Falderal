Falderal Test: basic pass/fail
------------------------------

This is an example Falderal document which contains some
intentionally failing tests.  It is intended to test that
your Falderal implementation is, itself, not producing
incorrect output.

    -> Functionality "Cat" is implemented by
    ->   shell    command "python cat.py"

    -> Tests for functionality "Cat"

Cat cats.

    | meow
    = meow

    | purr
    | prrr
    | prreow
    = purr
    = prrr
    = prreow

Cat dogs, too. (Intentional fail.)

    | meow
    = woof

    | purr
    | prrr
    | prreow
    = woof
    = woof
    = awoooo
