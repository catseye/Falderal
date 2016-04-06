Falderal Test: "new" format for tests
-------------------------------------

This document tests the alternate format for tests introduced in
the Falderal Literate Test Format version 0.11.  This format allows
the test body to consist entirely of un-prefixed text, as long as
it is written in a single indented block, and the final line(s) of
the test begin with one of the prefixes `=> ` or `==> ` or `===> `
or `?> ` or `??> `or `???> `.

    -> Functionality "Cat" is implemented by shell command "python cat.py"

    -> Tests for functionality "Cat"

Cat cats.

    meow
    => meow

    meow
    ==> meow

    meow
    ===> meow

There may be multiple final lines.

    purr
    prrr
    prreow
    ==> purr
    ==> prrr
    ==> prreow

The use of `==>` means that any text in the preceding lines that would,
in the previous format, be recognized as prefixes, are no longer recognized
as prefixes.

    | purr
    | prrr
    | prreow
    ==> | purr
    ==> | prrr
    ==> | prreow

    | purr
    + prrr
    + prreow
    => | purr
    => + prrr
    => + prreow

    purr
    -> prrr
    prreow
    ===> purr
    ===> -> prrr
    ===> prreow

Demonstrate error expectation (Intentional fail.)

    meow
    ?> woof

    meow
    ??> woof

    meow
    ???> bow
    ???> wow