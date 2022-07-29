Falderal Test: "freestyle" format for tests
-------------------------------------

This document tests the alternate ("freestyle") format for tests
introduced in the Falderal Literate Test Format version 0.11.  This
format allows the test body to consist entirely of un-prefixed text,
as long as it is written in a single indented block, and as long as
the final line(s) of the test begin with one of the prefixes
`=> ` or `==> ` or `===> ` or `?> ` or `??> `or `???> `.

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
    = prreow
    => | purr
    => | prrr
    => = prreow

    | purr
    + prrr
    ? prreow
    => | purr
    => + prrr
    => ? prreow

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

    | meow
    ? rreoww
    ???> bow
    ???> wow

Freestyle-format tests can also contain input sections.

    -> Functionality "Silly Interpreter" is implemented by
    -> shell command "python silly-interpreter.py %(test-body-file)"

    -> Tests for functionality "Silly Interpreter"

    read x
    read y
    print y
    print x
    <= meow
    <= purr
    => purr
    => meow

    read x
    read y
    print y
    print x
    <== meow
    <== purr
    ==> purr
    ==> meow

    read x
    read y
    print y
    print x
    <=== meow
    <=== purr
    ===> purr
    ===> meow

The trick of re-using the previous test body with a different
test input if the test body is omitted doesn't work with
freestyle-format test input sections (i.e., this will fail.)

    <== zing
    <== zang
    ==> zang
    ==> zing
