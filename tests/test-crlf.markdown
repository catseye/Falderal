Falderal Test: CRLF
-------------------

These Falderal tests aim to show that the implementation
can compare the result with the expected text, regardless
of end-of-line convention (LF or CRLF).

    -> Functionality "CRLF" is implemented by
    -> shell command "python3 crlf.py"

    -> Tests for functionality "CRLF"

Cat CRLFs.

    | my kitty, 'tis of thee
    | feline felicity
    | something something
    = my kitty, 'tis of thee
    = feline felicity
    = something something

No CRLFs in test report (Intentional fail.)

    | one
    | two
    = three
