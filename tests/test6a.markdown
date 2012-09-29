Falderal Test 6a
----------------

This is a two-file test (the other file is 6b) which shows
that even if you says "Tests for functionality x" in this
file, that tests-for meaning does not "leak" into the next
file.

    -> Functionality "Cat" is implemented by
    -> shell command "python cat.py"

    -> Tests for functionality "Cat"

Cat cats.

    | meow
    = meow
