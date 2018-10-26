Falderal Test: test-body-text and test-input-text
-------------------------------------------------

The test body and the test input can be passed as
strings to the shell command.

    -> Functionality "Echo Body" is implemented by shell command
    -> "python echo.py %(test-body-text)"

    -> Tests for functionality "Echo Body"

    | foo
    + bar
    = foo

Single quotes in the test body text are single escaped.

    | don't
    + can't
    = don't

    -> Functionality "Echo Input" is implemented by shell command
    -> "python echo.py %(test-input-text)"

    -> Tests for functionality "Echo Input"

    | foo
    + bar
    = bar

Single quotes in the test input text are single escaped.

    | don't
    + can't
    = can't
