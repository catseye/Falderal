import codecs
import os
import re
from subprocess import Popen, PIPE
from tempfile import mkstemp

# Python 2/3
try:
    unicode = unicode
except NameError:
    unicode = str

try:
    from shlex import quote as shlex_quote
except ImportError:
    from pipes import quote as shlex_quote

# Note: the __str__ method of all the classes defined herein should
# produce a short, human-readable summary of the contents of the object,
# suitable for displaying in the test results but not necessarily
# complete.  __repr__ should produce something complete, when it is
# present.  Dumping complete information in a human-readable format
# is done by non-magical methods.


##### Exceptions #####

class FalderalSyntaxError(ValueError):
    pass


##### Options #####

# If the Falderal objects are used by a command-line driver which
# gets an options object from OptParse, it should be duck-type
# compatible with objects of this class.

class Options(object):
    def __init__(self):
        self.substring_error = False


DEFAULT_OPTIONS = Options()


##### Test Results #####

class Outcome(object):
    """The outcome (either the expected outcome, or the actual outcome)
    of running a test case.

    Note that Outcomes are different from TestResults.  Outcomes are
    the result of doing the thing the test asks, TestResults are the
    result of judging whether the Outcome was correct.

    """
    def __init__(self, text):
        assert isinstance(text, unicode), repr(text)
        self.text = text

    def __repr__(self):
        return '%s(%r)' % (self.__class__.__name__, self.text)

    def __eq__(self, other):
        return self.__class__ == other.__class__ and self.text == other.text


class OutputOutcome(Outcome):
    def __str__(self):
        return u'output:\n' + self.text


class ErrorOutcome(Outcome):
    def __str__(self):
        return u'error:\n' + self.text


class TestResult(object):
    """The result of a test, representing whether the outcome was
    correct or not.

    Note that Outcomes are different from TestResults.  Outcomes are
    the result of doing the thing the test asks, TestResults are the
    result of judging whether the Outcome was correct.

    """
    def short_description(self):
        raise NotImplementedError

    def report(self):
        raise NotImplementedError

    def is_successful(self):
        raise NotImplementedError

    def format_text_block(self, text):
        """If the given text extends over more than one line, precede it
        with a newline.

        """
        text = unicode(text)
        if u'\n' in text and not text.startswith((u'output:', u'error:')):
            return (u'\n' + text).encode('UTF-8')
        else:
            return text.encode('UTF-8')


class Success(TestResult):
    def __init__(self, test, implementation):
        self.test = test
        self.implementation = implementation

    def short_description(self):
        return 'success'

    def report(self):
        pass

    def is_successful(self):
        return True

    def __repr__(self):
        return '%s(%s, %s)' % (self.__class__.__name__, self.test,
                               self.implementation)


class Failure(TestResult):
    def __init__(self, test, implementation, actual):
        self.test = test
        self.implementation = implementation
        self.actual = actual

    def short_description(self):
        return 'expected %r, got %r' % (self.test.expectation, self.actual)

    def fmt(self, field, contents):
        if str == unicode:  # Python 3
            if isinstance(contents, bytes):
                contents = contents.decode('utf-8')
            s = field + contents
            print(s)
        else:               # Python 2
            s = field + contents
            print(s)

    def report(self):
        self.fmt("FAILED  : ", self.format_text_block(self.test.description))
        self.fmt("Location: ", self.test.body_block.location())
        self.fmt("Function: ", self.format_text_block(self.test.functionality.name))
        self.fmt("Impl    : ", self.format_text_block(self.implementation))
        self.fmt("Body    : ", self.format_text_block(self.test.body))
        self.fmt("Expected: ", self.format_text_block(self.test.expectation))
        self.fmt("Actual  : ", self.format_text_block(self.actual))
        print("")

    def is_successful(self):
        return False

    def __repr__(self):
        return '%s(%s, %s, %s)' % (self.__class__.__name__, self.test,
                                   self.implementation, self.actual)


##### Blocks #####

class Block(object):
    """A segment of a Falderal-formatted file.

    """

    FREESTYLE_MAP = {
        u'<= ':   u'+ ',
        u'<== ':  u'+ ',
        u'<=== ': u'+ ',
        u'=> ':   u'= ',
        u'==> ':  u'= ',
        u'===> ': u'= ',
        u'?> ':   u'? ',
        u'??> ':  u'? ',
        u'???> ': u'? ',
    }
    FREESTYLE_PREFIXES = list(FREESTYLE_MAP.keys())
    PREFIXES = FREESTYLE_PREFIXES + [
        u'| ',
        u'+ ',
        u'? ',
        u'= ',
        u'->',
    ]
    VALID_PATTERNS = [
        [u'->'],
        [u'> '],
        [u'| ', u'= '],
        [u'| ', u'? '],
        [u'| ', u'+ ', u'= '],
        [u'| ', u'+ ', u'? '],
        [u'+ ', u'= '],
        [u'+ ', u'? '],
    ]

    def __init__(self, line_num=1, filename=None, lines=None):
        if lines is None:
            lines = []
        self.lines = lines
        self.line_num = line_num
        self.filename = filename

    def __repr__(self):
        filename_repr = '' if self.filename is None else ', filename=%r' % self.filename
        return "%s(line_num=%r%s)" % (
            self.__class__.__name__, self.line_num, filename_repr
        )

    def __str__(self):
        return unicode(repr(self))

    def location(self):
        filename = self.filename
        if filename is None:
            filename = "<<input file>>"
        return "%s, line %s" % (filename, self.line_num)

    def append(self, line):
        assert isinstance(line, unicode)
        self.lines.append(line)

    def text(self, seperator='\n'):
        return seperator.join(self.lines)

    def is_empty(self):
        return all([(not line or line.isspace()) for line in self.lines])

    def deconstruct(self):
        """Return a list of pairs of (prefix, list of lines) representing
        the contents of this Block.  The pairs are in the order the runs
        of prefixes occur in the block.  The lines in the list of lines
        have had their prefix stripped from them."""

        pairs = []
        prefix_state = None
        acc = []

        for line in self.lines:
            prefix_of_line = ''
            for prefix in self.PREFIXES:
                if line.startswith(prefix):
                    prefix_of_line = prefix
                    break
            if prefix_of_line == prefix_state:
                acc.append(line[len(prefix_of_line):])
            else:
                if acc:
                    pairs.append((prefix_state, acc))
                prefix_state = prefix_of_line
                acc = []
                acc.append(line[len(prefix_of_line):])

        if acc:
            pairs.append((prefix_state, acc))

        return pairs

    def classify(self, state):
        """Return the Test or Pragma that this Block represents."""

        pattern = self.deconstruct()
        pattern_prefixes = [p[0] for p in pattern]

        def make_block_from_pattern(prefix):
            lines = None
            for (candidate_prefix, candidate_lines) in pattern:
                if candidate_prefix == prefix:
                    lines = candidate_lines
                    break
            if lines is None:
                return None
            return Block(
                line_num=self.line_num, filename=self.filename, lines=lines
            )

        def reconstruct(pattern, default_prefix, prefix_map):
            new_pattern = []
            lines = []
            for (candidate_prefix, candidate_lines) in pattern:
                if candidate_prefix in prefix_map:
                    new_pattern.append((prefix_map[candidate_prefix], candidate_lines))
                else:
                    lines.extend([candidate_prefix + line for line in candidate_lines])
            return [(default_prefix, lines)] + new_pattern

        if pattern_prefixes[-1] in self.FREESTYLE_PREFIXES:
            # This block ends with an expectation indicating a freestyle block.
            # We re-interpret this block according to the freestyle block rules.
            pattern = reconstruct(pattern, u'| ', self.FREESTYLE_MAP)
            pattern_prefixes = [p[0] for p in pattern]

        if '' in pattern_prefixes:
            # There is plain, non-prefixed text embedded somewhere in this Block.
            # TODO:issue a warning unless cavalier
            # For now, assume it is Just Indented Text And That Is OK.
            return None

        if pattern_prefixes in [[u'= '], [u'? ']]:
            raise FalderalSyntaxError(
                ("line %d: " % self.line_num) +
                "expectation must be preceded by test body or test input")

        if pattern_prefixes in [[u'| ']]:
            raise FalderalSyntaxError(
                ("line %d: " % self.line_num) +
                "test body must be followed by expectation or test input")

        if pattern_prefixes not in self.VALID_PATTERNS:
            raise FalderalSyntaxError(
                ("line %d: " % self.line_num) +
                "incorrectly formatted test block")

        if pattern_prefixes == [u'->']:
            return Pragma(line_num=self.line_num, filename=self.filename, lines=pattern[0][1])
        elif pattern_prefixes[-1] in [u'= ', u'? ']:
            if state.current_functionality is None:
                raise FalderalSyntaxError(
                    ("line %d: " % self.line_num) +
                    "functionality under test not specified")
        
            body_block = make_block_from_pattern(u'| ') or state.last_test_body_block
            input_block = make_block_from_pattern(u'+ ') or state.last_test_input_block

            if pattern_prefixes[-1] == u'= ':
                expectation = OutputOutcome(make_block_from_pattern(u'= ').text())
            elif pattern_prefixes[-1] == u'? ':
                expectation = ErrorOutcome(make_block_from_pattern(u'? ').text())
            else:
                raise NotImplementedError

            test = Test(body_block=body_block,
                        input_block=input_block,
                        expectation=expectation,
                        functionality=state.current_functionality,
                        desc_block=state.last_desc_block)

            state.last_test_body_block = body_block

            return test
        else:
            raise FalderalSyntaxError(
                ("line %d: " % self.line_num) +
                "incorrectly formatted test block")


class Pragma(Block):
    def execute(self, state):
        pragma_text = self.text(seperator=' ')
        match = re.match(r'^\s*Tests\s+for\s+functionality\s*\"(.*?)\"\s*$', pragma_text)
        if match:
            functionality_name = match.group(1)
            state.current_functionality = state.functionalities.setdefault(
                functionality_name,
                Functionality(functionality_name)
            )
        match = re.match(r'^\s*Functionality\s*\"(.*?)\"\s*is\s+implemented\s+by\s+shell\s+command\s*\"(.*?)\"\s*$', pragma_text)
        if match:
            functionality_name = match.group(1)
            command = match.group(2)
            functionality = state.functionalities.setdefault(
                functionality_name,
                Functionality(functionality_name)
            )
            implementation = ShellImplementation(command)
            functionality.add_implementation(implementation)


class InterveningText(Block):
    pass


##### Parsing State #####


class ParseState(object):
    def __init__(self, current_functionality=None):
        self.last_desc_block = None
        self.last_test_body_block = None
        self.last_test_input_block = None
        self.current_functionality = current_functionality
        self.functionalities = None


##### Documents #####


class Document(object):
    """An object representing a parsed Falderal file.

    """
    def __init__(self):
        self.lines = []
        self.filename = None

    @classmethod
    def load(cls, filename):
        d = cls()
        f = codecs.open(filename, 'r', 'UTF-8')
        for line in f:
            d.append(line)
        f.close()
        d.filename = filename
        return d

    def append(self, line):
        assert isinstance(line, unicode)
        line = line.rstrip(u'\r\n')
        self.lines.append(line)

    def parse_lines_to_blocks(self):
        """Parse the lines of the Document into Blocks.

        """
        indent = None
        blocks = []
        line_num = 1
        block = None

        for line in self.lines:
            # make sure we get a Block to start with
            if indent is None:
                if line.startswith(u'    '):
                    indent = u''
                else:
                    indent = u'    '

            if indent == u'':
                if line.startswith(u'    '):
                    indent = u'    '
                    if block is not None:
                        blocks.append(block)
                    block = Block(
                        line_num=line_num,
                        filename=self.filename
                    )
            elif indent == u'    ':
                if not line.startswith(u'    '):
                    indent = u''
                    if block is not None:
                        blocks.append(block)
                    block = InterveningText(
                        line_num=line_num,
                        filename=self.filename
                    )

            line = line[len(indent):]

            block.append(line)
            line_num += 1

        if block is not None:
            blocks.append(block)

        return blocks

    def parse_blocks_to_tests(self, blocks, functionalities):
        state = ParseState()
        state.functionalities = functionalities

        tests = []
        for block in blocks:
            if isinstance(block, InterveningText):
                if block.is_empty():
                    continue
                state.last_desc_block = block
                continue

            test_or_pragma = block.classify(state)

            if test_or_pragma is None:
                # It was just some indented text which doesn't concern us
                pass
            elif isinstance(test_or_pragma, Test):
                tests.append(test_or_pragma)
            elif isinstance(test_or_pragma, Pragma):
                test_or_pragma.execute(state)
            else:
                raise NotImplementedError('need Pragma or Test, not ' + repr(test_or_pragma))

        return tests

    def extract_tests(self, functionalities):
        """Extract all Tests from this Document.

        """
        blocks = self.parse_lines_to_blocks()
        tests = self.parse_blocks_to_tests(blocks, functionalities)
        return tests


##### Functionalities and their Implementations #####

class Functionality(object):
    """An object representing a Falderal functionality.

    A functionality can have multiple implementations.

    Each test has exactly one functionality.

    """
    def __init__(self, name):
        self.name = name
        self.implementations = []

    def __repr__(self):
        return "Functionality(%r)" % self.name

    def __str__(self):
        return unicode(repr(self))

    def add_implementation(self, implementation):
        self.implementations.append(implementation)


class Implementation(object):
    """An object representing an implementation (something that is
    used to run a test) in Falderal.

    """
    def __init__(self):
        pass

    def run(self, body=None, input=None, verbose=False):
        """Returns the RunResult of running this implementation on the
        given test body and input.

        """
        raise NotImplementedError("subclass needs to implement run()")


class CallableImplementation(Implementation):
    """An implementation which is implemented by a Python callable.

    This is mostly useful for internal tests.

    """
    def __init__(self, callable):
        self.callable = callable

    def __repr__(self):
        return '%s(%r)' % (self.__class__.__name__, self.callable)

    def __str__(self):
        return u'callable "%r"' % self.callable

    def run(self, body=None, input=None, verbose=False):
        try:
            result = self.callable(body, input)
            return OutputOutcome(result)
        except Exception as e:
            return ErrorOutcome(unicode(e))


class ShellImplementation(Implementation):
    def __init__(self, command):
        self.command = command

    def __repr__(self):
        return '%s(%r)' % (self.__class__.__name__, self.command)

    def __str__(self):
        return u'shell command "%s"' % self.command

    def __eq__(self, other):
        return self.__class__ == other.__class__ and self.command == other.command

    def subst(self, command, var_name, value):
        """Replace all occurrences of `var_name` in `command` with
        `value`, but make sure `value` is properly shell-escaped first."""
        return command.replace(var_name, shlex_quote(value))

    def run(self, body=None, input=None, verbose=False):
        # first, expand all known variables in the command, using subst().
        test_filename = None
        output_filename = None
        command = self.command

        command_contained_test_body_file = False
        command_contained_test_body_text = False
        command_contained_test_input_file = False
        command_contained_test_input_text = False

        if '%(test-body-file)' in self.command:
            # choose a temp file name and write the body to that file
            fd, test_filename = mkstemp()
            with codecs.open(test_filename, 'w', 'UTF-8') as file:
                file.write(body)
                file.close()
            os.close(fd)
            # replace all occurrences in command
            command = self.subst(command, '%(test-body-file)', test_filename)
            command_contained_test_body_file = True

        if '%(test-body-text)' in self.command:
            # replace all occurrences in command
            command = self.subst(command, '%(test-body-text)', body)
            command_contained_test_body_text = True

        if '%(test-input-file)' in self.command:
            # choose a temp file name and write the input to that file
            fd, test_input_filename = mkstemp()
            with codecs.open(test_input_filename, 'w', 'UTF-8') as file:
                if input is not None:
                    file.write(input)
                file.close()
            os.close(fd)
            # replace all occurrences in command
            command = self.subst(command, '%(test-input-file)', test_input_filename)
            command_contained_test_input_file = True

        if '%(test-input-text)' in self.command:
            # replace all occurrences in command
            command = self.subst(command, '%(test-input-text)', input)
            command_contained_test_input_text = True

        if '%(output-file)' in self.command:
            # choose a temp file name to read output from later
            fd, output_filename = mkstemp()
            os.close(fd)
            # replace all occurrences in command
            command = self.subst(command, '%(output-file)', output_filename)

        if verbose:
            print(self, command)

        # subshell the command and return the output
        pipe = Popen(command, shell=True,
                     stdin=PIPE, stdout=PIPE, stderr=PIPE)
        # XXX How *exactly* do we decide what to send to the command's standard input?
        # XXX Check and/or update the spec.
        pipe_input = None
        if not (command_contained_test_input_file or command_contained_test_input_text):
            pipe_input = None if input is None else input.encode('UTF-8')
        if not (command_contained_test_body_file or command_contained_test_body_text):
            pipe_input = None if body is None else body.encode('UTF-8')
        outputs = pipe.communicate(input=pipe_input)

        def get_stdout(outputs):
            if output_filename is None:
                return self.normalize_output(outputs[0])
            else:
                with codecs.open(output_filename, 'r', 'UTF-8') as f:
                    output = f.read()
                return output
            
        if pipe.returncode == 0:
            result = OutputOutcome(get_stdout(outputs))
        else:
            # first look for error message on stderr.  if empty, try stdout.
            error_message = self.normalize_output(outputs[1])
            if not error_message:
                error_message = self.normalize_output(get_stdout(outputs))
            result = ErrorOutcome(error_message)

        # clean up temporary files
        for filename in (test_filename, output_filename):
            if filename is not None:
                os.unlink(filename)
        # finis
        return result

    def normalize_output(self, text):
        try:
            text = text.decode('UTF-8', errors='ignore')
        except AttributeError:
            pass
        text = re.sub(r'\r\n', '\n', text)
        return text.strip('\r\n')


##### Tests #####

class Test(object):
    """An object representing a Falderal test.

    Normally a test body Block is given as the body_block argument,
    and possibly a test input Block is given as input_block,
    and the body and input attributes are derived from it.  However
    in the absence of these blocks (as in many of the internal tests)
    a body and/or input may be passed alone.
    
    TODO: maybe write a helper function for that instead.

    """
    def __init__(self, body_block=None, input_block=None, expectation=None,
                 functionality=None, desc_block=None, body=None, input=None):
        self.body_block = body_block
        self.input_block = input_block
        self.expectation = expectation
        self.functionality = functionality
        self.desc_block = desc_block
        self.body = body
        if self.body is None:
            self.body = unicode(self.body_block.text())
        self.input = input
        if self.input is None and self.input_block is not None:
            self.input = unicode(self.input_block.text())
        assert isinstance(self.body, unicode)
        assert self.input is None or isinstance(self.input, unicode)
    
    def __repr__(self):
        return (
            ("Test(body_block=%r, input_block=%r, expectation=%r, " +
             "functionality=%r, desc_block=%r, body=%r, input=%r)") %
            (self.body_block, self.input_block, self.expectation,
             self.functionality, self.desc_block, self.body, self.input)
        )

    def __str__(self):
        return unicode(repr(self))

    def run(self, options=DEFAULT_OPTIONS):
        """Returns a list of Results, one for each implementation of
        the functionality being tested.

        """
        results = []
        for implementation in self.functionality.implementations:
            result = implementation.run(
                body=self.body, input=self.input, verbose=options.verbose
            )
            if self.judge(result, options):
                results.append(Success(self, implementation))
            else:
                results.append(Failure(self, implementation, result))
        return results

    def judge(self, result, options):
        if not isinstance(result, self.expectation.__class__):
            return False
        if options.substring_error and isinstance(result, ErrorOutcome):
            return self.expectation.text in result.text
        else:
            return self.expectation.text == result.text

    @property
    def description(self):
        if self.desc_block is None:
            return ''
        return self.desc_block.text()
