import os
from os.path import basename
import re
from subprocess import Popen, PIPE
from tempfile import mkstemp

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
        self.text = text

    def __repr__(self):
        return '%s(%r)' % (self.__class__.__name__, self.text)


class OutputOutcome(Outcome):
    def __str__(self):
        return 'output:\n' + self.text


class ErrorOutcome(Outcome):
    def __str__(self):
        return 'error:\n' + self.text


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

    def format_text_block(self, obj):
        """If the given text extends over more than one line, precede it
        with a newline.

        """
        text = str(obj)
        if '\n' in text and not text.startswith(('output:', 'error:')):
            return '\n' + text
        else:
            return text


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

    def report(self):
        print "FAILED  : " + self.format_text_block(self.test.description)
        print "Location: " + self.test.body_block.location()
        print "Impl    : " + self.format_text_block(self.implementation)
        print "Body    : " + self.format_text_block(self.test.body)
        print "Expected: " + self.format_text_block(self.test.expectation)
        print "Actual  : " + self.format_text_block(self.actual)
        print

    def is_successful(self):
        return False

    def __repr__(self):
        return '%s(%s, %s, %s)' % (self.__class__.__name__, self.test,
                                   self.implementation, self.actual)


##### Blocks #####

class Block(object):
    """A segment of a Falderal-formatted file.

    >>> b = Block('| ')
    >>> b.append('| test body line 1')
    >>> b.append('| test body line 2')
    >>> print b.text(prefix=True)
    | test body line 1
    | test body line 2
    >>> print b.text()
    test body line 1
    test body line 2
    >>> print b.text(seperator='')
    test body line 1test body line 2

    """
    def __init__(self, prefix, line_num=None, filename=None):
        self.prefix = prefix
        self.lines = []
        self.line_num = line_num
        self.filename = filename

    def __repr__(self):
        return "%s(%r, line_num=%r, filename=%r)" % (
            self.__class__.__name__, self.prefix, self.line_num, self.filename
        )

    def __str__(self):
        return repr(self)

    def location(self):
        filename = self.filename
        if filename is None:
            filename = "<<input file>>"
        return "%s, line %s" % (filename, self.line_num)

    def append(self, line):
        self.lines.append(line[len(self.prefix):])

    def text(self, prefix=False, seperator='\n'):
        if not prefix:
            return seperator.join(self.lines)
        else:
            return seperator.join(self.prefix + line for line in self.lines)


class LiterateCode(Block):
    pass


class Pragma(Block):
    pass


class TestBody(Block):
    pass


class TestInput(Block):
    pass


class ExpectedError(Block):
    pass


class ExpectedResult(Block):
    pass


class InterveningMarkdown(Block):
    pass


##### Documents #####

PREFIX = {
    '    | ': TestBody,
    '    + ': TestInput,
    '    ? ': ExpectedError,
    '    = ': ExpectedResult,
    '    ->': Pragma,
    '    > ': LiterateCode,
}


class Document(object):
    """An object representing a parsed Falderal file.

    """
    def __init__(self):
        self.lines = []
        self.blocks = None
        self.filename = None

    @classmethod
    def load(cls, filename):
        d = cls()
        f = open(filename)
        for line in f:
            d.append(line)
        f.close()
        d.filename = filename
        return d

    def append(self, line):
        line = line.rstrip('\r\n')
        self.lines.append(line)

    def parse_lines_to_blocks(self):
        r"""Parse the lines of the Document into Blocks.

        >>> d = Document()
        >>> d.append('This is a test file.')
        >>> d.append('    -> This is a pragma.')
        >>> d.append("    | This is some test input.\n")
        >>> d.append("    | It extends over two lines.")
        >>> d.append('    ? Expected Error')
        >>> d.append('    | Test with input')
        >>> d.append('    + input-for-test')
        >>> d.append('    = Expected result on output')
        >>> d.parse_lines_to_blocks()
        >>> [b.__class__.__name__ for b in d.blocks]
        ['InterveningMarkdown', 'Pragma', 'TestBody', 'ExpectedError',
         'TestBody', 'TestInput', 'ExpectedResult']
        >>> [b.line_num for b in d.blocks]
        [1, 2, 3, 5, 6, 7, 8]

        """
        state = '***'
        blocks = []
        block = None
        line_num = 1
        for line in self.lines:
            found_prefix = ''
            for prefix in PREFIX.keys():
                if line.startswith(prefix):
                    found_prefix = prefix
                    break
            if found_prefix == state:
                block.append(line)
            else:
                state = found_prefix
                if block is not None:
                    blocks.append(block)
                BlockClass = PREFIX.get(state, InterveningMarkdown)
                block = BlockClass(state, line_num=line_num,
                                   filename=self.filename)
                block.append(line)
            line_num += 1
        if block is not None:
            blocks.append(block)
        self.blocks = blocks

    def parse_blocks_to_tests(self, functionalities):
        r"""Assemble a list of Tests from the blocks in this Document.

        >>> funs = {}
        >>> d = Document()
        >>> d.append("This is a text file.")
        >>> d.append('It contains NO tests.')
        >>> d.parse_blocks_to_tests(funs)
        []

        >>> d = Document()
        >>> d.append('This is a test file.')
        >>> d.append('    -> Tests for functionality "Parse Thing"')
        >>> d.append("    | This is some test input.")
        >>> d.append("    | It extends over two lines.")
        >>> d.append('    ? Expected Error')
        >>> d.append('    | Test with input')
        >>> d.append('    + input-for-test')
        >>> d.append('    = Expected result on output')
        >>> d.append('    -> Tests for functionality "Run Thing"')
        >>> d.append("    | Thing")
        >>> d.append('    ? Oops')
        >>> tests = d.parse_blocks_to_tests(funs)
        >>> [t.body for t in tests]
        ['This is some test input.\nIt extends over two lines.',
         'Test with input', 'Thing']
        >>> [t.input_block for t in tests]
        [None, TestInput('    + ', line_num=7, filename=None), None]
        >>> tests[1].input_block.text()
        'input-for-test'
        >>> [t.expectation for t in tests]
        [ErrorOutcome('Expected Error'), OutputOutcome('Expected result on output'),
         ErrorOutcome('Oops')]
        >>> [t.functionality.name for t in tests]
        ['Parse Thing', 'Parse Thing', 'Run Thing']
        >>> sorted(funs.keys())
        ['Parse Thing', 'Run Thing']

        >>> d = Document()
        >>> d.append("    | This is some test body.")
        >>> d.append('    = Expected')
        >>> d.parse_blocks_to_tests({})
        Traceback (most recent call last):
        ...
        FalderalSyntaxError: line 2: functionality under test not specified

        >>> d = Document()
        >>> d.append('This is a test file.')
        >>> d.append('    ? Expected Error')
        >>> d.parse_blocks_to_tests({})
        Traceback (most recent call last):
        ...
        FalderalSyntaxError: line 2: expectation must be preceded by test body or test input

        >>> d = Document()
        >>> d.append('    -> Hello, this is pragma')
        >>> d.append('    = Expected')
        >>> d.parse_blocks_to_tests({})
        Traceback (most recent call last):
        ...
        FalderalSyntaxError: line 2: expectation must be preceded by test body or test input

        >>> d = Document()
        >>> d.append('    | This is test')
        >>> d.append('This is text')
        >>> d.parse_blocks_to_tests({})
        Traceback (most recent call last):
        ...
        FalderalSyntaxError: line 2: test body must be followed by expectation or test input

        >>> d = Document()
        >>> d.append('    -> Hello, this is pragma')
        >>> d.append('    + Input to where exactly?')
        >>> d.parse_blocks_to_tests({})
        Traceback (most recent call last):
        ...
        FalderalSyntaxError: line 2: test input must be preceded by test body

        >>> d = Document()
        >>> funs = {}
        >>> d.append('    -> Functionality "Parse Stuff" is implemented by '
        ...          'shell command "parse"')
        >>> d.append('')
        >>> d.append('    -> Functionality "Parse Stuff" is')
        >>> d.append('    -> implemented by shell command "pxxxy"')
        >>> tests = d.parse_blocks_to_tests(funs)
        >>> len(funs.keys())
        1
        >>> [i for i in funs["Parse Stuff"].implementations]
        [ShellImplementation('parse'), ShellImplementation('pxxxy')]

        """
        if self.blocks is None:
            self.parse_lines_to_blocks()
        tests = []
        current_functionality = None
        prev_block = None
        last_desc_block = None
        last_test_body_block = None
        last_test_input_block = None
        for block in self.blocks:
            # First, handle ExpectedError/ExpectedOutcome blocks.
            expectation_class = None
            if isinstance(block, ExpectedError):
                expectation_class = ErrorOutcome
            if isinstance(block, ExpectedResult):
                expectation_class = OutputOutcome
            if expectation_class:
                # Expectations must be preceded by TestBody or TestInput.
                if not (isinstance(prev_block, TestBody) or isinstance(prev_block, TestInput)):
                    raise FalderalSyntaxError(
                        ("line %d: " % block.line_num) +
                        "expectation must be preceded by test body or test input")
                if current_functionality is None:
                    raise FalderalSyntaxError(
                        ("line %d: " % block.line_num) +
                        "functionality under test not specified")
                test = Test(body_block=last_test_body_block,
                            input_block=last_test_input_block,
                            expectation=expectation_class(block.text()),
                            functionality=current_functionality,
                            desc_block=last_desc_block)
                tests.append(test)
                last_test_body_block = None
                last_test_input_block = None
            elif isinstance(block, TestInput):
                # Test input must be preceded by TestBody.
                if not isinstance(prev_block, TestBody):
                    raise FalderalSyntaxError(
                        ("line %d: " % block.line_num) +
                        "test input must be preceded by test body")
                # If we see a TestInput block, record it.
                last_test_input_block = block
            elif isinstance(block, TestBody):
                # If we see a TestBody block, record it.
                last_test_body_block = block
            else:
                # All others must not follow TestBody, or TestInput, as those need to be
                # followed by an expectation or test input
                if isinstance(prev_block, TestBody):
                    raise FalderalSyntaxError(
                        ("line %d: " % block.line_num) +
                        "test body must be followed by expectation or test input")
                if isinstance(prev_block, TestInput):
                    raise FalderalSyntaxError(
                        ("line %d: " % block.line_num) +
                        "test input must be followed by expectation")
                if isinstance(block, Pragma):
                    pragma_text = block.text(seperator=' ')
                    match = re.match(r'^\s*Tests\s+for\s+functionality\s*\"(.*?)\"\s*$', pragma_text)
                    if match:
                        functionality_name = match.group(1)
                        current_functionality = functionalities.setdefault(
                            functionality_name,
                            Functionality(functionality_name)
                        )
                    match = re.match(r'^\s*Functionality\s*\"(.*?)\"\s*is\s+implemented\s+by\s+shell\s+command\s*\"(.*?)\"\s*$', pragma_text)
                    if match:
                        functionality_name = match.group(1)
                        command = match.group(2)
                        functionality = functionalities.setdefault(
                            functionality_name,
                            Functionality(functionality_name)
                        )
                        implementation = ShellImplementation(command)
                        functionality.add_implementation(implementation)
                    match = re.match(r'^\s*Functionality\s*\"(.*?)\"\s*is\s+implemented\s+by\s+Haskell\s+function\s*(.*?)\:(.*?)\s*$', pragma_text)
                    if match:
                        functionality_name = match.group(1)
                        module = match.group(2)
                        function = match.group(3)
                        command = r'ghc -e "do c <- readFile \"%%(test-file)\"; putStrLn $ %s.%s c" %s.hs' % (
                            module, function, module
                        )
                        functionality = functionalities.setdefault(
                            functionality_name,
                            Functionality(functionality_name)
                        )
                        implementation = ShellImplementation(command)
                        functionality.add_implementation(implementation)
                elif isinstance(block, InterveningMarkdown):
                    if not re.match(r'^\s*$', block.text(seperator=' ')):
                        last_desc_block = block
            prev_block = block
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
        return repr(self)

    def add_implementation(self, implementation):
        self.implementations.append(implementation)


class Implementation(object):
    """An object representing an implementation (something that is
    used to run a test) in Falderal.

    """
    def __init__(self):
        pass

    def run(self, input=None):
        """Returns the RunResult of running this implementation on the
        given input.

        """
        raise NotImplementedError("subclass needs to implement run()")


class CallableImplementation(Implementation):
    """An implementation which is implemented by a Python callable.

    This is mostly useful for internal tests.

    """
    def __init__(self, callable):
        self.callable = callable

    def run(self, input=None):
        try:
            result = self.callable(input)
            return OutputOutcome(result)
        except Exception as e:
            return ErrorOutcome(str(e))


class ShellImplementation(Implementation):
    def __init__(self, command):
        self.command = command

    def __repr__(self):
        return "ShellImplementation(%r)" % self.name

    def __str__(self):
        return repr(self)

    def run(self, input=None):
        r"""
        >>> i = ShellImplementation('cat')
        >>> i.run('text')
        OutputOutcome('text')

        >>> i = ShellImplementation('cat fhofhofhf')
        >>> i.run('text')
        ErrorOutcome('cat: fhofhofhf: No such file or directory')

        >>> i = ShellImplementation('cat %(test-body-file)')
        >>> i.run('text')
        OutputOutcome('text')

        >>> i = ShellImplementation('echo %(test-body-text)')
        >>> i.run('text')
        OutputOutcome('text')

        >>> i = ShellImplementation('cat >%(output-file)')
        >>> i.run('text')
        OutputOutcome('text')

        """
        # expand variables in the command
        test_filename = None
        output_filename = None
        command = self.command

        # DEPRECATED
        if '%(test-file)' in self.command:
            # choose a temp file name and write the body to that file
            fd, test_filename = mkstemp(dir='.')
            test_filename = basename(test_filename)
            with open(test_filename, 'w') as file:
                file.write(input)
                file.close()
            os.close(fd)
            # replace all occurrences in command
            command = re.sub(r'\%\(test-file\)', test_filename, command)
            input = None
        # DEPRECATED
        if '%(test-text)' in self.command:
            # escape all single quotes in input
            input = re.sub(r"'", r"\'", input)
            # replace all occurrences in command
            command = re.sub(r'\%\(test-text\)', input, command)
            input = None

        # Preferred over test-file
        if '%(test-body-file)' in self.command:
            # choose a temp file name and write the body to that file
            fd, test_filename = mkstemp(dir='.')
            test_filename = basename(test_filename)
            with open(test_filename, 'w') as file:
                file.write(input)
                file.close()
            os.close(fd)
            # replace all occurrences in command
            command = re.sub(r'\%\(test-body-file\)', test_filename, command)
            input = None
        # Preferred over test-text
        if '%(test-body-text)' in self.command:
            # escape all single quotes in input
            input = re.sub(r"'", r"\'", input)
            # replace all occurrences in command
            command = re.sub(r'\%\(test-body-text\)', input, command)
            input = None

        if '%(output-file)' in self.command:
            # choose a temp file name to read output from later
            fd, output_filename = mkstemp(dir='.')
            output_filename = basename(output_filename)
            os.close(fd)
            # replace all occurrences in command
            command = re.sub(r'\%\(output-file\)', output_filename, command)

        # subshell the command and return the output
        pipe = Popen(command, shell=True,
                     stdin=PIPE, stdout=PIPE, stderr=PIPE)
        outputs = pipe.communicate(input=input)
        if pipe.returncode == 0:
            if output_filename is None:
                output = self.normalize_output(outputs[0])
            else:
                f = open(output_filename, 'r')
                output = f.read()
                f.close()
            result = OutputOutcome(output)
        else:
            result = ErrorOutcome(self.normalize_output(outputs[1]))

        # clean up temporary files
        for filename in (test_filename, output_filename):
            if filename is not None:
                os.unlink(filename)
        # finis
        return result

    def normalize_output(self, text):
        text = re.sub(r'\r\n', '\n', text)
        return text.strip('\r\n')

    def __repr__(self):
        return '%s(%r)' % (self.__class__.__name__, self.command)

    def __str__(self):
        return 'shell command "%s"' % self.command


##### Tests #####

class Test(object):
    """An object representing a Falderal test.

    Normally a TestBody block is given as the text_block argument,
    and the body is derived from it.  However in the absence of a
    TestBody block (as in many of the internal tests) a body may
    be passed alone.

    >>> b = TestBody('    |')
    >>> b.append('    | foo')
    >>> b.append('    | bar')
    >>> t = Test(body_block=b)
    >>> print t.body
    foo
    bar

    """
    def __init__(self, body_block=None, input_block=None, expectation=None,
                 functionality=None, desc_block=None, body=None):
        self.body_block = body_block
        self.input_block = input_block
        self.expectation = expectation
        self.functionality = functionality
        self.desc_block = desc_block
        self.body = body
        if self.body is None:
            self.body = self.body_block.text()
    
    def __repr__(self):
        return (
            ("Test(body_block=%r, input_block=%r, expectation=%r, " +
             "functionality=%r, desc_block=%r, body=%r)") %
            (self.body_block, self.input_block, self.expectation,
             self.functionality, self.desc_block, self.body)
        )

    def __str__(self):
        return repr(self)

    def run(self, options=DEFAULT_OPTIONS):
        """Returns a list of Results, one for each implementation of
        the functionality being tested.

        >>> f = Functionality('Cat File')
        >>> f.add_implementation(CallableImplementation(lambda x: x))
        >>> t = Test(body='foo', expectation=OutputOutcome('foo'),
        ...          functionality=f)
        >>> [r.short_description() for r in t.run()]
        ['success']

        >>> f = Functionality('Cat File')
        >>> f.add_implementation(CallableImplementation(lambda x: x))
        >>> t = Test(body='foo', expectation=OutputOutcome('bar'),
        ...          functionality=f)
        >>> [r.short_description() for r in t.run()]
        ["expected OutputOutcome('bar'), got OutputOutcome('foo')"]

        >>> f = Functionality('Cat File')
        >>> f.add_implementation(CallableImplementation(lambda x: x))
        >>> t = Test(body='foo', expectation=ErrorOutcome('foo'),
        ...          functionality=f)
        >>> [r.short_description() for r in t.run()]
        ["expected ErrorOutcome('foo'), got OutputOutcome('foo')"]

        >>> f = Functionality('Cat File')
        >>> def e(x):
        ...     raise ValueError(x)
        >>> f.add_implementation(CallableImplementation(e))
        >>> t = Test(body='foo', expectation=ErrorOutcome('foo'),
        ...          functionality=f)
        >>> [r.short_description() for r in t.run()]
        ['success']

        >>> f = Functionality('Cat File')
        >>> def e(x):
        ...     raise ValueError(x)
        >>> f.add_implementation(CallableImplementation(e))
        >>> t = Test(body='foo', expectation=ErrorOutcome('bar'),
        ...          functionality=f)
        >>> [r.short_description() for r in t.run()]
        ["expected ErrorOutcome('bar'), got ErrorOutcome('foo')"]

        >>> f = Functionality('Cat File')
        >>> def e(x):
        ...     raise ValueError(x)
        >>> f.add_implementation(CallableImplementation(e))
        >>> t = Test(body='foo', expectation=OutputOutcome('foo'),
        ...          functionality=f)
        >>> [r.short_description() for r in t.run()]
        ["expected OutputOutcome('foo'), got ErrorOutcome('foo')"]

        A functionality can have multiple implementations.  We test them all.

        >>> f = Functionality('Cat File')
        >>> def c1(x):
        ...     return x
        >>> def c2(x):
        ...     return x + '...'
        >>> def c3(x):
        ...     raise ValueError(x)
        >>> for c in (c1, c2, c3):
        ...     f.add_implementation(CallableImplementation(c))
        >>> t = Test(body='foo', expectation=OutputOutcome('foo'),
        ...          functionality=f)
        >>> [r.short_description() for r in t.run()]
        ['success', "expected OutputOutcome('foo'), got OutputOutcome('foo...')",
         "expected OutputOutcome('foo'), got ErrorOutcome('foo')"]

        """
        results = []
        for implementation in self.functionality.implementations:
            result = implementation.run(input=self.body)
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
