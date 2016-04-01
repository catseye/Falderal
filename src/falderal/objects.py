import codecs
import os
import re
from subprocess import Popen, PIPE
from tempfile import mkstemp

# Note: the __unicode__ method of all the classes defined herein should
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


class OutputOutcome(Outcome):
    def __unicode__(self):
        return u'output:\n' + self.text


class ErrorOutcome(Outcome):
    def __unicode__(self):
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

    def report(self):
        print "FAILED  : " + self.format_text_block(self.test.description)
        print "Location: " + self.test.body_block.location()
        print "Function: " + self.format_text_block(self.test.functionality.name)
        print "Impl    : " + self.format_text_block(self.implementation)
        print "Body    : " + self.format_text_block(self.test.body)
        #if input is not None:
        #print "Input   : " + self.format_text_block(self.test.input)
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

    >>> b = Block()
    >>> b.append(u'line 1')
    >>> b.append(u'line 2')
    >>> print b.text()
    line 1
    line 2
    >>> print b.text(seperator='')
    line 1line 2
    >>> print b.deconstruct()
    [('', [u'line 1', u'line 2'])]
    
    >>> b = Block()
    >>> b.append(u'-> This is a pragma.')
    >>> b.append(u"| This is some test input.")
    >>> b.append(u"| It extends over two lines.")
    >>> b.append(u'? Expected Error')
    >>> b.append(u'Plain text')
    >>> b.append(u'More plain text')
    >>> b.append(u'| Test with input')
    >>> b.append(u'+ input-for-test')
    >>> b.append(u'= Expected result on output')
    >>> b.append(u'= which extends over two lines')
    >>> print [pair[0] for pair in b.deconstruct()]
    [u'->', u'| ', u'? ', '', u'| ', u'+ ', u'= ']
    
    >>> b = Block()
    >>> b.append(u'-> This is a pragma.')
    >>> b.append(u'-> which extends over two lines')
    >>> print b.classify(ParseState())
    Pragma(line_num=1)

    >>> f = Functionality('foo')
    >>> b = Block()
    >>> b.append(u'| Test body here.')
    >>> b.append(u'= Expected result here.')
    >>> print b.classify(ParseState(current_functionality=f))
    Test(body_block=Block(line_num=1), input_block=None,
         expectation=OutputOutcome(u'Expected result here.'),
         functionality=Functionality('foo'), desc_block=None,
         body=u'Test body here.', input=None)
 
    >>> b = Block()
    >>> b.append(u'| Test body here.')
    >>> b.append(u'? Expected error here.')
    >>> print b.classify(ParseState(current_functionality=f))
    Test(body_block=Block(line_num=1), input_block=None,
         expectation=ErrorOutcome(u'Expected error here.'),
         functionality=Functionality('foo'), desc_block=None,
         body=u'Test body here.', input=None)

    """

    PREFIXES = [
        u'==> ',
        u'??> ',
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

    def __unicode__(self):
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

        def reconstruct(pattern, except_prefixes):
            new_pattern = []
            lines = []
            for (candidate_prefix, candidate_lines) in pattern:
                if candidate_prefix in except_prefixes:
                    new_pattern.append((candidate_prefix, candidate_lines))
                else:
                    lines.extend([candidate_prefix + line for line in candidate_lines])
            return [(u'', lines)] + new_pattern

        if '' in pattern_prefixes:
            # There is plain, non-prefixed text embedded somewhere in this Block.
            # We interpret this according to the new, not-yet-written rules.
            if pattern_prefixes[-1] in [[u'==> ', u'??> ']]:
                pattern = reconstruct(pattern, [u'==> ', u'??> '])
                pattern_prefixes = [p[0] for p in pattern]
            else:
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
        r"""Parse the lines of the Document into Blocks.

        >>> d = Document()
        >>> d.append(u'This is a test file.')
        >>> d.append(u'    -> This is a pragma.')
        >>> d.append(u'')
        >>> d.append(u"    | This is some test input.\n")
        >>> d.append(u"    | It extends over two lines.")
        >>> d.append(u'    ? Expected Error')
        >>> d.append(u'')
        >>> d.append(u'    | Test with input')
        >>> d.append(u'    + input-for-test')
        >>> d.append(u'    = Expected result on output')
        >>> blocks = d.parse_lines_to_blocks()
        >>> [block.lines for block in blocks if isinstance(block, InterveningText)]
        [[u'This is a test file.'], [u''], [u'']]
        >>> [b.__class__.__name__ for b in blocks]
        ['InterveningText', 'Block', 'InterveningText', 'Block', 'InterveningText', 'Block']
        >>> [b.line_num for b in blocks]
        [1, 2, 3, 4, 7, 8]

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
        r"""Extract all Tests from this Document.

        >>> functionalities = {}
        >>> d = Document()
        >>> d.append(u"This is a text file.")
        >>> d.append(u'It contains NO tests.')
        >>> d.extract_tests(functionalities)
        []

        >>> d = Document()
        >>> d.append(u'This is a test file.')
        >>> d.append(u'    -> Tests for functionality "Parse Thing"')
        >>> d.append(u'')
        >>> d.append(u"    | This is some test body.")
        >>> d.append(u'    = Expected result')
        >>> d.extract_tests(functionalities)
        [Test(body_block=Block(line_num=4), input_block=None,
              expectation=OutputOutcome(u'Expected result'),
              functionality=Functionality(u'Parse Thing'),
              desc_block=InterveningText(line_num=1),
              body=u'This is some test body.', input=None)]

        >>> d = Document()
        >>> d.append(u'This is a test file.')
        >>> d.append(u'    -> Tests for functionality "Parse Thing"')
        >>> d.append(u'')
        >>> d.append(u"    | This is some test body.")
        >>> d.append(u"    | It extends over two lines.")
        >>> d.append(u'    ? Expected Error')
        >>> d.append(u'')
        >>> d.append(u'    | Test with input')
        >>> d.append(u'    + input-for-test')
        >>> d.append(u'    = Expected result on output')
        >>> d.append(u'')
        >>> d.append(u'    + Other input-for-test')
        >>> d.append(u'    = Other Expected result on output')
        >>> d.append(u'')
        >>> d.append(u'    -> Tests for functionality "Run Thing"')
        >>> d.append(u'')
        >>> d.append(u"    | Thing")
        >>> d.append(u'    ? Oops')
        >>> tests = d.extract_tests(functionalities)
        >>> [t.body for t in tests]
        [u'This is some test body.\nIt extends over two lines.',
         u'Test with input', u'Test with input', u'Thing']
        >>> [t.input_block for t in tests]
        [None, Block(line_num=8), Block(line_num=12), None]
        >>> tests[1].input_block.text()
        u'input-for-test'
        >>> tests[2].input_block.text()
        u'Other input-for-test'
        >>> [t.expectation for t in tests]
        [ErrorOutcome(u'Expected Error'),
         OutputOutcome(u'Expected result on output'),
         OutputOutcome(u'Other Expected result on output'),
         ErrorOutcome(u'Oops')]
        >>> [t.functionality.name for t in tests]
        [u'Parse Thing', u'Parse Thing', u'Parse Thing', u'Run Thing']
        >>> sorted(functionalities.keys())
        [u'Parse Thing', u'Run Thing']

        >>> d = Document()
        >>> d.append(u"    | This is some test body.")
        >>> d.append(u'    = Expected')
        >>> d.extract_tests({})
        Traceback (most recent call last):
        ...
        FalderalSyntaxError: line 1: functionality under test not specified

        >>> d = Document()
        >>> d.append(u'This is a test file.')
        >>> d.append(u'    ? Expected Error')
        >>> d.extract_tests({})
        Traceback (most recent call last):
        ...
        FalderalSyntaxError: line 2: expectation must be preceded by test body or test input

        >>> d = Document()
        >>> d.append(u'    -> Hello, this is pragma')
        >>> d.append(u'    = Expected')
        >>> d.extract_tests({})
        Traceback (most recent call last):
        ...
        FalderalSyntaxError: line 1: incorrectly formatted test block

        >>> d = Document()
        >>> d.append(u'    | This is test')
        >>> d.append(u'This is text')
        >>> d.extract_tests({})
        Traceback (most recent call last):
        ...
        FalderalSyntaxError: line 1: test body must be followed by expectation or test input

        >>> d = Document()
        >>> d.append(u'    -> Hello, this is pragma')
        >>> d.append(u'    + Input to where exactly?')
        >>> d.extract_tests({})
        Traceback (most recent call last):
        ...
        FalderalSyntaxError: line 1: incorrectly formatted test block

        >>> d = Document()
        >>> funs = {}
        >>> d.append(u'    -> Functionality "Parse Stuff" is implemented by '
        ...          u'shell command "parse"')
        >>> d.append(u'')
        >>> d.append(u'    -> Functionality "Parse Stuff" is')
        >>> d.append(u'    -> implemented by shell command "pxxxy"')
        >>> tests = d.extract_tests(funs)
        >>> len(funs.keys())
        1
        >>> [i for i in funs["Parse Stuff"].implementations]
        [ShellImplementation(u'parse'), ShellImplementation(u'pxxxy')]

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

    def __unicode__(self):
        return unicode(repr(self))

    def add_implementation(self, implementation):
        self.implementations.append(implementation)


class Implementation(object):
    """An object representing an implementation (something that is
    used to run a test) in Falderal.

    """
    def __init__(self):
        pass

    def run(self, body=None, input=None):
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

    def __unicode__(self):
        return u'callable "%r"' % self.callable

    def run(self, body=None, input=None):
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

    def __unicode__(self):
        return u'shell command "%s"' % self.command

    def run(self, body=None, input=None):
        r"""
        >>> i = ShellImplementation('cat')
        >>> i.run(body=u'text')
        OutputOutcome(u'text')

        >>> i = ShellImplementation('cat fhofhofhf')
        >>> i.run(body=u'text')
        ErrorOutcome(u'cat: fhofhofhf: No such file or directory')

        >>> i = ShellImplementation('cat %(test-body-file)')
        >>> i.run(body=u'text')
        OutputOutcome(u'text')

        >>> i = ShellImplementation("echo '%(test-body-text)'")
        >>> i.run(body=u'text')
        OutputOutcome(u'text')

        >>> i = ShellImplementation('cat >%(output-file)')
        >>> i.run(body=u'text')
        OutputOutcome(u'text')

        >>> i = ShellImplementation("echo '%(test-body-text)' '%(test-input-text)'")
        >>> i.run(body=u'text', input=u'zzrk')
        OutputOutcome(u'text zzrk')

        Here the body is sent to cat's stdin, but cat ignores it.
        
        >>> i = ShellImplementation('cat >%(output-file) <%(test-input-file)')
        >>> i.run(body=u'text', input=u'zzrk')
        OutputOutcome(u'zzrk')

        """
        # expand variables in the command
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
            command = re.sub(r'\%\(test-body-file\)', test_filename, command)
            command_contained_test_body_file = True

        if '%(test-body-text)' in self.command:
            # escape all single quotes in body
            body = re.sub(r"'", r"\'", body)
            # replace all occurrences in command
            command = re.sub(r'\%\(test-body-text\)', body, command)
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
            command = re.sub(r'\%\(test-input-file\)', test_input_filename, command)
            command_contained_test_input_file = True

        if '%(test-input-text)' in self.command:
            # escape all single quotes in input
            body = re.sub(r"'", r"\'", body)
            # replace all occurrences in command
            command = re.sub(r'\%\(test-input-text\)', input, command)
            command_contained_test_input_text = True

        if '%(output-file)' in self.command:
            # choose a temp file name to read output from later
            fd, output_filename = mkstemp()
            os.close(fd)
            # replace all occurrences in command
            command = re.sub(r'\%\(output-file\)', output_filename, command)

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
        text = text.decode('UTF-8', errors='ignore')
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

    >>> b = Block()
    >>> b.append(u'foo')
    >>> b.append(u'bar')
    >>> i = Block()
    >>> i.append(u'green')
    >>> t = Test(body_block=b, input_block=i)
    >>> print t.body
    foo
    bar
    >>> print t.input
    green

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

        >>> f = Functionality('Cat File')
        >>> f.add_implementation(CallableImplementation(lambda x, y: x))
        >>> t = Test(body=u'foo', expectation=OutputOutcome(u'foo'),
        ...          functionality=f)
        >>> [r.short_description() for r in t.run()]
        ['success']

        >>> f = Functionality('Cat File')
        >>> f.add_implementation(CallableImplementation(lambda x, y: x))
        >>> t = Test(body=u'foo', expectation=OutputOutcome(u'bar'),
        ...          functionality=f)
        >>> [r.short_description() for r in t.run()]
        ["expected OutputOutcome(u'bar'), got OutputOutcome(u'foo')"]

        >>> f = Functionality('Cat File')
        >>> f.add_implementation(CallableImplementation(lambda x, y: x))
        >>> t = Test(body=u'foo', expectation=ErrorOutcome(u'foo'),
        ...          functionality=f)
        >>> [r.short_description() for r in t.run()]
        ["expected ErrorOutcome(u'foo'), got OutputOutcome(u'foo')"]

        >>> f = Functionality('Cat File')
        >>> def e(x, y):
        ...     raise ValueError(x)
        >>> f.add_implementation(CallableImplementation(e))
        >>> t = Test(body=u'foo', expectation=ErrorOutcome(u'foo'),
        ...          functionality=f)
        >>> [r.short_description() for r in t.run()]
        ['success']

        >>> f = Functionality('Cat File')
        >>> def e(x, y):
        ...     raise ValueError(x)
        >>> f.add_implementation(CallableImplementation(e))
        >>> t = Test(body=u'foo', expectation=ErrorOutcome(u'bar'),
        ...          functionality=f)
        >>> [r.short_description() for r in t.run()]
        ["expected ErrorOutcome(u'bar'), got ErrorOutcome(u'foo')"]

        >>> f = Functionality('Cat File')
        >>> def e(x, y):
        ...     raise ValueError(x)
        >>> f.add_implementation(CallableImplementation(e))
        >>> t = Test(body=u'foo', expectation=OutputOutcome(u'foo'),
        ...          functionality=f)
        >>> [r.short_description() for r in t.run()]
        ["expected OutputOutcome(u'foo'), got ErrorOutcome(u'foo')"]

        >>> f = Functionality('Cat File with Input')
        >>> f.add_implementation(CallableImplementation(lambda x, y: x + y))
        >>> t = Test(body=u'foo', input=u'bar', expectation=OutputOutcome(u'foobar'),
        ...          functionality=f)
        >>> [r.short_description() for r in t.run()]
        ['success']

        A functionality can have multiple implementations.  We test them all.

        >>> f = Functionality('Cat File')
        >>> def c1(body, input):
        ...     return body
        >>> def c2(body, input):
        ...     return body + '...'
        >>> def c3(body, input):
        ...     raise ValueError(body)
        >>> for c in (c1, c2, c3):
        ...     f.add_implementation(CallableImplementation(c))
        >>> t = Test(body=u'foo', expectation=OutputOutcome(u'foo'),
        ...          functionality=f)
        >>> [r.short_description() for r in t.run()]
        ['success', "expected OutputOutcome(u'foo'), got OutputOutcome(u'foo...')",
         "expected OutputOutcome(u'foo'), got ErrorOutcome(u'foo')"]

        """
        results = []
        for implementation in self.functionality.implementations:
            result = implementation.run(body=self.body, input=self.input)
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
