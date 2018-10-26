# Note: these are unit tests for py-falderal itself,
# not tests that Falderal can understand.

import unittest
from unittest import TestCase

from falderal.objects import (
    Block, Pragma,
    ParseState, InterveningText,
    Document,
    Functionality, ShellImplementation,
    Test, OutputOutcome, ErrorOutcome,
    FalderalSyntaxError,
)


class BlockTestCase(TestCase):
    def test_block(self):
        b = Block()
        b.append(u'line 1')
        b.append(u'line 2')
        self.assertEqual(b.text(), "line 1\nline 2")
        self.assertEqual(b.text(seperator=''), "line 1line 2")
        self.assertEqual(
            b.deconstruct(),
            [('', [u'line 1', u'line 2'])]
        )

    def test_deconstruct_block(self):
        b = Block()
        b.append(u'-> This is a pragma.')
        b.append(u"| This is some test input.")
        b.append(u"| It extends over two lines.")
        b.append(u'? Expected Error')
        b.append(u'Plain text')
        b.append(u'More plain text')
        b.append(u'| Test with input')
        b.append(u'+ input-for-test')
        b.append(u'= Expected result on output')
        b.append(u'= which extends over two lines')
        self.assertEqual(
            [pair[0] for pair in b.deconstruct()],
            [u'->', u'| ', u'? ', '', u'| ', u'+ ', u'= ']
        )

    def test_classify_block_pragma(self):
        b = Block()
        b.append(u'-> This is a pragma.')
        b.append(u'-> which extends over two lines')
        result = b.classify(ParseState())
        self.assertIsInstance(result, Pragma)
        self.assertEqual(result.lines, [u' This is a pragma.', u' which extends over two lines'])
        self.assertEqual(result.line_num, 1)

    def test_classify_block_success_test(self):
        f = Functionality('foo')
        b = Block()
        b.append(u'| Test body here.')
        b.append(u'= Expected result here.')
        result = b.classify(ParseState(current_functionality=f))
        self.assertIsInstance(result, Test)
        self.assertEqual(result.body_block.lines, [u'Test body here.'])
        self.assertEqual(result.body_block.line_num, 1)
        self.assertEqual(result.body_block.filename, None)
        self.assertEqual(result.input_block, None)
        self.assertEqual(result.expectation, OutputOutcome(u'Expected result here.'))
        self.assertEqual(result.functionality, f)
        self.assertEqual(result.desc_block, None)
        self.assertEqual(result.body, u'Test body here.')
        self.assertEqual(result.input, None)

    def test_classify_block_error_test(self):
        f = Functionality('foo')
        b = Block()
        b.append(u'| Test body here.')
        b.append(u'? Expected error here.')
        result = b.classify(ParseState(current_functionality=f))
        self.assertIsInstance(result, Test)
        self.assertEqual(result.body_block.lines, [u'Test body here.'])
        self.assertEqual(result.body_block.line_num, 1)
        self.assertEqual(result.body_block.filename, None)
        self.assertEqual(result.input_block, None)
        self.assertEqual(result.expectation, ErrorOutcome(u'Expected error here.'))
        self.assertEqual(result.functionality, f)
        self.assertEqual(result.desc_block, None)
        self.assertEqual(result.body, u'Test body here.')
        self.assertEqual(result.input, None)


class DocumentTestCase(TestCase):
    def test_document(self):
        d = Document()
        d.append(u'This is a test file.')
        d.append(u'    -> This is a pragma.')
        d.append(u'')
        d.append(u"    | This is some test input.\n")
        d.append(u"    | It extends over two lines.")
        d.append(u'    ? Expected Error')
        d.append(u'')
        d.append(u'    | Test with input')
        d.append(u'    + input-for-test')
        d.append(u'    = Expected result on output')
        blocks = d.parse_lines_to_blocks()
        self.assertEqual(
            [block.lines for block in blocks if isinstance(block, InterveningText)],
            [[u'This is a test file.'], [u''], [u'']]
        )
        self.assertEqual(
            [b.__class__.__name__ for b in blocks],
            ['InterveningText', 'Block', 'InterveningText', 'Block', 'InterveningText', 'Block']
        )
        self.assertEqual(
            [b.line_num for b in blocks],
            [1, 2, 3, 4, 7, 8]
        )

    def test_extract_tests_empty(self):
        d = Document()
        d.append(u"This is a text file.")
        d.append(u'It contains NO tests.')
        functionalities = {}
        self.assertEqual(d.extract_tests(functionalities), [])

    def test_extract_tests_basic(self):
        d = Document()
        d.append(u'This is a test file.')
        d.append(u'    -> Tests for functionality "Parse Thing"')
        d.append(u'')
        d.append(u"    | This is some test body.")
        d.append(u'    = Expected result')
        functionalities = {}
        tests = d.extract_tests(functionalities)
        self.assertEqual(len(tests), 1)
        result = tests[0]
        self.assertIsInstance(result, Test)
        self.assertEqual(result.body_block.lines, [u'This is some test body.'])
        self.assertEqual(result.body_block.line_num, 4)
        self.assertEqual(result.body_block.filename, None)
        self.assertEqual(result.input_block, None)
        self.assertEqual(result.expectation, OutputOutcome(u'Expected result'))
        self.assertEqual(result.functionality, functionalities['Parse Thing'])
        self.assertEqual(result.desc_block.__class__, InterveningText)
        self.assertEqual(result.desc_block.lines, [u'This is a test file.'])
        self.assertEqual(result.body, u'This is some test body.')
        self.assertEqual(result.input, None)

    def test_extract_tests_more(self):
        d = Document()
        d.append(u'This is a test file.')
        d.append(u'    -> Tests for functionality "Parse Thing"')
        d.append(u'')
        d.append(u"    | This is some test body.")
        d.append(u"    | It extends over two lines.")
        d.append(u'    ? Expected Error')
        d.append(u'')
        d.append(u'    | Test with input')
        d.append(u'    + input-for-test')
        d.append(u'    = Expected result on output')
        d.append(u'')
        d.append(u'    + Other input-for-test')
        d.append(u'    = Other Expected result on output')
        d.append(u'')
        d.append(u'    -> Tests for functionality "Run Thing"')
        d.append(u'')
        d.append(u"    | Thing")
        d.append(u'    ? Oops')
        functionalities = {}
        tests = d.extract_tests(functionalities)
        self.assertEqual(
            [t.body for t in tests],
            [u'This is some test body.\nIt extends over two lines.',
             u'Test with input', u'Test with input', u'Thing']
        )
        self.assertEqual(
            [t.input_block.__class__ for t in tests],
            [None.__class__, Block, Block, None.__class__]
        )
        self.assertEqual(
            [t.input_block.text() for t in tests if t.input_block is not None],
            [u'input-for-test', u'Other input-for-test']
        )
        self.assertEqual(
            [t.expectation for t in tests],
            [ErrorOutcome(u'Expected Error'),
             OutputOutcome(u'Expected result on output'),
             OutputOutcome(u'Other Expected result on output'),
             ErrorOutcome(u'Oops')]
        )
        self.assertEqual(
            [t.functionality.name for t in tests],
            [u'Parse Thing', u'Parse Thing', u'Parse Thing', u'Run Thing']
        )
        self.assertEqual(
            sorted(functionalities.keys()),
            [u'Parse Thing', u'Run Thing']
        )

    def test_no_functionality_under_test(self):
        d = Document()
        d.append(u"    | This is some test body.")
        d.append(u'    = Expected')
        with self.assertRaises(FalderalSyntaxError) as ar:
            d.extract_tests({})
        self.assertEqual(str(ar.exception), "line 1: functionality under test not specified")

    def test_expectation_in_bad_place(self):
        d = Document()
        d.append(u'This is a test file.')
        d.append(u'    ? Expected Error')
        with self.assertRaises(FalderalSyntaxError) as ar:
            d.extract_tests({})
        self.assertEqual(str(ar.exception), "line 2: expectation must be preceded by test body or test input")

    def test_badly_formatted_test_block(self):
        d = Document()
        d.append(u'    -> Hello, this is pragma')
        d.append(u'    = Expected')
        with self.assertRaises(FalderalSyntaxError) as ar:
            d.extract_tests({})
        self.assertEqual(str(ar.exception), "line 1: incorrectly formatted test block")

    def test_body_not_followed_by_anything_sensible(self):
        d = Document()
        d.append(u'    | This is test')
        d.append(u'This is text')
        with self.assertRaises(FalderalSyntaxError) as ar:
            d.extract_tests({})
        self.assertEqual(str(ar.exception), "line 1: test body must be followed by expectation or test input")

    def test_another_badly_formatted_block(self):
        d = Document()
        d.append(u'    -> Hello, this is pragma')
        d.append(u'    + Input to where exactly?')
        with self.assertRaises(FalderalSyntaxError) as ar:
            d.extract_tests({})
        self.assertEqual(str(ar.exception), "line 1: incorrectly formatted test block")

    def test_parse_functionalities(self):
        d = Document()
        funs = {}
        d.append(u'    -> Functionality "Parse Stuff" is implemented by '
                 u'shell command "parse"')
        d.append(u'')
        d.append(u'    -> Functionality "Parse Stuff" is')
        d.append(u'    -> implemented by shell command "pxxxy"')
        tests = d.extract_tests(funs)
        self.assertEqual(list(funs.keys()), ['Parse Stuff'])
        self.assertEqual(
            [i for i in funs["Parse Stuff"].implementations],
            [ShellImplementation(u'parse'), ShellImplementation(u'pxxxy')]
        )


class ShellImplementationTestCase(TestCase):
    def test_cat(self):
        i = ShellImplementation('cat')
        self.assertEqual(i.run(body=u'text'), OutputOutcome(u'text'))

    def test_cat_file(self):
        i = ShellImplementation('cat fhofhofhf')
        self.assertEqual(i.run(body=u'text'), ErrorOutcome(u'cat: fhofhofhf: No such file or directory'))

    def test_cat_test_body_file(self):
        i = ShellImplementation('cat %(test-body-file)')
        self.assertEqual(i.run(body=u'text'), OutputOutcome(u'text'))

    def test_cat_test_body_text(self):
        i = ShellImplementation("echo '%(test-body-text)'")
        self.assertEqual(i.run(body=u'text'), OutputOutcome(u'text'))

    def test_cat_output_file(self):
        i = ShellImplementation('cat >%(output-file)')
        self.assertEqual(i.run(body=u'text'), OutputOutcome(u'text'))

    def test_echo(self):
        i = ShellImplementation("echo '%(test-body-text)' '%(test-input-text)'")
        self.assertEqual(i.run(body=u'text', input=u'zzrk'), OutputOutcome(u'text zzrk'))

    def test_cat_stdin(self):
        # Here the body is sent to cat's stdin, but cat ignores it.
        i = ShellImplementation('cat >%(output-file) <%(test-input-file)')
        self.assertEqual(i.run(body=u'text', input=u'zzrk'), OutputOutcome(u'zzrk'))


def TestTestCase(TestCase):
    def test_test_contents(self):
        b = Block()
        b.append(u'foo')
        b.append(u'bar')
        i = Block()
        i.append(u'green')
        t = Test(body_block=b, input_block=i)
        self.assertEqual(t.body, "foo\nbar")
        self.assertEqual(t.input, "green")

    def test_tests_1(self):
        f = Functionality('Cat File')
        f.add_implementation(CallableImplementation(lambda x, y: x))
        t = Test(body=u'foo', expectation=OutputOutcome(u'foo'), functionality=f)
        self.assertEqual(
            [r.short_description() for r in t.run()]
            ['success']
        )

    def test_tests_2(self):
        f = Functionality('Cat File')
        f.add_implementation(CallableImplementation(lambda x, y: x))
        t = Test(body=u'foo', expectation=OutputOutcome(u'bar'),
                  functionality=f)
        self.assertEqual(
            [r.short_description() for r in t.run()],
            ["expected OutputOutcome(u'bar'), got OutputOutcome(u'foo')"]
        )

    def test_tests_3(self):
        f = Functionality('Cat File')
        f.add_implementation(CallableImplementation(lambda x, y: x))
        t = Test(body=u'foo', expectation=ErrorOutcome(u'foo'),
                 functionality=f)
        self.assertEqual(
            [r.short_description() for r in t.run()],
            ["expected ErrorOutcome(u'foo'), got OutputOutcome(u'foo')"]
        )

    def test_tests_4(self):
        f = Functionality('Cat File')
        def e(x, y):
            raise ValueError(x)
        f.add_implementation(CallableImplementation(e))
        t = Test(body=u'foo', expectation=ErrorOutcome(u'foo'),
                 functionality=f)
        self.assertEqual(
            [r.short_description() for r in t.run()],
            ['success']
        )

    def test_tests_5(self):
        f = Functionality('Cat File')
        def e(x, y):
            raise ValueError(x)
        f.add_implementation(CallableImplementation(e))
        t = Test(body=u'foo', expectation=ErrorOutcome(u'bar'),
                 functionality=f)
        self.assertEqual(
            [r.short_description() for r in t.run()],
            ["expected ErrorOutcome(u'bar'), got ErrorOutcome(u'foo')"]
        )

    def test_tests_6(self):
        f = Functionality('Cat File')
        def e(x, y):
            raise ValueError(x)
        f.add_implementation(CallableImplementation(e))
        t = Test(body=u'foo', expectation=OutputOutcome(u'foo'),
                 functionality=f)
        self.assertEqual(
            [r.short_description() for r in t.run()],
            ["expected OutputOutcome(u'foo'), got ErrorOutcome(u'foo')"]
        )

    def test_tests_7(self):
        f = Functionality('Cat File with Input')
        f.add_implementation(CallableImplementation(lambda x, y: x + y))
        t = Test(body=u'foo', input=u'bar', expectation=OutputOutcome(u'foobar'),
                 functionality=f)
        self.assertEqual(
            [r.short_description() for r in t.run()],
            ['success']
        )

    def test_functionality_with_multiple_implementations(self):
        # A functionality can have multiple implementations.  We test them all.

        f = Functionality('Cat File')
        def c1(body, input):
            return body
        def c2(body, input):
            return body + '...'
        def c3(body, input):
            raise ValueError(body)
        for c in (c1, c2, c3):
            f.add_implementation(CallableImplementation(c))
        t = Test(body=u'foo', expectation=OutputOutcome(u'foo'),
                 functionality=f)
        self.assertEqual(
            [r.short_description() for r in t.run()],
            ['success', "expected OutputOutcome(u'foo'), got OutputOutcome(u'foo...')",
             "expected OutputOutcome(u'foo'), got ErrorOutcome(u'foo')"]
         )


if __name__ == '__main__':
    unittest.main()
