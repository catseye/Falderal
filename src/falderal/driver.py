""""\
Usage: falderal [<option>...] <filename.markdown>...
"""

from optparse import OptionParser
import sys

from falderal.objects import Document, FalderalSyntaxError


def main(args):
    parser = OptionParser()

    parser.add_option("-b", "--substring-error",
                      action="store_true", default=False,
                      help="match expected errors as substrings")
    parser.add_option("-c", "--clear-functionalities",
                      metavar="NAMES", default=None,
                      help="clear all implementations of the specified "
                           "functionalities that were specified in pragmas "
                           "in documents; useful in conjunction with -f. "
                           "Format of the argument is a colon-seperated "
                           "list of functionality names.")
    parser.add_option("-d", "--dump",
                      action="store_true", default=False,
                      help="print out info about parsed tests, don't run them")
    parser.add_option("-f", "--functionalities",
                      metavar="SPEC",
                      help="specify implementations of functionalies, "
                           "over and above what are specified in pragmas "
                           "in documents (not yet implemented)")
    parser.add_option("-t", "--test",
                      action="store_true", default=False,
                      help="run internal tests and exit")
    parser.add_option("-v", "--verbose",
                      action="store_true", default=False,
                      help="print out info about each test as it is run")

    (options, args) = parser.parse_args(args[1:])

    # for compatibility with previous versions of falderal
    if args and args[0] == 'test':
        args = args[1:]

    if options.test:
        import doctest
        import falderal.objects
        (failure_count, test_count) = \
            doctest.testmod(falderal.objects,
                            optionflags=doctest.NORMALIZE_WHITESPACE)
        if failure_count > 0:
            return 1
        else:
            return 0

    try:
        return test_documents(options, args)
    except FalderalSyntaxError as e:
        # if options.show_full_exception: do that, else
        sys.stderr.write('%s: %s\n' % (e.__class__.__name__, str(e)))
        return 1


def test_documents(options, args):
    # load Falderal documents
    documents = []
    for filename in args:
        documents.append(Document.load(filename))

    # collect functionalities
    # XXX if any implementations are given in the command line,
    # we can put them in here.
    functionalities = {}

    # create Falderal Tests
    tests = []
    for document in documents:
        tests += document.parse_blocks_to_tests(functionalities)

    if options.clear_functionalities:
        for name in options.clear_functionalities.split(':'):
            n = name.strip()
            if n in functionalities:
                functionalities[n].implementations = []

    # XXX lint: check for no tests, or no implementations of a functionality
    # that is being tested, or a functionality that is not being tested, and
    # break with an error unless some option to suppress this is present

    if options.dump:
        print "Functionalities:"
        for name in functionalities:
            print "  " + name
            for implementation in functionalities[name].implementations:
                print "  +-" + str(implementation)
        print "Tests:"
        for test in tests:
            print "  " + str(test)
        return 0

    # run tests
    results = []
    for test in tests:
        if options.verbose:
            print str(test)
        results += test.run(options=options)

    # report on results
    for result in results:
        result.report()
    num_results = len(results)
    num_failures = len([x for x in results if not x.is_successful()])
    print '--------------------------------'
    print 'Total test runs: %d, failures: %d' % (num_results, num_failures)
    print '--------------------------------'

    if num_failures == 0:
        return 0
    else:
        return 1
