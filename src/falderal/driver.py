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
    parser.add_option("-d", "--dump",
                      action="store_true", default=False,
                      help="print out info about parsed tests, don't run them")
    parser.add_option("-t", "--test",
                      action="store_true", default=False,
                      help="run internal tests only and exit")
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

    # load Documents and create Falderal Tests from them
    documents = []
    functionalities = {}
    tests = []
    try:
        for filename in args:
            documents.append(Document.load(filename))
        for document in documents:
            tests += document.parse_blocks_to_tests(functionalities)
    except FalderalSyntaxError as e:
        # if options.show_full_exception: do that, else
        sys.stderr.write('%s: %s\n' % (e.__class__.__name__, str(e)))
        return 1

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
