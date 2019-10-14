""""\
Usage: falderal [<option>...] <filename.markdown>...
"""

from argparse import ArgumentParser
import sys

from falderal.objects import Document, FalderalSyntaxError


##### Exceptions #####

class FalderalLintingError(ValueError):
    pass


##### Main #####

def main(args):
    argparser = ArgumentParser()
    argparser.add_argument('input_files', nargs='+', metavar='FILENAME', type=str,
                           help='Falderal files containing the tests to use')
    argparser.add_argument("-b", "--substring-error",
                           action="store_true", default=False,
                           help="no effect (provided for backwards compatibility)")
    argparser.add_argument("--cavalier",
                           action="store_true", default=False,
                           help="don't perform sanity linting before running tests")
    argparser.add_argument("-d", "--dump",
                           action="store_true", default=False,
                           help="print out info about parsed tests, don't run them")
    argparser.add_argument("-v", "--verbose",
                           action="store_true", default=False,
                           help="print out info about each test as it is run")
    argparser.add_argument('--version', action='version', version="%(prog)s 0.13")

    options = argparser.parse_args(args[1:])

    if not options.substring_error:
        if options.verbose:
            print("NOTE: --substring-error is now default, option has no effect")
        options.substring_error = True

    # load Documents and create Falderal Tests from them
    documents = []
    functionalities = {}
    tests = []
    try:
        for filename in options.input_files:
            documents.append(Document.load(filename))
        for document in documents:
            tests += document.extract_tests(functionalities)

        if not options.cavalier:
            if not documents:
                raise FalderalLintingError("No test documents were specified")
            if not functionalities:
                raise FalderalLintingError(
                    "No functionalities were found in any of the test documents"
                )
            for name in functionalities:
                functionality = functionalities[name]
                if not functionality.implementations:
                    raise FalderalLintingError(
                        "No implementations were found for the functionality '%s'" %
                        functionality.name
                    )
            if not tests:
                raise FalderalLintingError(
                    "No tests were found in any of the test documents"
                )

    except (FalderalSyntaxError, FalderalLintingError) as e:
        # if options.show_full_exception: do that, else
        sys.stderr.write('%s: %s\n' % (e.__class__.__name__, str(e)))
        return 1

    if options.dump:
        print("Functionalities:")
        for name in functionalities:
            print("  " + name)
            for implementation in functionalities[name].implementations:
                print("  +-" + str(implementation))
        print("Tests:")
        for test in tests:
            print("  " + str(test))
        return 0

    # run tests
    results = []
    dup_check = {}
    all_ran = False
    try:
        for test in tests:
            if options.verbose:
                print(str(test))
            these_results = test.run(options=options)
            if options.verbose:
                for result in these_results:
                    key = repr(test.body_block.text()) + repr(result.implementation)
                    location = "%s, line %d" % (
                        test.body_block.filename, test.body_block.line_num
                    )
                    dup_check.setdefault(key, []).append(location)
            results += these_results
        all_ran = True
    except KeyboardInterrupt:
        pass

    if options.verbose:
        for key in dup_check:
            if len(dup_check[key]) != 1:
                print("WARNING: test/impl combination %s was run %d times %r" % (
                    key, len(dup_check[key]), dup_check[key]
                ))

    # report on results
    for result in results:
        result.report()
    num_results = len(results)
    num_failures = len([x for x in results if not x.is_successful()])
    if not all_ran:
        print('**************************************************************')
        print('** TESTING TERMINATED PREMATURELY -- NOT ALL TESTS WERE RUN **')
        print('**************************************************************')

    print('--------------------------------')
    print('Total test runs: %d, failures: %d' % (num_results, num_failures))
    print('--------------------------------')

    if num_failures == 0:
        return 0
    else:
        return 1
