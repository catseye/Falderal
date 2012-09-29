import sys

if sys.argv[1] == '-n':
    sys.stdout.write(sys.argv[2])
else:
    sys.stdout.write(sys.argv[1] + '\n')
