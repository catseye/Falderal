import sys

input = sys.stdin
output = sys.stdout

if len(sys.argv) > 1 and sys.argv[1] == '-f':
    input = open(sys.argv[2], 'r')
    sys.argv = sys.argv[2:]

if len(sys.argv) > 1 and sys.argv[1] == '-o':
    output = open(sys.argv[2], 'w')
    sys.argv = sys.argv[2:]

output.write(input.read())
