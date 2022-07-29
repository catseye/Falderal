import sys

try:
    input = raw_input
except:
    pass

program = open(sys.argv[1]).read().split('\n')

vars = {}
for line in program:
    if line.startswith('print '):
        var = line[6]
        print(vars.get(var, ''))
    if line.startswith('read '):
        var = line[5]
        vars[var] = input('')
