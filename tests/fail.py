import sys

sys.stdout.write(sys.argv[1] + '\n')
sys.stdout.flush()
sys.stderr.write(sys.argv[2] + '\n')
sys.exit(int(sys.argv[3]))
