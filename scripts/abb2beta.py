#!/usr/bin/python3
import sys

sys.stdin.readline()
for line in sys.stdin:
    spl = line.split()
    print(''.join(spl[2:]) + ',' + ('+' if spl[0] == '1' else '-'))
