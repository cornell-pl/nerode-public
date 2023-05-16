#!/usr/bin/python3
import sys

strings = []

ALPHABET_SIZE = 2

for line in sys.stdin:
    spl = line.strip().split(',')
    memb = '0' if spl[1] == '-' else '1'

    # Need to expand X as each char in {0, 1}
    for i in range(2 ** spl[0].count('X')):
        t = []
        m = i
        for x in spl[0]:
            if x == 'X':
                t.append(m % 2)
                m = int(m / 2)
            else:
                t.append(x)
        strings.append((memb, t))

print('{} {}'.format(len(strings), ALPHABET_SIZE))

for s in strings:
    print('{} {} '.format(s[0], len(s[1])), end='')
    for x in s[1]:
        print(x, end=' ')
    print('')
