#!/usr/bin/env python

"""
Module docstring.
"""

def unindent_docstring(s, n):
    lines = s.split('\n')
    for i, line in enumerate(lines[1:]):
        if line:
            lines[i] = line[n:]
    new_s = '\n'.join(lines)
    return new_s


if __name__ == '__main__':
    import sys
    unindent(sys.argv[1], sys.argv[2])

    sys.exit()
