#!/usr/bin/env python3

import jedi


# https://jedi.readthedocs.io/en/latest/docs/api.html#jedi.Script
def completions(source, line, column, path):
    script = jedi.Script(source, line=line, column=column)
    completions = script.completions()
    return [i.complete for i in completions]


def main():
    import sys
    import os
    try:
        path, line, column = sys.argv[1:]
        path = os.path.expanduser(path)
        line = int(line)
        column = int(column)
        # Read from STDIN.  sys.stdin.read() does not work in Python 3        
        source = ''.join(l for l in sys.stdin)
        if source == '':
            raise ValueError
    except ValueError:
        sys.stderr.write(f'Usage: {__file__} FILE LINE COLUMN <FILE\n')
        sys.exit(1)

    for c in completions(source, line, column, path):
        print(c)

if __name__ == '__main__':
    main()
