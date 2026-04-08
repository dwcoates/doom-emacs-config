#!/usr/bin/env python3
"""Check parenthesis balance in an Emacs Lisp file, skipping strings and comments.
Reports every top-level form that is unbalanced."""
import sys
import re

def check_parens(path):
    text = open(path).read()
    lines = text.split('\n')

    in_string = False
    in_comment = False
    escape = False
    depth = 0
    form_start_line = 1
    errors = []

    for i, c in enumerate(text):
        line_no = text[:i].count('\n') + 1

        if in_comment:
            if c == '\n':
                in_comment = False
            continue
        if escape:
            escape = False
            continue
        if c == '\\':
            if in_string:
                escape = True
            continue
        if c == '"' and not in_string:
            in_string = True
            continue
        if c == '"' and in_string:
            in_string = False
            continue
        if in_string:
            continue
        if c == ';':
            in_comment = True
            continue

        if c == '(':
            if depth == 0:
                form_start_line = line_no
            depth += 1
        elif c == ')':
            depth -= 1
            if depth < 0:
                errors.append(f'line {line_no}: unmatched )')
                depth = 0  # reset and keep scanning
            elif depth == 0:
                pass  # clean top-level form end

    if depth != 0:
        errors.append(f'end of file: {depth} unclosed paren(s) — last form started at line {form_start_line}')

    if errors:
        for e in errors:
            # show context for line-based errors
            m = re.match(r'line (\d+)', e)
            if m:
                ln = int(m.group(1))
                for l in range(max(0, ln - 3), min(len(lines), ln + 1)):
                    print(f'  {l+1}: {lines[l]}')
            print(f'ERROR: {e}')
        return 1

    print('OK: balanced')
    return 0

if __name__ == '__main__':
    if len(sys.argv) != 2:
        print(f'Usage: {sys.argv[0]} <file.el>')
        sys.exit(1)
    sys.exit(check_parens(sys.argv[1]))
