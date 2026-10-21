// Package pyedit reads the file edits a shell command states through python:
// the `python3 - <<'EOF'` script an agent writes in place of an Edit call,
// which reads a file, replaces literal old text with literal new text, and
// writes it back.
//
// THE RULE IS NARROW ON PURPOSE. A command states edits only when every python
// program it runs is a bare here-document this package reads WHOLE, in a
// subset with no control flow: imports, names bound to string literals and
// paths, `Path.read_text()` / `open().read()`, `str.replace(old, new[, 1])`
// on known strings, loops over literal lists of pairs, asserts and prints that
// only read, and a write of a file's own edited text back to that file. Any
// other construct (an if, a def, a regex, an f-string where a value belongs, a
// write of text the program does not state) and the command states NOTHING.
// So an edit Parse answers is always one the command literally spells out,
// never a guess; a command it declines is drawn as it always was.
//
// WHAT THE COMMAND STATES, NOT WHAT IT DID. The edits are read from the
// command text alone, before it runs; whether the script got as far as its
// writes is the shell's exit status and output to say.
package pyedit

import "fmt"

// Replacement is one str.replace a program makes to a file's text: every
// occurrence of Old, or only the first when First (a count of 1).
type Replacement struct {
	Old   string
	New   string
	First bool
}

// FileEdit is the replacements one write states for one file, in the order
// the program makes them. The path is the program's own, verbatim.
type FileEdit struct {
	Path         string
	Replacements []Replacement
}

// Parse reads the edits a shell command line states. It answers no edits and
// a nil error for a command that runs no python, and an error saying why for
// a command that runs python this package cannot read whole; either way the
// command states nothing drawable.
func Parse(command string) ([]FileEdit, error) {
	scripts, err := pythonScripts(command)
	if err != nil {
		return nil, err
	}
	var edits []FileEdit
	for i, script := range scripts {
		body, err := parse(script)
		if err != nil {
			return nil, fmt.Errorf("python script %d: %w", i+1, err)
		}
		stated, err := run(body)
		if err != nil {
			return nil, fmt.Errorf("python script %d: %w", i+1, err)
		}
		edits = append(edits, stated...)
	}
	return edits, nil
}
