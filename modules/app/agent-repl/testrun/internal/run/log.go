package run

import (
	"fmt"
	"io"
	"sync"
)

// Prefix is every line the runner itself prints. The merge gate's parser
// (daemon/internal/merge/testgate.go) reads the suite lines through it.
const Prefix = "[agent-repl-tests]"

// Log is the runner's ONE logging path: progress to Out, errors to Err, every
// line prefixed. Nothing in this module prints any other way.
type Log struct {
	mu  sync.Mutex
	Out io.Writer
	Err io.Writer
}

// Infof prints one progress line.
func (l *Log) Infof(format string, args ...any) {
	l.mu.Lock()
	defer l.mu.Unlock()
	fmt.Fprintf(l.Out, "%s %s\n", Prefix, fmt.Sprintf(format, args...))
}

// Errorf prints one error line.
func (l *Log) Errorf(format string, args ...any) {
	l.mu.Lock()
	defer l.mu.Unlock()
	fmt.Fprintf(l.Err, "%s ERROR: %s\n", Prefix, fmt.Sprintf(format, args...))
}

// Block copies a finished unit's whole output to Out in one piece, so units
// that ran side by side never interleave their lines.
func (l *Log) Block(data []byte) {
	l.mu.Lock()
	defer l.mu.Unlock()
	l.Out.Write(data)
	if n := len(data); n > 0 && data[n-1] != '\n' {
		io.WriteString(l.Out, "\n")
	}
}
