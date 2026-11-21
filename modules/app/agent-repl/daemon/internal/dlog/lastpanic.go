package dlog

import (
	"errors"
	"fmt"
	"io"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
)

// A DAEMON THAT DIED OF A PANIC SAID WHY ONLY ON ITS STDERR. The Go runtime
// writes a panic, or a fatal error, to fd 2, which is the stdio log
// (StdioLogName), and never through the canonical log: a boot that finds its
// predecessor died without its orderly exit reads the cause back from the
// stdio log's tail (LastPanic) and records it through the canonical log, so
// the death is in every digest (2026-11-21: a crash loop's panics stood only
// in daemon.stdio.log, and the deploy mediator never saw them).

// stdioTailBytes bounds how much of the stdio log's end a boot reads for its
// predecessor's panic: a panic's message and its goroutine's stack, with
// every other goroutine's after it, fit with room to spare.
const stdioTailBytes = 1 << 20

// Panic is the last panic, or fatal runtime error, the stdio log holds.
type Panic struct {
	// Message is its first line, as the runtime wrote it ("panic: ...",
	// "fatal error: ...").
	Message string
	// Site is the first agent-repld frame of the panicking goroutine: the
	// function and its file:line, "" when the stack names none.
	Site string
}

// LastPanic reads the panic the daemon of process PID died of from the stdio
// log under LOGSDIR, from its last stdioTailBytes, answering false when that
// tail holds none of that daemon's: a panic is that daemon's only when the
// last record written before it is that daemon's (each record carries its
// process's "pid"), so an older daemon's panic is never blamed on a
// predecessor that died some other way (a SIGKILL writes nothing). A missing
// log holds none; any other read failure is answered.
func LastPanic(logsDir string, pid int) (Panic, bool, error) {
	path := filepath.Join(logsDir, StdioLogName)
	f, err := os.Open(path)
	if errors.Is(err, fs.ErrNotExist) {
		return Panic{}, false, nil
	}
	if err != nil {
		return Panic{}, false, fmt.Errorf("dlog: open the stdio log %s: %w", path, err)
	}
	defer f.Close()
	info, err := f.Stat()
	if err != nil {
		return Panic{}, false, fmt.Errorf("dlog: stat the stdio log %s: %w", path, err)
	}
	start := max(info.Size()-stdioTailBytes, 0)
	tail := make([]byte, info.Size()-start)
	if _, err := f.ReadAt(tail, start); err != nil && !errors.Is(err, io.EOF) {
		return Panic{}, false, fmt.Errorf("dlog: read the stdio log %s: %w", path, err)
	}
	p, found := lastPanicIn(string(tail), pid)
	return p, found, nil
}

// lastPanicIn finds the last panic in OUT written by the daemon of process
// PID: its first line, and the first agent-repld frame after it with that
// frame's file:line.
func lastPanicIn(out string, pid int) (Panic, bool) {
	lines := strings.Split(out, "\n")
	at := -1
	for i := len(lines) - 1; i >= 0; i-- {
		if strings.HasPrefix(lines[i], "panic: ") || strings.HasPrefix(lines[i], "fatal error: ") {
			at = i
			break
		}
	}
	if at < 0 || !writtenBy(lines[:at], pid) {
		return Panic{}, false
	}
	p := Panic{Message: strings.TrimSpace(lines[at])}
	for i := at + 1; i < len(lines)-1; i++ {
		frame := strings.TrimSpace(lines[i])
		if strings.HasPrefix(frame, "agent-repld/") {
			fn := frame
			if open := strings.LastIndex(fn, "("); open > 0 {
				fn = fn[:open]
			}
			file := strings.TrimSpace(lines[i+1])
			if space := strings.LastIndex(file, " +0x"); space > 0 {
				file = file[:space]
			}
			p.Site = fn + " at " + file
			break
		}
	}
	return p, true
}

// writtenBy reports whether the last structured record among LINES is the
// daemon of process PID's.
func writtenBy(lines []string, pid int) bool {
	mark := fmt.Sprintf(`"pid":%d`, pid)
	for i := len(lines) - 1; i >= 0; i-- {
		if strings.HasPrefix(lines[i], "{") && strings.Contains(lines[i], `"pid":`) {
			return strings.Contains(lines[i], mark+",") || strings.Contains(lines[i], mark+"}")
		}
	}
	return false
}
