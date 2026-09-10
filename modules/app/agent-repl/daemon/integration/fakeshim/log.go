package main

import (
	"encoding/json"
	"os"
	"sync"
	"time"
)

// logSink writes the shim's JSONL records to the descriptor the daemon handed
// it (fd 3 by contract). A write failure is reported to stderr rather than
// swallowed, because a poisoned log sink is itself a fault the suite asserts.
type logSink struct {
	mu sync.Mutex
	f  *os.File
}

func newLogSink(fd int) *logSink {
	if fd < 0 {
		return &logSink{}
	}
	return &logSink{f: os.NewFile(uintptr(fd), "shim.log")}
}

func (l *logSink) write(operation string, context map[string]any) {
	if l == nil || l.f == nil {
		return
	}
	rec := map[string]any{
		"timestamp": time.Now().UTC().Format(time.RFC3339Nano),
		"runtime":   "shim",
		"level":     "debug",
		"pid":       os.Getpid(),
		"operation": "shim.fake." + operation,
		"context":   context,
	}
	line, err := json.Marshal(rec)
	if err != nil {
		os.Stderr.WriteString("fakeshim: cannot encode log record: " + err.Error() + "\n")
		return
	}
	l.mu.Lock()
	defer l.mu.Unlock()
	if _, err := l.f.Write(append(line, '\n')); err != nil {
		os.Stderr.WriteString("fakeshim: log sink write failed: " + err.Error() + "\n")
	}
}

func (l *logSink) close() {
	if l == nil || l.f == nil {
		return
	}
	l.mu.Lock()
	defer l.mu.Unlock()
	l.f.Close()
}
