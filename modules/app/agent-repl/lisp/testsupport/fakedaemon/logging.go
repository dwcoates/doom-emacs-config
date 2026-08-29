package main

import (
	"encoding/json"
	"os"
	"sync"
	"time"
)

// Structured JSON logs to stderr, one record per line.  The fake daemon is
// read through these logs when an integration run needs to know what the
// neighbor saw, so every logical branch below emits one.
var logMu sync.Mutex

func logRecord(level, operation, message string, context map[string]any) {
	rec := map[string]any{
		"timestamp": time.Now().UTC().Format(time.RFC3339Nano),
		"runtime":   "fakedaemon",
		"pid":       os.Getpid(),
		"level":     level,
		"operation": operation,
		"message":   message,
	}
	if context != nil {
		rec["context"] = context
	}
	line, err := json.Marshal(rec)
	if err != nil {
		// A log record that cannot be serialized is itself a defect; say so
		// in the only channel guaranteed to work.
		line = []byte(`{"level":"error","operation":"fakedaemon.log.marshal-failed"}`)
	}
	logMu.Lock()
	defer logMu.Unlock()
	os.Stderr.Write(append(line, '\n'))
}

func logDebug(op, msg string, ctx map[string]any) { logRecord("debug", op, msg, ctx) }
func logInfo(op, msg string, ctx map[string]any)  { logRecord("info", op, msg, ctx) }
func logWarn(op, msg string, ctx map[string]any)  { logRecord("warning", op, msg, ctx) }
func logError(op, msg string, ctx map[string]any) { logRecord("error", op, msg, ctx) }
