// Package zlog is zettel's one logging helper: every record zettel writes goes
// through a Logger, as one JSON line in the record shape every agent-repl
// runtime shares (timestamp, runtime, pid, level, verbosity, operation,
// message, context), so bin/logs.sh merges it with the others.
package zlog

import (
	"encoding/json"
	"fmt"
	"io"
	"maps"
	"os"
	"sync"
	"time"

	sharedlogging "agentrepl/logging"
)

// Runtime is the name every zettel record carries in its `runtime` field.
const Runtime = "zettel"

// Context is a record's structured fields.
type Context map[string]any

// Logger writes zettel's records. Copies made by With share the sink and its
// lock, so records from every copy are whole lines in one file.
type Logger struct {
	sink     io.Writer
	terminal io.Writer
	window   *sharedlogging.Window
	clock    func() time.Time
	pid      int
	base     Context
	mu       *sync.Mutex
}

// New builds a Logger writing to sink, with records below window's level
// dropped. terminal receives a record only when the sink refuses it, so a
// broken log file still leaves a trace on launchd's stderr capture.
func New(sink, terminal io.Writer, window *sharedlogging.Window, clock func() time.Time) *Logger {
	if sink == nil || terminal == nil || window == nil || clock == nil {
		panic("zlog: nil sink, terminal, window or clock")
	}
	return &Logger{sink: sink, terminal: terminal, window: window, clock: clock, pid: os.Getpid(), base: Context{}, mu: &sync.Mutex{}}
}

// With returns a copy whose records all carry ctx's fields, beneath any the
// call itself supplies.
func (l *Logger) With(ctx Context) *Logger {
	copy := *l
	copy.base = maps.Clone(l.base)
	maps.Copy(copy.base, ctx)
	return &copy
}

// Debug writes a debug record.
func (l *Logger) Debug(operation, message string, ctx Context) { l.write("debug", operation, message, ctx) }

// Info writes an info record.
func (l *Logger) Info(operation, message string, ctx Context) { l.write("info", operation, message, ctx) }

// Warn writes a warn record.
func (l *Logger) Warn(operation, message string, ctx Context) { l.write("warn", operation, message, ctx) }

// Error writes an error record.
func (l *Logger) Error(operation, message string, ctx Context) { l.write("error", operation, message, ctx) }

type record struct {
	Timestamp string  `json:"timestamp"`
	Runtime   string  `json:"runtime"`
	PID       int     `json:"pid"`
	Level     string  `json:"level"`
	Verbosity string  `json:"verbosity"`
	Operation string  `json:"operation"`
	Message   string  `json:"message"`
	Context   Context `json:"context"`
}

func (l *Logger) write(level, operation, message string, ctx Context) {
	if operation == "" {
		panic("zlog: a record needs an operation")
	}
	allowed, ended := l.window.Allows(level)
	if ended != nil {
		l.write("info", "zettel.logging.level-window", ended.Message(), Context(ended.Context()))
	}
	if !allowed {
		return
	}
	merged := maps.Clone(l.base)
	maps.Copy(merged, ctx)
	line, err := json.Marshal(record{
		Timestamp: sharedlogging.Timestamp(l.clock()), Runtime: Runtime, PID: l.pid,
		Level: level, Verbosity: "normal", Operation: operation, Message: message, Context: merged,
	})
	if err != nil {
		panic(fmt.Sprintf("zlog: encoding a record failed operation=%s: %v", operation, err))
	}
	line = append(line, '\n')
	l.mu.Lock()
	defer l.mu.Unlock()
	if _, err := l.sink.Write(line); err != nil {
		fmt.Fprintf(l.terminal, "zettel log sink refused a record (%v): %s", err, line)
	}
}
