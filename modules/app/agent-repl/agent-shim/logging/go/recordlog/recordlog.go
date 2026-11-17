// Package recordlog is the one record writer of agent-repl's standalone
// services (zettel, the test service): every record a service writes goes
// through a Logger, as one JSON line in the record shape every agent-repl
// runtime shares (timestamp, runtime, pid, level, verbosity, operation,
// message, context), so bin/logs.sh merges it with the others. Each service
// names its runtime once, at New.
package recordlog

import (
	"encoding/json"
	"fmt"
	"io"
	"maps"
	"os"
	"path/filepath"
	"sync"
	"time"

	sharedlogging "agentrepl/logging"
)

// Context is a record's structured fields.
type Context map[string]any

// Logger writes one service's records. Copies made by With share the sink
// and its lock, so records from every copy are whole lines in one file.
type Logger struct {
	runtime  string
	sink     io.Writer
	terminal io.Writer
	window   *sharedlogging.Window
	clock    func() time.Time
	pid      int
	base     Context
	mu       *sync.Mutex
}

// New builds a Logger writing RUNTIME's records to sink, with records below
// window's level dropped. terminal receives a record only when the sink
// refuses it, so a broken log file still leaves a trace on the service
// manager's stderr capture.
func New(runtime string, sink, terminal io.Writer, window *sharedlogging.Window, clock func() time.Time) *Logger {
	if runtime == "" || sink == nil || terminal == nil || window == nil || clock == nil {
		panic("recordlog: no runtime, or a nil sink, terminal, window or clock")
	}
	return &Logger{runtime: runtime, sink: sink, terminal: terminal, window: window, clock: clock, pid: os.Getpid(), base: Context{}, mu: &sync.Mutex{}}
}

// Runtime is the runtime every record of this Logger names.
func (l *Logger) Runtime() string { return l.runtime }

// With returns a copy whose records all carry ctx's fields, beneath any the
// call itself supplies.
func (l *Logger) With(ctx Context) *Logger {
	copy := *l
	copy.base = maps.Clone(l.base)
	maps.Copy(copy.base, ctx)
	return &copy
}

// Debug writes a debug record.
func (l *Logger) Debug(operation, message string, ctx Context) {
	l.write("debug", operation, message, ctx)
}

// Info writes an info record.
func (l *Logger) Info(operation, message string, ctx Context) {
	l.write("info", operation, message, ctx)
}

// Warn writes a warn record.
func (l *Logger) Warn(operation, message string, ctx Context) {
	l.write("warn", operation, message, ctx)
}

// Error writes an error record.
func (l *Logger) Error(operation, message string, ctx Context) {
	l.write("error", operation, message, ctx)
}

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
		panic("recordlog: a record needs an operation")
	}
	allowed, ended := l.window.Allows(level)
	if ended != nil {
		l.write("info", l.runtime+".logging.level-window", ended.Message(), Context(ended.Context()))
	}
	if !allowed {
		return
	}
	merged := maps.Clone(l.base)
	maps.Copy(merged, ctx)
	line, err := json.Marshal(record{
		Timestamp: sharedlogging.Timestamp(l.clock()), Runtime: l.runtime, PID: l.pid,
		Level: level, Verbosity: "normal", Operation: operation, Message: message, Context: merged,
	})
	if err != nil {
		panic(fmt.Sprintf("recordlog: encoding a record failed operation=%s: %v", operation, err))
	}
	line = append(line, '\n')
	l.mu.Lock()
	defer l.mu.Unlock()
	if _, err := l.sink.Write(line); err != nil {
		fmt.Fprintf(l.terminal, "%s log sink refused a record (%v): %s", l.runtime, err, line)
	}
}

// Open opens RUNTIME's log at PATH: a size-capped rotating file, its level
// chosen by AGENT_REPL_LOG_LEVEL and sharedlogging.UntilEnvironment, records
// the file refuses written to TERMINAL. The selection's note, when it has
// one, is the first record. It answers the logger and the file's close.
func Open(runtime, path string, terminal io.Writer, getenv func(string) string, clock func() time.Time) (*Logger, func() error, error) {
	selection, err := sharedlogging.SelectLevel(getenv("AGENT_REPL_LOG_LEVEL"), getenv(sharedlogging.UntilEnvironment), clock())
	if err != nil {
		return nil, nil, err
	}
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		return nil, nil, err
	}
	// WARN AND ERROR RECORDS GO TO THEIR OWN FILE (warnings.go).
	file, err := sharedlogging.OpenRotatingPair(path, sharedlogging.DefaultCapBytes, sharedlogging.DefaultBackups)
	if err != nil {
		return nil, nil, err
	}
	log := New(runtime, file, terminal, sharedlogging.NewWindow(selection, clock), clock)
	if message, ok := selection.Note(); ok {
		log.Info(runtime+".logging.level-window", message, Context(selection.Context()))
	}
	return log, file.Close, nil
}
