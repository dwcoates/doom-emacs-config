// Package recordlog is the one record writer of agent-repl's standalone
// services (zettel, the test service): every record a service writes goes
// through a Logger, as one JSON line in the record shape every agent-repl
// runtime shares (timestamp, runtime, pid, level, verbosity, operation,
// message, site, template, context), so bin/logs.sh merges it with the
// others. Each service names its runtime once, at New.
//
// Every record names its SITE (logging-contract.md "Log sites"): the key of
// the log call that wrote it, logsite.Key over the runtime, the function the
// call is written in and its template, and the template itself.
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
	"agentrepl/logging/logsite"
)

// template is a log message's template: a string constant, never a value
// built at run time. It is unexported so no other package can convert a
// run-time string to it; an untyped string constant converts implicitly.
//
// A template is the call's site summary and part of its key, so a dynamic
// value goes in the record's context under a name, and the template names it
// as {name}: "binding the {runtime} socket failed" with Context{"runtime":
// r}. A literal brace is written {{ or }}.
type template string

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

// Debug writes a debug record of MESSAGE, a constant template rendered from
// the record's fields (base and ctx merged).
func (l *Logger) Debug(operation string, message template, ctx Context) {
	l.write("debug", operation, string(message), logsite.Caller(), ctx)
}

// Info writes an info record.
func (l *Logger) Info(operation string, message template, ctx Context) {
	l.write("info", operation, string(message), logsite.Caller(), ctx)
}

// Warn writes a warn record.
func (l *Logger) Warn(operation string, message template, ctx Context) {
	l.write("warn", operation, string(message), logsite.Caller(), ctx)
}

// Error writes an error record.
func (l *Logger) Error(operation string, message template, ctx Context) {
	l.write("error", operation, string(message), logsite.Caller(), ctx)
}

// Log writes a LEVEL record of MESSAGE, keyed by the function that made it
// with logsite.Msg: the path of a helper that logs its caller's message (the
// level window's notes). A Message no Msg made names no call: it panics.
func (l *Logger) Log(level, operation string, message logsite.Message, ctx Context) {
	l.write(level, operation, message.Template(), message.Forwarded(), ctx)
}

type record struct {
	Timestamp string  `json:"timestamp"`
	Runtime   string  `json:"runtime"`
	PID       int     `json:"pid"`
	Level     string  `json:"level"`
	Verbosity string  `json:"verbosity"`
	Operation string  `json:"operation"`
	Message   string  `json:"message"`
	Site      string  `json:"site"`
	Template  string  `json:"template"`
	Context   Context `json:"context"`
}

// write records the call written as TEMPLATE in FUNCTION. A template naming
// a field the record does not hold, or one whose braces do not parse, is a
// defect of the call, which each module's static scan test exists to catch:
// it panics.
func (l *Logger) write(level, operation, template, function string, ctx Context) {
	if operation == "" {
		panic("recordlog: a record needs an operation")
	}
	allowed, ended := l.window.Allows(level)
	if ended != nil {
		l.Log("info", l.runtime+".logging.level-window", ended.Message(), Context(ended.Context()))
	}
	if !allowed {
		return
	}
	merged := maps.Clone(l.base)
	maps.Copy(merged, ctx)
	message, err := logsite.RenderFields(template, merged)
	if err != nil {
		panic(fmt.Sprintf("recordlog: the record of operation %s cannot be rendered: %v", operation, err))
	}
	line, err := json.Marshal(record{
		Timestamp: sharedlogging.Timestamp(l.clock()), Runtime: l.runtime, PID: l.pid,
		Level: level, Verbosity: "normal", Operation: operation, Message: message,
		Site: logsite.Key(l.runtime, function, template), Template: template, Context: merged,
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
		log.Log("info", runtime+".logging.level-window", message, Context(selection.Context()))
	}
	return log, file.Close, nil
}
