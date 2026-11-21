// Package logging owns shim-lock's diagnostic contract: one JSON record per
// logged branch, written to STDERR and nowhere else.
//
// STDOUT IS A PROTOCOL CHANNEL HERE, not a log sink. The single line shim-lock
// writes there is what the shim waits on to know the claim is made, so a
// diagnostic that landed on stdout would be read as a readiness signal. Every
// record therefore goes to stderr, which the shim drains and folds into its own
// `shim-session-lock` records.
//
// The record SHAPE is the one every agent-repl Go runtime writes — the shared
// timestamp layout, the runtime name, the pid, the level, the operation, the
// message, and a flat context map — so a shim-lock record joins against the
// daemon's, the store's and the sidecar's by the same keys.
//
// EVERY RECORD NAMES ITS SITE (logging-contract.md "Log sites"): the key of
// the log call that wrote it (logsite.Key over Runtime, the function the call
// is written in and its template) and the template itself, right after the
// message.
package logging

import (
	"encoding/json"
	"fmt"
	"io"
	"maps"
	"os"
	"sync"
	"time"

	sharedlogging "agentrepl/logging"
	"agentrepl/logging/logsite"
)

// template is a log message's template: a string constant, never a value
// built at run time. It is unexported so no other package can convert a
// run-time string to it; an untyped string constant converts implicitly.
//
// The template is the call's site summary and part of its key, so a dynamic
// value goes in the call's context under a name, and the template names it
// as {name}: "flock failed: {cause}" with Context{"cause": err.Error()}. A
// literal brace is written {{ or }}.
type template string

// Runtime is the value every shim-lock record reports as its producer.
const Runtime = "shim-lock"

// Component is the subsystem name shim-lock records carry. It matches the
// shim's own `shim-session-lock` family deliberately: the holder and the shim
// that spawned it are two halves of one claim, and a reader filtering on the
// lock should see both.
const Component = "shim-session-lock"

// Context is a record's structured attribution. Empty values are omitted
// rather than emitted as sentinels.
type Context map[string]any

type record struct {
	Timestamp string  `json:"timestamp"`
	Runtime   string  `json:"runtime"`
	PID       int     `json:"pid"`
	Level     string  `json:"level"`
	Component string  `json:"component"`
	Operation string  `json:"operation"`
	Message   string  `json:"message"`
	Site      string  `json:"site"`
	Template  string  `json:"template"`
	Context   Context `json:"context,omitempty"`
}

// Logger writes records to one sink under a mutex, so two goroutines cannot
// interleave halves of two records on one line. Copies made by With share the
// sink, its mutex and its failure flag.
type Logger struct {
	*sink
	// base is the context every record of this copy carries, beneath the
	// call's own.
	base Context
}

// sink is the one destination every copy of a Logger writes to.
type sink struct {
	mu sync.Mutex
	w  io.Writer
	// clock and pid are seams the suite substitutes; production leaves them
	// at time.Now and os.Getpid.
	clock func() time.Time
	pid   func() int
	// sinkFailed records that a write to the sink failed. See SinkFailed.
	sinkFailed bool
}

// New builds a Logger over w.
func New(w io.Writer) *Logger {
	return &Logger{sink: &sink{w: w, clock: time.Now, pid: os.Getpid}}
}

// With returns a copy whose records all carry ctx's fields, beneath any the
// call itself supplies.
func (l *Logger) With(ctx Context) *Logger {
	base := maps.Clone(l.base)
	if base == nil {
		base = Context{}
	}
	maps.Copy(base, ctx)
	return &Logger{sink: l.sink, base: base}
}

// Info records a normal branch: MESSAGE, a constant template, rendered from
// the record's context.
func (l *Logger) Info(operation string, message template, ctx Context) {
	l.log("info", operation, string(message), logsite.Caller(), ctx)
}

// Error records a failure. shim-lock has no warn level: every branch it logs
// is either the claim proceeding or the claim failing.
func (l *Logger) Error(operation string, message template, ctx Context) {
	l.log("error", operation, string(message), logsite.Caller(), ctx)
}

// log records the call written as TEMPLATE in FUNCTION. A template naming a
// field the record does not hold, or one whose braces do not parse, is a
// defect of the call, which the static scan test (logsite_scan_test.go)
// exists to catch: it panics.
func (l *Logger) log(level, operation, template, function string, ctx Context) {
	if operation == "" {
		// A record with no operation cannot be correlated, so it is a defect in
		// the caller rather than something to emit half of.
		panic("shim-lock logging: every record must name an operation")
	}
	merged := ctx
	if len(l.base) > 0 {
		merged = maps.Clone(l.base)
		maps.Copy(merged, ctx)
	}
	message := logsite.MustRender(operation, template, merged)
	site := logsite.Key(Runtime, function, template)
	rec := record{
		Timestamp: sharedlogging.Timestamp(l.clock()),
		Runtime:   Runtime,
		PID:       l.pid(),
		Level:     level,
		Component: Component,
		Operation: operation,
		Message:   message,
		Site:      site,
		Template:  template,
		Context:   merged,
	}
	encoded, err := json.Marshal(rec)
	if err != nil {
		// Marshalling cannot be allowed to swallow the branch that was being
		// reported: fall back to a plain line rather than to silence.
		encoded = []byte(fmt.Sprintf(`{"runtime":%q,"level":"error","operation":%q,"message":%q,"site":%q,"template":%q}`,
			Runtime, operation, fmt.Sprintf("record could not be marshalled (%v): %s", err, message), site, template))
	}
	l.mu.Lock()
	defer l.mu.Unlock()
	if _, err := l.w.Write(append(encoded, '\n')); err != nil {
		// STDERR IS THE ONLY REPORTING CHANNEL THIS PROCESS HAS, so a failed
		// write to it has nowhere left to go: there is no second sink to
		// escalate to, and stdout is the readiness protocol the shim parses.
		// The error is surfaced the only way it still can be — the process's
		// exit status, via the flag SinkFailed reports — rather than
		// being dropped here.
		l.sinkFailed = true
	}
}

// SinkFailed answers whether any record failed to reach the sink. main turns a
// true answer into a nonzero exit, so a holder whose diagnostics went nowhere
// is never mistaken for one that reported nothing because nothing happened.
func (l *Logger) SinkFailed() bool {
	l.mu.Lock()
	defer l.mu.Unlock()
	return l.sinkFailed
}
