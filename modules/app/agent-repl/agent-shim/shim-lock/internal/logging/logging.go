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
package logging

import (
	"encoding/json"
	"fmt"
	"io"
	"os"
	"sync"
	"time"

	sharedlogging "agentrepl/logging"
)

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
	Context   Context `json:"context,omitempty"`
}

// Logger writes records to one sink under a mutex, so two goroutines cannot
// interleave halves of two records on one line.
type Logger struct {
	mu   sync.Mutex
	sink io.Writer
	// clock and pid are seams the suite substitutes; production leaves them
	// at time.Now and os.Getpid.
	clock func() time.Time
	pid   func() int
	// sinkFailed records that a write to the sink failed. See SinkFailed.
	sinkFailed bool
}

// New builds a Logger over sink.
func New(sink io.Writer) *Logger {
	return &Logger{sink: sink, clock: time.Now, pid: os.Getpid}
}

// Info records a normal branch.
func (l *Logger) Info(operation, message string, ctx Context) { l.log("info", operation, message, ctx) }

// Error records a failure. shim-lock has no warn level: every branch it logs
// is either the claim proceeding or the claim failing.
func (l *Logger) Error(operation, message string, ctx Context) {
	l.log("error", operation, message, ctx)
}

func (l *Logger) log(level, operation, message string, ctx Context) {
	if operation == "" {
		// A record with no operation cannot be correlated, so it is a defect in
		// the caller rather than something to emit half of.
		panic("shim-lock logging: every record must name an operation")
	}
	rec := record{
		Timestamp: sharedlogging.Timestamp(l.clock()),
		Runtime:   Runtime,
		PID:       l.pid(),
		Level:     level,
		Component: Component,
		Operation: operation,
		Message:   message,
		Context:   ctx,
	}
	encoded, err := json.Marshal(rec)
	if err != nil {
		// Marshalling cannot be allowed to swallow the branch that was being
		// reported: fall back to a plain line rather than to silence.
		encoded = []byte(fmt.Sprintf(`{"runtime":%q,"level":"error","operation":%q,"message":%q}`,
			Runtime, operation, fmt.Sprintf("record could not be marshalled (%v): %s", err, message)))
	}
	l.mu.Lock()
	defer l.mu.Unlock()
	if _, err := l.sink.Write(append(encoded, '\n')); err != nil {
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
