package dlog

import (
	"fmt"
	"os"
)

// destination is one durable sink a logger writes to: a workspace sink or the
// run log. Both are authoritative and synchronous.
type destination interface {
	write(line []byte) error
}

// logger is the Logger implementation. It holds the destination its records
// belong to, so routing is decided once when the logger is obtained rather
// than per record — which is what makes "failing to resolve the workspace is
// an invariant violation" enforceable: a workspace logger that could not be
// built is never handed out, and there is no code path from a workspace record
// to the global sink.
type logger struct {
	s       *surfaces
	dest    destination
	runtime string
	base    Context
}

// Debug implements Logger.
func (l *logger) Debug(operation, message string, ctx Context) {
	l.emit(LevelDebug, operation, message, ctx)
}

// Info implements Logger.
func (l *logger) Info(operation, message string, ctx Context) {
	l.emit(LevelInfo, operation, message, ctx)
}

// Warn implements Logger.
func (l *logger) Warn(operation, message string, ctx Context) {
	l.emit(LevelWarn, operation, message, ctx)
}

// Error implements Logger.
func (l *logger) Error(operation, message string, ctx Context) {
	l.emit(LevelError, operation, message, ctx)
}

// With implements Logger.
func (l *logger) With(ctx Context) Logger {
	return &logger{s: l.s, dest: l.dest, runtime: l.runtime, base: merge(l.base, ctx)}
}

// emit renders one record and delivers it.
func (l *logger) emit(level, operation, message string, ctx Context) {
	rec := newRecord(l.s.now(), l.runtime, level, operation, message, merge(l.base, ctx), l.s.pid)
	l.deliver(rec)
}

// deliver writes the record durably first and mirrors it second. The durable
// write never waits on the mirror.
func (l *logger) deliver(rec record) {
	line := rec.marshal()
	if err := l.dest.write(line); err != nil {
		emergency(err, line)
	}
	if rec.Verbosity == VerbosityVerbose && !l.s.verbose {
		return
	}
	if status := l.s.mirror.enqueue(line); !status.ok() {
		l.reportMirror(status)
	}
}

// reportMirror records a degraded terminal mirror durably and only durably: a
// mirror that just failed or dropped is the wrong place to send the news, and
// re-mirroring it would recurse.
func (l *logger) reportMirror(status mirrorStatus) {
	ctx := Context{"dropped_records": status.Dropped}
	if status.Failure != nil {
		ctx["cause"] = status.Failure.Error()
	}
	rec := newRecord(l.s.now(), l.runtime, LevelWarn,
		"daemon.dlog.terminal_mirror_degraded",
		"the terminal mirror dropped records or failed to write; durable records are unaffected",
		merge(l.base, ctx), l.s.pid)
	line := rec.marshal()
	if err := l.dest.write(line); err != nil {
		emergency(err, line)
	}
}

// emergency is the one permitted exception to "every record goes to its
// durable sink": the canonical sink cannot record its own failure. It writes
// the failure and the record it could not persist to stderr.
func emergency(cause error, line []byte) {
	fmt.Fprintf(os.Stderr, "agent-repl daemon: LOG SINK FAILURE: %v\nunpersisted record: %s", cause, line)
}
