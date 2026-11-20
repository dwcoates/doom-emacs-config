package dlog

import (
	"fmt"
	"os"
	"strings"

	"agentrepl/logging/logsite"
)

// callers resolves the function each log call is written in, skipping the
// frames of this package (every Logger wrapper lives here).
var callers = logsite.NewCallers("agent-repld/internal/dlog")

// destination is one durable sink a logger writes to: a workspace sink or the
// run log. Both are authoritative and synchronous. LEVEL is the record's, and
// picks the file it lands in: a warn or error record goes only to the
// warnings file (sinkFor).
type destination interface {
	write(level string, line []byte) error
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
	// workspaceID is the minted id of the workspace this logger's sink
	// belongs to, empty for a logger bound to no workspace (the run log, a
	// closed surface's dropping logger). Only a logger with one tees its Warn
	// and Error records (see BindRecordTee).
	workspaceID string
}

// Debug implements Logger.
func (l *logger) Debug(operation string, message template, ctx Context) {
	l.emit(LevelDebug, operation, string(message), callers.Function(), ctx)
}

// Info implements Logger.
func (l *logger) Info(operation string, message template, ctx Context) {
	l.emit(LevelInfo, operation, string(message), callers.Function(), ctx)
}

// Warn implements Logger.
func (l *logger) Warn(operation string, message template, ctx Context) {
	l.emit(LevelWarn, operation, string(message), callers.Function(), ctx)
}

// Error implements Logger.
func (l *logger) Error(operation string, message template, ctx Context) {
	l.emit(LevelError, operation, string(message), callers.Function(), ctx)
}

// Log implements Logger.
func (l *logger) Log(level, operation string, message logsite.Message, ctx Context) {
	l.emit(level, operation, message.Template(), forwarded(message), ctx)
}

// With implements Logger.
func (l *logger) With(ctx Context) Logger {
	return &logger{s: l.s, dest: l.dest, runtime: l.runtime, base: merge(l.base, ctx), workspaceID: l.workspaceID}
}

// emit renders one record of the call written as TEMPLATE in FUNCTION and
// delivers it.
func (l *logger) emit(level, operation, template, function string, ctx Context) {
	if !l.s.admits(level) {
		return
	}
	merged := merge(l.base, ctx)
	rec := newKeyedRecord(l.s.now(), l.runtime, level, operation, template, function, merged, l.s.pid)
	text := rec.Message
	l.deliver(rec)
	l.s.count(l.runtime, level, operation)
	l.tee(level, operation, text)
}

// mustRender answers TEMPLATE rendered from the record's named values
// FIELDS. A template naming a value the record does not hold, or one whose
// braces do not parse, is a defect of the call that wrote it, which the
// static scan test (logsite_scan_test.go) exists to catch: it panics.
func mustRender(operation, template string, fields Context) string {
	text, err := logsite.RenderFields(template, fields)
	if err != nil {
		panic(fmt.Sprintf("dlog: the record of operation %s cannot be rendered: %v", operation, err))
	}
	return text
}

// forwarded answers the function MESSAGE was written in. A Message no Msg
// made names no call: it panics.
func forwarded(message logsite.Message) string {
	if message.IsZero() {
		panic("dlog: a forwarded message must be made by logsite.Msg")
	}
	return message.Function()
}

// tee hands a workspace logger's Warn or Error record to the bound record tee,
// AFTER it is durable: THE ONE PLACE a workspace's warnings and errors are
// copied out of the log. Nothing else is teed — a record of a lower level, and
// every record of a logger bound to no workspace.
func (l *logger) tee(level, operation, message string) {
	if l.workspaceID == "" || (level != LevelWarn && level != LevelError) {
		return
	}
	box := l.s.tee.Load()
	if box == nil || box.tee == nil {
		return
	}
	box.tee.OnWorkspaceRecord(WorkspaceRecord{
		WorkspaceID: l.workspaceID,
		Level:       level,
		Operation:   operation,
		Message:     message,
	})
}

// deliver writes the record durably first and mirrors it second. The durable
// write never waits on the mirror.
func (l *logger) deliver(rec record) {
	line := rec.marshal()
	if err := l.dest.write(rec.Level, line); err != nil {
		l.s.emergency(l.runtime, err, line)
	}
	if status := l.s.mirror.enqueue(rec.Level, line); !status.ok() {
		l.reportMirror(status)
	}
}

// reportMirror records a degraded terminal mirror durably and only durably: a
// mirror that just failed or dropped is the wrong place to send the news, and
// re-mirroring it would recurse.
func (l *logger) reportMirror(status mirrorStatus) {
	if !l.s.admits(LevelWarn) {
		return
	}
	ctx := Context{"dropped_records": status.Dropped}
	if status.Failure != nil {
		ctx["cause"] = status.Failure.Error()
	}
	rec := newSiteRecord(l.s.now(), l.runtime, LevelWarn,
		"daemon.dlog.terminal_mirror_degraded",
		logsite.Msg("the terminal mirror dropped records or failed to write; durable records are unaffected"),
		merge(l.base, ctx), l.s.pid)
	line := rec.marshal()
	if err := l.dest.write(rec.Level, line); err != nil {
		l.s.emergency(l.runtime, err, line)
	}
}

// emergency is the one permitted exception to "every record goes to its
// durable sink": the canonical sink cannot record its own failure, so the
// failure and the record it could not persist go to stderr.
//
// IT IS ITSELF A RECORD, on ONE line. It used to be two lines of prose --
// "LOG SINK FAILURE: <cause>" and "unpersisted record: <the json>" -- and
// every reader in the system parses a log line as a record, so the LAST
// RESORT was the one output nothing could read: 24 unparseable lines in the
// 2026-09-13 sweep, each carrying a real record nobody could group, level or
// attribute. The record it could not persist travels whole, as a string in
// `unpersisted_record`, so nothing of it is lost.
func (s *surfaces) emergency(runtime string, cause error, line []byte) {
	ctx := Context{"cause": cause.Error()}
	if len(line) > 0 {
		ctx["unpersisted_record"] = strings.TrimSuffix(string(line), "\n")
	}
	rec := newSiteRecord(s.now(), runtime, LevelError,
		"daemon.dlog.sink_failure",
		logsite.Msg("the durable sink refused a record; it is echoed here because its own sink cannot carry the news"),
		ctx, s.pid)
	_, _ = os.Stderr.Write(rec.marshal())
}
