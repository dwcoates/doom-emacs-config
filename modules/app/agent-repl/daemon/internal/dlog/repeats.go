package dlog

import (
	"fmt"
	"sort"
	"strings"

	"agentrepl/logging/logsite"
)

// RepeatFilter is a render's Logger that logs a workspace's debug decisions
// only when they CHANGE (owner ruling, 2026-10-12): the roster draws every
// workspace's row on every render, so re-logging every unchanged row was 26%
// of a test daemon's records.
//
// Its debug records are gathered by the workspace they name (KeyWorkspaceID,
// in the record's context or its logger's), and Flush compares each group
// with the group the last render logged: a changed group is logged whole, an
// unchanged one is skipped. Records naming no workspace, and every record
// above debug, pass straight through. A held record is replayed keyed by the
// call that wrote it (Logger.logAt), so filtering never moves a site.
//
// It lives in this package because every Logger does (Logger.logAt).
type RepeatFilter struct {
	base   Logger
	ctx    Context
	shared *repeatBuffer
}

// repeatBuffer is one render's gathered debug records, by workspace.
type repeatBuffer struct {
	order  []string
	groups map[string][]pendingRecord
	last   map[string]string
}

// pendingRecord is one gathered debug record and the logger it goes out on.
type pendingRecord struct {
	l                             Logger
	operation, template, function string
	ctx                           Context
}

// NewRepeatFilter wraps LOG for one render, against the groups the last
// render logged (LAST, which Flush updates).
func NewRepeatFilter(log Logger, last map[string]string) *RepeatFilter {
	return &RepeatFilter{base: log, shared: &repeatBuffer{groups: map[string][]pendingRecord{}, last: last}}
}

// Debug implements Logger: a record naming a workspace waits for Flush; any
// other goes out now.
func (f *RepeatFilter) Debug(operation string, message template, ctx Context) {
	f.logAt(LevelDebug, operation, string(message), logsite.Caller(), ctx)
}

// Info implements Logger.
func (f *RepeatFilter) Info(operation string, message template, ctx Context) {
	f.logAt(LevelInfo, operation, string(message), logsite.Caller(), ctx)
}

// Warn implements Logger.
func (f *RepeatFilter) Warn(operation string, message template, ctx Context) {
	f.logAt(LevelWarn, operation, string(message), logsite.Caller(), ctx)
}

// Error implements Logger.
func (f *RepeatFilter) Error(operation string, message template, ctx Context) {
	f.logAt(LevelError, operation, string(message), logsite.Caller(), ctx)
}

// Log implements Logger.
func (f *RepeatFilter) Log(level, operation string, message logsite.Message, ctx Context) {
	f.logAt(level, operation, message.Template(), message.Forwarded(), ctx)
}

func (f *RepeatFilter) logAt(level, operation, template, function string, ctx Context) {
	key := repeatKey(f.ctx, ctx)
	if level != LevelDebug || key == "" {
		f.base.logAt(level, operation, template, function, ctx)
		return
	}
	b := f.shared
	if _, seen := b.groups[key]; !seen {
		b.order = append(b.order, key)
	}
	b.groups[key] = append(b.groups[key], pendingRecord{l: f.base, operation: operation, template: template, function: function, ctx: ctx})
}

// With implements Logger: the derived logger gathers into the same render's
// buffer.
func (f *RepeatFilter) With(ctx Context) Logger {
	return &RepeatFilter{base: f.base.With(ctx), ctx: merge(f.ctx, ctx), shared: f.shared}
}

// Flush logs every workspace group that differs from its last render's and
// answers how many groups it SKIPPED as unchanged and how many were DRAWN, so
// the caller records the skipping itself. A workspace the render no longer
// drew is forgotten, so a row that comes back logs its decisions again.
func (f *RepeatFilter) Flush() (skipped, drawn int) {
	b := f.shared
	seen := make(map[string]bool, len(b.order))
	for _, key := range b.order {
		seen[key] = true
		group := b.groups[key]
		print := fingerprint(group)
		if b.last[key] == print {
			skipped++
			continue
		}
		b.last[key] = print
		for _, rec := range group {
			rec.l.logAt(LevelDebug, rec.operation, rec.template, rec.function, rec.ctx)
		}
	}
	for key := range b.last {
		if !seen[key] {
			delete(b.last, key)
		}
	}
	return skipped, len(b.order)
}

// repeatKey is the workspace a record names, through its logger's context or
// its own; empty when it names none.
func repeatKey(base, ctx Context) string {
	if id, ok := ctx[KeyWorkspaceID]; ok {
		return fmt.Sprint(id)
	}
	if id, ok := base[KeyWorkspaceID]; ok {
		return fmt.Sprint(id)
	}
	return ""
}

// fingerprint renders a group of records as one comparable string: each
// record's operation, site, template and context, keys in order.
func fingerprint(group []pendingRecord) string {
	var sb strings.Builder
	for _, rec := range group {
		sb.WriteString(rec.operation)
		sb.WriteByte(0)
		sb.WriteString(rec.function)
		sb.WriteByte(0)
		sb.WriteString(rec.template)
		keys := make([]string, 0, len(rec.ctx))
		for k := range rec.ctx {
			keys = append(keys, k)
		}
		sort.Strings(keys)
		for _, k := range keys {
			sb.WriteString("\x00" + k + "=" + fmt.Sprint(rec.ctx[k]))
		}
		sb.WriteByte('\n')
	}
	return sb.String()
}
