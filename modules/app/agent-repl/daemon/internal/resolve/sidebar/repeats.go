package sidebar

import (
	"fmt"
	"sort"
	"strings"

	"agent-repld/internal/dlog"
)

// THE ROSTER'S DECISION RECORDS ARE LOGGED WHEN THEY CHANGE, not on every
// render (owner ruling, 2026-10-12). A render draws every workspace's row --
// in each of the roster's views -- and every row logs its decisions at debug,
// so a state change of one workspace re-logged every other workspace's
// unchanged decisions: four records a row, three views, every workspace,
// every change -- 26% of a test daemon's records and the bulk of an owner's
// debug window.
//
// While debug is enabled a render's debug records are gathered by the
// workspace they name, and each workspace's group is compared with the group
// its last render logged: a group that changed is logged whole, one that did
// not is skipped, and one record per render counts what it skipped, so the
// skipping is itself on the record. Records naming no workspace, and every
// record above debug, pass straight through. At info nothing is gathered.

// repeatFilter is a render's Logger while debug is enabled.
type repeatFilter struct {
	base   dlog.Logger
	ctx    dlog.Context
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
	l                  dlog.Logger
	operation, message string
	ctx                dlog.Context
}

// newRepeatFilter wraps log for one render, against the groups the last
// render logged (last, which the filter updates).
func newRepeatFilter(log dlog.Logger, last map[string]string) *repeatFilter {
	return &repeatFilter{base: log, shared: &repeatBuffer{groups: map[string][]pendingRecord{}, last: last}}
}

// Debug implements dlog.Logger: a record naming a workspace waits for the
// render's end; any other goes out now.
func (f *repeatFilter) Debug(operation, message string, ctx dlog.Context) {
	key := workspaceKey(f.ctx, ctx)
	if key == "" {
		f.base.Debug(operation, message, ctx)
		return
	}
	b := f.shared
	if _, seen := b.groups[key]; !seen {
		b.order = append(b.order, key)
	}
	b.groups[key] = append(b.groups[key], pendingRecord{l: f.base, operation: operation, message: message, ctx: ctx})
}

// Info implements dlog.Logger.
func (f *repeatFilter) Info(operation, message string, ctx dlog.Context) {
	f.base.Info(operation, message, ctx)
}

// Warn implements dlog.Logger.
func (f *repeatFilter) Warn(operation, message string, ctx dlog.Context) {
	f.base.Warn(operation, message, ctx)
}

// Error implements dlog.Logger.
func (f *repeatFilter) Error(operation, message string, ctx dlog.Context) {
	f.base.Error(operation, message, ctx)
}

// With implements dlog.Logger: the derived logger gathers into the same
// render's buffer.
func (f *repeatFilter) With(ctx dlog.Context) dlog.Logger {
	merged := dlog.Context{}
	for k, v := range f.ctx {
		merged[k] = v
	}
	for k, v := range ctx {
		merged[k] = v
	}
	return &repeatFilter{base: f.base.With(ctx), ctx: merged, shared: f.shared}
}

// flush logs every workspace group that differs from its last render's, and
// counts the ones it skipped. A workspace the render no longer drew is
// forgotten, so a row that comes back logs its decisions again.
func (f *repeatFilter) flush() {
	b := f.shared
	skipped := 0
	drawn := make(map[string]bool, len(b.order))
	for _, key := range b.order {
		drawn[key] = true
		group := b.groups[key]
		print := fingerprint(group)
		if b.last[key] == print {
			skipped++
			continue
		}
		b.last[key] = print
		for _, rec := range group {
			rec.l.Debug(rec.operation, rec.message, rec.ctx)
		}
	}
	for key := range b.last {
		if !drawn[key] {
			delete(b.last, key)
		}
	}
	if skipped > 0 {
		f.base.Debug("daemon.sidebar.unchanged_rows", "the roster drew rows whose decisions are unchanged since its last render; they are not logged again",
			dlog.Context{"unchanged": skipped, "drawn": len(b.order)})
	}
}

// workspaceKey is the workspace a record names, through its logger's context
// or its own; empty when it names none.
func workspaceKey(base, ctx dlog.Context) string {
	if id, ok := ctx["workspace_id"]; ok {
		return fmt.Sprint(id)
	}
	if id, ok := base["workspace_id"]; ok {
		return fmt.Sprint(id)
	}
	return ""
}

// fingerprint renders a group of records as one comparable string: each
// record's operation, message and context, keys in order.
func fingerprint(group []pendingRecord) string {
	var sb strings.Builder
	for _, rec := range group {
		sb.WriteString(rec.operation)
		sb.WriteByte(0)
		sb.WriteString(rec.message)
		keys := make([]string, 0, len(rec.ctx))
		for k := range rec.ctx {
			keys = append(keys, k)
		}
		sort.Strings(keys)
		for _, k := range keys {
			fmt.Fprintf(&sb, "\x00%s=%v", k, rec.ctx[k])
		}
		sb.WriteByte('\n')
	}
	return sb.String()
}
