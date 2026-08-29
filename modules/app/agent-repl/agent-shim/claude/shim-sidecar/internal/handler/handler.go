// Package handler is the sidecar's record→record layer: pure functions with
// ZERO IO that turn decoded file records into the stored records the shim-store
// persists.
//
// A handler owns a converter and does three things with a batch of frames:
// attributes each frame (which session, which detached-work card, which file
// offset), asks the converter what the record IS, and defers the one record
// whose meaning depends on a line that may not be written yet.
//
// IT NEVER DECIDES WHETHER A RECORD IS INTERESTING. Curation is a downstream
// concern; ingestion's only job is that every JSON object on disk ends up in the
// store as a protobuf shape.
package handler

import (
	"time"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/convert"
	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

// Producer is the fixed StoreEntryWrite producer identity for the sidecar.
const Producer = convert.Producer

// Context / Kind live in the tail package (tailer-owned attribution); aliased
// here so handler code reads naturally.
type Context = tail.Context

// nowMillis is the producer wall clock in unix millis. Overridable in tests.
var nowMillis = func() int64 { return time.Now().UnixMilli() }

// attribute builds the conversion attribution for one frame.
//
// `Container` is the detached-work card every record in this file sits inside.
// It is DERIVED from the task id rather than looked up, which is what lets a
// spool read after a restart land on the same card as the launch that opened it
// before one.
func attribute(ctx *Context, offset int64) convert.Attribution {
	at := convert.Attribution{
		SessionID:    ctx.SessionID,
		Path:         ctx.Path,
		Offset:       offset,
		ProducedAtMs: nowMillis(),
	}
	if ctx.Kind != tail.KindSessionTranscript && ctx.TaskID != "" {
		at.Container = convert.DetachedWorkMessageID(ctx.TaskID)
	}
	return at
}

// logUnconverted records every record stored as an UNSERVED item.
//
// THE TEST CHANGED WITH THE CONTRACT. It used to be "carries no external half",
// because store.v1's predecessor split every record into an internal and an
// external half and only the external one reached the daemon. StoreEntry has no
// such split, so the equivalent question is now which agent_info arm the record
// landed on: an unserved_item is by construction not a serveable frame, so it
// never reaches a page and is invisible to the user.
//
// That is a legitimate outcome and also the one worth counting: it is the
// running measure of how much of what the vendor writes this schema does not yet
// model — a number the unported conversions have made much larger.
func logUnconverted(log *logging.Bound, ctx *Context, entries []*storev1.StoreEntry) {
	for _, entry := range entries {
		if entry.GetAgentUpdate().GetUnservedItem() == nil {
			continue
		}
		log.With(logging.Context{Operation: "unconverted", Path: ctx.Path, VendorSessionID: ctx.SessionID, TaskID: ctx.TaskID}).
			LogVerbose("record stored as an unserved item: %s", convert.Describe(entry))
	}
}
