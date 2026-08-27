package handler

import (
	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/convert"
	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

// SessionTranscriptHandler reads a session transcript: the record of one
// conversation as the harness wrote it to disk.
//
// IT IS THE ONLY PRODUCER OF A CONVERSATION'S CONTENT, and that is a deliberate
// consequence of the lineage model rather than a division of labour. A message
// must state its parent, and the live SDK stream carries no parent pointer, so
// the shim cannot write one; only the file the CLI itself wrote has the chain.
// The cost, accepted: content becomes durable a beat after it is spoken.
//
// A stop_hook_summary is NOT a turn boundary and is never read as one. It is
// written after the turn's result and can be delayed past the next accepted
// prompt, so only the live shim's stream plane owns turn lifecycle.
type SessionTranscriptHandler struct {
	conv *convert.Converter
	log  *logging.Bound
}

// NewSessionTranscriptHandler builds a handler with its own converter.
func NewSessionTranscriptHandler(log *logging.Bound) *SessionTranscriptHandler {
	log.With(logging.Context{Operation: "transcript-handler-new"}).LogVerbose("constructing session transcript handler")
	return &SessionTranscriptHandler{conv: convert.New(log), log: log}
}

// Handle implements tail.Handler.
func (h *SessionTranscriptHandler) Handle(frames []tail.Frame, ctx *Context) []*storev1.StoreEntry {
	h.log.With(logging.Context{Operation: "transcript-handle", Path: ctx.Path, Session: ctx.SessionID, Task: ctx.TaskID}).
		LogVerbose("handling frames=%d records_observed=%d", len(frames), ctx.RecordsObserved)

	// A compaction boundary at the very end of a batch is UNSETTLED: its summary
	// is the next line in the file and may not be written yet. holdCount defers
	// it — leaving the reader's cursor parked BEFORE it — rather than converting
	// it on half the evidence and throwing the summary away for good.
	originalCount := len(frames)
	frames = frames[:h.holdCount(frames, ctx)]
	if len(frames) != originalCount {
		h.log.With(logging.Context{Operation: "transcript-handle", Path: ctx.Path, Session: ctx.SessionID, Task: ctx.TaskID}).
			Log("deferred compact boundary frames=%d processed=%d held_deliveries=%d held_offset=%d",
				originalCount, len(frames), ctx.HeldDeliveries, ctx.HeldOffset)
	}

	var out []*storev1.StoreEntry
	for i, frame := range frames {
		at := attribute(ctx, frame.Offset)
		if frame.ParseErr != nil {
			h.log.With(logging.Context{Operation: "parse", Path: ctx.Path, Session: ctx.SessionID, Task: ctx.TaskID, Level: "warn"}).
				Log("parse failure at offset=%d; the record is stored whole with no path to a page: %v", frame.Offset, frame.ParseErr)
			out = append(out, convert.UnparsedEntry(at, frame.Raw, frame.ParseErr))
			continue
		}
		out = append(out, h.conv.Line(frame.Obj, at, lookahead(frames, i+1))...)
	}
	logUnconverted(h.log, ctx, out)
	h.log.With(logging.Context{Operation: "transcript-handle", Path: ctx.Path, Session: ctx.SessionID, Task: ctx.TaskID}).
		LogVerbose("handled frames=%d entries=%d", len(frames), len(out))
	return out
}

// lookahead returns the decoded record that FOLLOWS a frame in the file, or nil
// past the end of the batch. It exists for one record: a compaction boundary,
// whose summary the harness writes as the following line.
func lookahead(frames []tail.Frame, i int) map[string]any {
	if i < 0 || i >= len(frames) {
		return nil
	}
	return frames[i].Obj
}

// maxHoldDeliveries is the SILENCE BOUND on a held compaction boundary: how many
// further deliveries the handler waits for a summary that never arrives before
// converting the boundary without one.
//
// It is ONE. The two lines are written about a millisecond apart against a poll
// interval three orders of magnitude longer, so a single further scan is already
// an enormous margin over the gap being covered. Every additional scan of
// holding only delays the truncation render for the sessions that genuinely stop
// at a boundary, and buys nothing for the ones that do not.
const maxHoldDeliveries = 1

// holdCount returns how many frames may be converted NOW, and records on ctx
// whatever it deferred.
//
// It defers exactly one frame, the batch's last, and only when that frame is a
// compaction boundary — the sole record in a transcript whose meaning depends on
// a line that may not be written yet. Everything else is settled by its own
// bytes and is converted the moment it is read.
//
// A deferral is only ever taken when the READER promises to hand the frame back
// (ctx.Redelivers). A handler given one standalone batch has no next delivery,
// so holding there would drop the compaction silently; that caller gets the
// summary-less conversion, loud log and all.
func (h *SessionTranscriptHandler) holdCount(frames []tail.Frame, ctx *Context) int {
	n := len(frames)
	// What was deferred LAST delivery. Both fields are rewritten below on every
	// call, so a stale hold can never outlive the batch that took it.
	heldOffset, heldFor := ctx.HeldOffset, ctx.HeldDeliveries
	ctx.HeldOffset, ctx.HeldDeliveries = 0, 0
	if n == 0 || !ctx.Redelivers {
		return n
	}
	last := frames[n-1]
	if last.ParseErr != nil {
		// An unparsable frame is settled: it is stored as unparsed now, and no
		// later line can change that.
		return n
	}
	if !convert.IsCompactBoundary(last.Obj) {
		return n
	}
	if heldOffset == last.Offset && heldFor >= maxHoldDeliveries {
		// Held once already and the file still says nothing after it. Stop
		// waiting: the caller converts it without a summary, loudly.
		return n
	}
	if heldOffset == last.Offset {
		ctx.HeldDeliveries = heldFor + 1
	} else {
		ctx.HeldDeliveries = 1
	}
	ctx.HeldOffset = last.Offset
	return n - 1
}
