package handler

// transcript.go — the session transcript: the record of one conversation as the
// harness wrote it to disk.
//
// IT IS THE ONLY PRODUCER OF A CONVERSATION'S CONTENT. The live SDK stream is
// first to know and owns turn LIFECYCLE, but what the vendor itself recorded is
// what this reads back — so content becomes durable a beat after it is spoken,
// and that cost is accepted.
//
// A stop_hook_summary is NOT a turn boundary and is never read as one: it is
// written after the turn's result and can be delayed past the next accepted
// prompt, so only the stream plane owns turn lifecycle.

import (
	"io"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/convert"
	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

// SessionTranscriptHandler reads a session transcript.
type SessionTranscriptHandler struct {
	conv *convert.Converter
	log  *logging.Bound
	// obs is the reader's callbacks, adopted one at a time and installed on the
	// converter once, at construction: a converter has exactly one observer, and
	// two independent adoptions must not overwrite each other.
	obs *seamObserver
}

// NewSessionTranscriptHandler builds a handler with its own converter.
func NewSessionTranscriptHandler(log *logging.Bound) *SessionTranscriptHandler {
	log.With(logging.Context{Operation: "transcript-handler-new"}).LogVerbose("constructing session transcript handler")
	obs := &seamObserver{}
	conv := convert.New(log)
	conv.SetObserver(obs)
	return &SessionTranscriptHandler{conv: conv, log: log, obs: obs}
}

// Handle implements tail.Handler.
func (h *SessionTranscriptHandler) Handle(frames []tail.Frame, ctx *Context) []*storev1.StoreEntry {
	h.obs.bind(ctx)
	h.log.With(handleCtx("transcript-handle", ctx)).
		LogVerbose("handling frames=%d records_observed=%d", len(frames), ctx.RecordsObserved)

	// A compaction boundary at the very end of a batch is UNSETTLED: its summary
	// is the next line in the file and may not be written yet. holdCount defers
	// it — leaving the reader's cursor parked BEFORE it — rather than converting
	// it on half the evidence and throwing the summary away for good.
	originalCount := len(frames)
	frames = frames[:h.holdCount(frames, ctx)]
	if len(frames) != originalCount {
		h.log.With(handleCtx("hold", ctx)).
			With(logging.Context{Offset: logging.Off(ctx.HeldOffset)}).
			Log("deferred the trailing compaction boundary: frames=%d processed=%d held_deliveries=%d",
				originalCount, len(frames), ctx.HeldDeliveries)
	}

	out := convertFrames(h.conv, h.log, frames, ctx)
	h.log.With(handleCtx("transcript-handle", ctx)).
		LogVerbose("handled frames=%d entries=%d", len(frames), len(out))
	return out
}

// Prime implements tail.Primer: the bytes before the first frame this handler
// will be handed are classified for the keep-alive rule, so a keep-alive whose
// prompt a restarted reader resumed past still owns the records after it
// (convert/keepalive.go).
func (h *SessionTranscriptHandler) Prime(prefix io.Reader, ctx *Context) error {
	seed, err := h.conv.SeedKeepalive(prefix)
	if err != nil {
		return err
	}
	h.log.With(handleCtx("keepalive-seed", ctx)).LogVerbose(
		"classified the %d line(s) before the first delivered frame for the keep-alive rule: %d keep-alive record(s) under %d keep-alive prompt(s)",
		seed.Lines, seed.KeepaliveRecords, seed.KeepalivePrompts)
	return nil
}

// convertFrames runs the converter over a batch, turning an unreadable line into
// durable evidence rather than a silent drop.
func convertFrames(conv *convert.Converter, log *logging.Bound, frames []tail.Frame, ctx *Context) []*storev1.StoreEntry {
	var out []*storev1.StoreEntry
	for i, frame := range frames {
		at := attribute(ctx, frame.Offset)
		if frame.ParseErr != nil {
			log.With(handleWarn("parse", ctx)).With(logging.Context{Offset: logging.Off(frame.Offset)}).
				Log("parse failure; the line is classified as unparsed residue and not stored, so the bytes to investigate are this file at this offset: %v", frame.ParseErr)
			unparsed := convert.UnparsedEntry(at, frame.Raw, frame.ParseErr)
			logResidue(log, ctx, frame.Offset, []*storev1.StoreEntry{unparsed})
			out = append(out, unparsed)
			continue
		}
		converted := conv.Line(frame.Obj, at, lookahead(frames, i+1))
		logResidue(log, ctx, frame.Offset, converted)
		out = append(out, converted...)
	}
	return out
}

// maxHoldDeliveries is the SILENCE BOUND on a held compaction boundary: how many
// further deliveries the handler waits for a summary that never arrives before
// converting the boundary without one.
//
// IT IS ONE, and the hold is bounded to exactly one redelivery by ruling. The two
// lines are written about a millisecond apart against a poll interval three
// orders of magnitude longer, so a single further scan is already an enormous
// margin over the gap being covered. Every additional scan of holding only delays
// the cut for the sessions that genuinely stop at a boundary.
const maxHoldDeliveries = 1

// holdCount returns how many frames may be converted NOW, and records on ctx
// whatever it deferred.
//
// It defers exactly one frame, the batch's LAST, and only when that frame is a
// compaction boundary — the sole record whose meaning depends on a line that may
// not be written yet. Everything else is settled by its own bytes.
//
// A DEFERRAL IS ONLY EVER TAKEN WHEN THE READER PROMISES TO HAND THE FRAME BACK
// (ctx.Redelivers). A handler given one standalone batch has no next delivery, so
// holding there would drop the compaction silently; that caller gets the
// summary-less conversion, loud log and all.
func (h *SessionTranscriptHandler) holdCount(frames []tail.Frame, ctx *Context) int {
	n := len(frames)
	// What was deferred LAST delivery. Both fields are rewritten on every call,
	// so a stale hold can never outlive the batch that took it.
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
		// waiting: the boundary converts without a summary, loudly.
		// INFO, NOT WARN: converting without one is no longer a loss. The cut is
		// drawn with the stated placeholder, and a summary that names this
		// boundary supersedes it whenever it arrives, however many deliveries
		// later (convert.attachCompactSummary).
		h.log.With(handleCtx("hold", ctx)).With(logging.Context{Offset: logging.Off(last.Offset)}).
			Log("the held compaction boundary was held for %d delivery and no summary followed; converting it with the placeholder, which a later summary naming it supersedes", heldFor)
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

// Conv exposes the handler's converter, so the reader can read the per-file
// facts the conversion accumulated.
func (h *SessionTranscriptHandler) Conv() *convert.Converter { return h.conv }
