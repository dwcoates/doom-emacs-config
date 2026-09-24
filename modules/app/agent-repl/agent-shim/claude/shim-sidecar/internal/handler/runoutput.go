package handler

// runoutput.go — the ONE implementation of what a SEAM-MINTED TERMINAL owes.
//
// A terminal the READER concludes (a LOST sweep, a person's stop) is not read
// off the file: the reader states the fact and the converter spells it. But the
// frame still owes two things only the file's own reader holds — the run's
// OUTPUT, and the FILE COORDINATES the write identity is digested from — so
// every handler that can be asked for one has to accumulate both.
//
// IT IS ONE TYPE RATHER THAN ONE COPY PER HANDLER. Two handlers spelling "the
// same terminal" from two private accumulations is exactly how the shapes drift:
// one bounds the remembered output and the other does not, one digests the file
// id and the other the path, and the difference is invisible until a consumer
// reads two runs settled two different ways. Embedding this is what makes "the
// same terminal shape, the same book" a fact about the code rather than a
// promise in a comment.
//
// IT MODELS NOTHING ABOUT THE BYTES. It appends them verbatim to a bounded
// buffer and hands them back verbatim; nothing here parses, classifies, or
// interprets. That is why a handler whose whole job is envelope-level residue
// can embed it without becoming a converter of the bytes it ingests.

import (
	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/convert"
	"agentrepl/shim-claude-sidecar/internal/logging"
)

// maxRememberedOutput bounds what one handler holds of its run's output.
// A terminal has to carry the run's output, so SOMETHING must be held; this is
// how much, and everything past it is reported as omitted rather than silently
// dropped or unboundedly accumulated.
//
// IT IS THE RENDERER'S OWN CAP, AND IT HOLDS THE TAIL. The daemon draws a
// detached shell's last 16 KiB (`spoolCap`, daemon/internal/resolve/feed) and
// draws nothing of a terminal's output at all, so holding a megabyte — the old
// bound, a PREFIX — put up to 1 MiB into every terminal row for nobody to read.
// The run's most recent bytes are what a reader is shown, so they are what a
// terminal keeps.
const maxRememberedOutput = 16 << 10

// fileCoords is where a handler last read: the cursor's own identity for the
// file, and how far into it the handler has seen.
type fileCoords struct {
	Path   string
	FileID string
	Offset int64
}

// RunOutput accumulates what a seam-minted terminal owes, and spells the two
// terminals the reader can conclude.
type RunOutput struct {
	conv *convert.Converter
	log  *logging.Bound
	// seen is the TAIL of what this run has said so far, bounded by
	// maxRememberedOutput, and omitted counts the earlier bytes it dropped.
	//
	// A TERMINAL STATES THE RUN'S OUTPUT, and the only place the whole of it
	// exists is the spool this handler is the sole reader of. The deltas the
	// consumer accumulates are not available to a terminal minted from a
	// staleness conclusion, so without this a LOST or EXITed run settled with an
	// EMPTY output claiming to be `whole` — which erases what the run actually
	// said. The bound is what keeps the cost constant; past it the extent is
	// stated as partial rather than misreported as whole.
	seen    []byte
	omitted uint64
	// read reports that this handler has already accumulated a batch of this
	// run's bytes, which is what a terminal states as `output_observed`.
	read bool
	// coords are the FILE COORDINATES of the last batch this handler read, kept
	// so a terminal minted from the READER'S conclusion — a LOST sweep, a
	// person's stop — is stated at a real position in a real file.
	//
	// WITHOUT THEM THE WRITE IDENTITY COLLAPSES. A seam-minted terminal built
	// from an attribution carrying no file id and no offset digests
	// "producer||0|terminal" for EVERY run in the process, so the second spool
	// concluded LOST mints the write id the first one already used and the store
	// — whose absorption is write_id equality — swallows it as a replay. One of
	// the two runs then has no terminal at all and stays open in every reader
	// downstream. They are set on every Handle and are the same coordinates the
	// cursor is stated in.
	coords fileCoords
}

// NewRunOutput builds the accumulator and the converter its terminals are
// spelled through.
func NewRunOutput(log *logging.Bound) *RunOutput {
	return &RunOutput{conv: convert.New(log), log: log}
}

// Conv answers the converter this accumulator spells through, so a handler that
// also converts its own records does it through ONE converter rather than
// standing a second one beside it.
func (r *RunOutput) Conv() *convert.Converter { return r.conv }

// Read reports whether any batch of this run's bytes has been accumulated.
func (r *RunOutput) Read() bool { return r.read }

// Seen answers the run's output as far as the bound, and how much was omitted.
func (r *RunOutput) Seen() (string, uint64) { return string(r.seen), r.omitted }

// RememberCoords records where this handler has read to, so a terminal the
// READER concludes can be stated at a real file position rather than at the
// zero value every such terminal would otherwise share.
func (r *RunOutput) RememberCoords(ctx *Context) {
	r.coords = fileCoords{Path: ctx.Path, FileID: ctx.FileID, Offset: ctx.BytesObserved}
}

// Remember accumulates the TAIL of the run's output up to the bound, counting
// the earlier bytes it drops.
func (r *RunOutput) Remember(ctx *Context, raw []byte) {
	r.read = true
	crossed := r.omitted == 0
	r.seen = append(r.seen, raw...)
	if len(r.seen) <= maxRememberedOutput {
		return
	}
	drop := len(r.seen) - maxRememberedOutput
	r.omitted += uint64(drop)
	r.seen = append(r.seen[:0], r.seen[drop:]...)
	if !crossed {
		return
	}
	// THE TERMINAL ITSELF CARRIES THE OMITTED COUNT, so the reader is told what
	// it is looking at and nothing is silently truncated. The bound firing is
	// the bound doing its job on a talkative run, so the record is
	// informational, states the counts, and is written once: on the batch that
	// first crosses it.
	r.log.With(handleCtx("run-output-bound", ctx)).Log(
		"the run has said more than %d bytes; its terminal states the last %d and reports %d omitted rather than claiming to carry the whole",
		maxRememberedOutput, maxRememberedOutput, r.omitted)
}

// TerminalAttribution builds the attribution a seam-minted terminal is written
// under.
func (r *RunOutput) TerminalAttribution(taskID, ownerAgentID, run string) convert.Attribution {
	if r.coords.FileID == "" {
		r.log.With(logging.Context{
			Operation: "terminal-attribution", TaskID: taskID,
			AgentID: ownerAgentID, ActivityID: run, Path: r.coords.Path,
		}).LogVerbose("this handler read no batch of the run's spool, so its terminal is identified by the run and states not_observed for its output")
		return convert.Attribution{
			VendorSessionID: ownerAgentID,
			MainAgentID:     ownerAgentID,
			AgentID:         ownerAgentID,
			TaskID:          taskID,
			WriteScope:      convert.RunScope(run),
		}
	}
	return convert.Attribution{
		VendorSessionID: ownerAgentID,
		MainAgentID:     ownerAgentID,
		AgentID:         ownerAgentID,
		TaskID:          taskID,
		Path:            r.coords.Path,
		FileID:          r.coords.FileID,
		Offset:          r.coords.Offset,
	}
}

// Cancelled spells a person's stop as the run's terminal, carrying the output
// this handler has accumulated off the run's file.
//
// IT IS THE SAME SEAM AS Lost, for the same reason: the reader learns the fact
// (from another file's records, or from its own staleness policy) and only this
// side can spell it, because only this side holds the run's bytes.
func (r *RunOutput) Cancelled(taskID, run, ownerAgentID string, settledAtMs int64) []*storev1.StoreEntry {
	if run == "" {
		r.log.With(logging.Context{Operation: "cancel-terminal", Level: "error", TaskID: taskID, AgentID: ownerAgentID}).
			Log("no terminal minted for a stopped run: no spawning-call activity id was supplied, so the frame would name no unit")
		return nil
	}
	at := r.TerminalAttribution(taskID, ownerAgentID, run)
	output, omitted := r.Seen()
	return []*storev1.StoreEntry{r.conv.BashCancelled(at, run, output, omitted, settledAtMs, r.read)}
}

// Lost spells the reader's LOST conclusion as the detached run's terminal.
//
// LOST IS ITS OWN WORD — "we stopped seeing it", not "known failed" — and the
// wire says exactly that: the run resolves as AgentBash.success.interrupted
// with cause = `lost`, whose DetachedLost arm names HOW we concluded it
// (file_vanished | went_silent | swept_up). Setting `by_user` or `timed_out`
// would be an accusation with no evidence; `lost` is the arm that is honest,
// and it is on the wire rather than only in this process's log, so a reader can
// draw the distinction.
//
// The reason is the reader's own vocabulary, and convert.DetachedLostArm
// RAISES on one it does not know rather than leaving the oneof unset: a lost
// run with no arm states nothing, which is worse than the conclusion itself.
func (r *RunOutput) Lost(taskID, runActivityID, ownerAgentID, reason string, catchup bool) []*storev1.StoreEntry {
	// THE RUN IS THE SPAWNING CALL AND NOTHING ELSE. Falling back to the vendor
	// task id would key the terminal on a row no reader of the conversation can
	// join to the call, which is worse than saying nothing: the run would appear
	// settled while the call it belongs to stayed open forever.
	run := runActivityID
	if run == "" {
		// Nothing to name the run by: the terminal would upsert no row. Refused
		// loudly rather than emitted against an invented key.
		r.log.With(logging.Context{Operation: "lost-terminal", Level: "error", TaskID: taskID, AgentID: ownerAgentID}).
			Log("no terminal minted for a LOST run: no spawning-call activity id was supplied, so the frame would name no unit (reason=%s)", reason)
		return nil
	}
	at := r.TerminalAttribution(taskID, ownerAgentID, run)
	output, omitted := r.Seen()
	return []*storev1.StoreEntry{r.conv.BashLost(at, run, output, omitted, convert.LostReason(reason), r.read, catchup)}
}
