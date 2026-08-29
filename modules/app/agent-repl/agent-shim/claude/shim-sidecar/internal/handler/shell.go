package handler

// shell.go — the detached shell spool: unstructured bytes with exactly ONE
// structured thing in them, the `EXIT=<code>` terminator the harness appends when
// the wrapped command finishes.
//
// So the handler does two things: append the bytes to the run as a DELTA carrying
// the offset they start at, and END the run when the marker arrives. Completion is
// never GUESSED — absent the marker this handler infers nothing and the staleness
// policy owns the outcome.

import (
	"bytes"
	"strconv"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/convert"
	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

// exitMarkerPrefix opens the terminator line the harness appends to a shell spool
// when the wrapped command exits: `EXIT=<code>` on its own line.
var exitMarkerPrefix = []byte("EXIT=")

// maxRememberedOutput bounds what one spool handler holds of its run's output.
// A terminal has to carry the run's output, so SOMETHING must be held; this is
// how much, and everything past it is reported as omitted rather than silently
// dropped or unboundedly accumulated.
const maxRememberedOutput = 1 << 20

// maxExitMarkerDigits bounds the digits accepted after `EXIT=`. A shell exit code
// is 0-255, so anything longer is not the harness's marker.
const maxExitMarkerDigits = 3

// ShellOutputHandler tracks a background shell spool.
type ShellOutputHandler struct {
	conv *convert.Converter
	log  *logging.Bound
	// seen is what this run has said SO FAR, bounded by maxRememberedOutput, and
	// omitted counts the bytes past that bound.
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
	// read reports that this handler has already converted a batch of this
	// spool, and endedOnNewline whether that batch's last byte was one. Together
	// they answer the only question the EXIT-marker parser cannot answer from
	// one batch: whether the batch BEGINS a line.
	//
	// THE RAW CODEC CARRIES NOTHING (a spool has no record structure to carry
	// on), so a batch may start mid-line — which is why a marker at the very
	// start of a mid-file batch cannot be trusted on its own. It can be trusted
	// when the previous batch ended on a newline, and only this handler knows
	// that. Without it a spool whose `EXIT=` line simply arrived on its own poll
	// -- the ordinary case for a command that finishes between two polls --
	// never settled on evidence at all and waited out a staleness window.
	read           bool
	endedOnNewline bool
	// onTerminal reports that this handler READ the run's own terminal off the
	// file. The reader owns what that means for the LOST policy; all this side
	// states is that the run ended on evidence rather than on silence.
	onTerminal func(path, run string)
}

// NewShellOutputHandler builds a handler.
func NewShellOutputHandler(log *logging.Bound) *ShellOutputHandler {
	log.With(logging.Context{Operation: "shell-handler-new"}).LogVerbose("constructing shell output handler")
	return &ShellOutputHandler{conv: convert.New(log), log: log}
}

// Handle implements tail.Handler.
func (h *ShellOutputHandler) Handle(frames []tail.Frame, ctx *Context) []*storev1.StoreEntry {
	h.log.With(handleCtx("shell-handle", ctx)).
		LogVerbose("handling frames=%d bytes_observed=%d", len(frames), ctx.BytesObserved)
	if len(frames) == 0 {
		h.log.With(handleCtx("shell-handle", ctx)).
			LogVerbose("no frames to convert")
		return nil
	}
	// THE RUN IS THE SPAWNING CALL, NEVER THE VENDOR TASK ID. A detached command
	// is announced under one identity on both planes — the tool_use_id of the
	// call that launched it — so that is what the spool's frames are keyed by.
	// The reader resolves it from the launch it observed and hands it over on the
	// context; a spool whose owner is unresolved is HELD rather than tailed, so
	// reaching here without one is a reader defect, stated as such.
	run := ctx.RunActivityID
	if run == "" {
		// A spool with no run identity names no run, so its bytes have nowhere
		// to accumulate. They are NEVER silently discarded: they land as residue
		// naming the spool, which is what the aged-unowned-spool policy requires.
		h.log.With(handleErr("shell-handle", ctx)).
			Log("shell spool reached the handler with no spawning-call identity; its bytes have no run to append to and are stored as residue")
		at := attribute(ctx, frames[0].Offset)
		var raw bytes.Buffer
		for _, frame := range frames {
			raw.Write(frame.Raw)
		}
		return []*storev1.StoreEntry{convert.VendorSpecificEntry(at, "unowned_spool", map[string]any{
			"path":   ctx.Path,
			"offset": float64(frames[0].Offset),
			"output": raw.String(),
		})}
	}

	at := attribute(ctx, frames[0].Offset)

	var output bytes.Buffer
	for _, frame := range frames {
		output.Write(frame.Raw)
	}
	atLineStart := h.atLineStart(frames[0].Offset)
	h.observe(output.Bytes())
	h.remember(ctx, output.Bytes())
	// The delta's from_offset is the file position these bytes START at, which is
	// exactly the count the consumer must already hold for this run.
	entries := []*storev1.StoreEntry{h.conv.BashDelta(at, run, output.String(), frames[0].Offset)}

	code, ok := trailingExitCode(frames[0].Raw, atLineStart)
	if !ok {
		h.log.With(handleCtx("shell-handle", ctx)).
			LogVerbose("no terminal exit marker in batch entries=%d", len(entries))
		return entries
	}
	// The terminal states the RUN's output, not this batch's: a spool whose
	// marker arrives on a later poll than its output would otherwise settle
	// carrying only the last chunk while claiming to carry the whole.
	entries = append(entries, h.conv.BashExited(at, run, string(h.seen), h.omitted, code))
	if h.onTerminal != nil {
		// A RUN THAT ENDED ON ITS OWN MARKER CAN NEVER BE LOST. Telling the
		// reader here is what stops the staleness policy restating a finished
		// run as LOST once its finished spool inevitably goes quiet.
		h.onTerminal(ctx.Path, run)
	}
	return entries
}

// Lost states that a detached run stopped being observable. It is called by the
// staleness policy in the root package, never inferred here.
func (h *ShellOutputHandler) Lost(ctx *Context, reason convert.LostReason) *storev1.StoreEntry {
	at := attribute(ctx, ctx.BytesObserved)
	return h.conv.BashLost(at, ctx.RunActivityID, string(h.seen), h.omitted, reason)
}

// atLineStart answers whether a batch beginning at offset starts a line.
func (h *ShellOutputHandler) atLineStart(offset int64) bool {
	if !h.read {
		// A first batch at the file's start begins a line by construction; one
		// that begins mid-file is a resumed cursor, and nothing here knows what
		// preceded it.
		return offset == 0
	}
	return h.endedOnNewline
}

// observe records what the batch says about the NEXT batch's line alignment.
func (h *ShellOutputHandler) observe(raw []byte) {
	h.read = true
	h.endedOnNewline = len(raw) > 0 && raw[len(raw)-1] == '\n'
}

// remember accumulates the run's output up to the bound, counting the rest.
func (h *ShellOutputHandler) remember(ctx *Context, raw []byte) {
	room := maxRememberedOutput - len(h.seen)
	if room <= 0 {
		h.omitted += uint64(len(raw))
		return
	}
	if len(raw) <= room {
		h.seen = append(h.seen, raw...)
		return
	}
	h.seen = append(h.seen, raw[:room]...)
	h.omitted += uint64(len(raw) - room)
	h.log.With(handleWarn("shell-output-bound", ctx)).Log(
		"the run has said more than %d bytes; its terminal will state the first %d and report %d omitted rather than claiming to carry the whole",
		maxRememberedOutput, maxRememberedOutput, h.omitted)
}

// trailingExitCode reads the `EXIT=<code>` terminator off the END of a raw spool
// batch, returning the code and whether the marker was found.
//
// The matching is deliberately strict, because `EXIT=` is COMMON as ordinary
// command output. Measured over the SHELL spools this parser actually reads
// (`b*.output`): of 234 shell spools, 44 contain the substring `EXIT=` at all, 23
// of those carry it ONLY mid-line as script output (`BUILD_EXIT=0`,
// `WEBAPP_TEST_EXIT=`, …), and 21 carry a line-start `EXIT=<digits>`. A loose
// match would end those 23 runs early and wrongly. So:
//
//   - The marker must be the LAST thing in the batch, newline-terminated. The
//     tailer reads to the file's current EOF, so "end of batch" is "end of file as
//     of this poll". A marker does NOT always terminate its spool: of the 21 shell
//     spools carrying a line-start marker, 19 end on it, 2 have further output
//     after an early one, and 1 of the 19 carries two markers — so 3 of the 21 have
//     a marker that is not the sole final line. The last-line-of-batch rule is what
//     makes those safe.
//   - Between `EXIT=` and the newline there must be ONLY digits, at most
//     maxExitMarkerDigits of them. A stray `EXIT=abc` fails here.
//   - The marker must start a LINE, which is what rejects `BUILD_EXIT=0`: either
//     the preceding byte in the batch is a newline, or the BATCH ITSELF begins a
//     line — which it does at file offset 0 (a command that produced no output
//     at all, a real observed case: a 7-byte spool that is exactly `EXIT=0\n`)
//     and whenever the previous batch this handler read ended on a newline.
//
// A marker split across two polls is NOT matched and is left to the staleness
// policy, which is the pre-existing behavior of the ~91% of shell spools carrying
// no marker at all — not a new silent failure mode.
func trailingExitCode(raw []byte, batchAtLineStart bool) (int, bool) {
	if !bytes.HasSuffix(raw, []byte("\n")) {
		return 0, false
	}
	line := raw[:len(raw)-1]

	// Locate the final line's start, and require it to genuinely BE one.
	start := bytes.LastIndexByte(line, '\n') + 1
	if start == 0 && !batchAtLineStart {
		// The batch does not begin a line and holds no newline before this text,
		// so this may be the tail of a line that began in an earlier batch.
		return 0, false
	}
	line = line[start:]

	if !bytes.HasPrefix(line, exitMarkerPrefix) {
		return 0, false
	}
	digits := line[len(exitMarkerPrefix):]
	if len(digits) == 0 || len(digits) > maxExitMarkerDigits {
		return 0, false
	}
	for _, c := range digits {
		if c < '0' || c > '9' {
			return 0, false
		}
	}
	code, err := strconv.Atoi(string(digits))
	if err != nil {
		// Unreachable: every byte was checked to be a digit and the length is
		// bounded. Kept so that weakening either guard cannot turn a parse
		// failure into a silently wrong exit code.
		return 0, false
	}
	return code, true
}
