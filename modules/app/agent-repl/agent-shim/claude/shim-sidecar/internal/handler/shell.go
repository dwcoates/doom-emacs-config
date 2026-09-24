package handler

// shell.go — the detached shell spool: unstructured bytes with exactly THREE
// structured things in them, all terminators — the `EXIT=<code>` line our own
// harness scripts append, and the `[exited with code N]` and `[killed]` lines
// the vendor's background-shell wrapper appends. Any one of them ends the run;
// when a harness marker sits above a wrapper line the harness's is the
// command's verdict and wins.
//
// So the handler does two things: fold the bytes into the run's rendered TAIL,
// which supersedes the run's one tail row whole, and END the run when the
// marker arrives. Completion is
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

// wrapperExitPrefix opens the OTHER terminator a shell spool can carry, and the
// one the vendor's own background-shell wrapper writes: `[exited with code N]`
// on its own final line.
//
// THIS READER USED TO KNOW ONLY `EXIT=`, AND THAT WAS A DEFECT, not a lifecycle
// outcome. `EXIT=` is written by OUR harness scripts; the bracket line is written
// by the tool that detached the shell in the first place, so it is present on
// runs no script of ours wrapped. Measured over the shell spools of one session
// (67 of them, 2026-09-13): 32 end on the bracket line, 11 carry an `EXIT=`
// marker, and ALL 11 of those also carry the bracket line AFTER it. So the
// bracket is the outer terminator and `EXIT=` the inner one — and a spool where
// both land in the same poll failed the old "the marker is the batch's last
// line" rule outright, leaving a run that plainly ended to sit open until a
// silence window concluded it LOST. Realtest 2026-09-13 harvested exactly that:
// three runs whose spools end `[exited with code 0]` were each written up as
// `went_silent`.
var wrapperExitPrefix = []byte("[exited with code ")

// wrapperKilledLine is the THIRD terminator, and the one this reader did not
// know: the vendor's wrapper writes it, alone on the spool's final line, for a
// run that was killed and never reported a status.
//
// NOT KNOWING IT WAS A DEFECT WITH A MEASURED COST. Across the task spools of
// one machine (2026-09-13), 121 `b*` spools end on this line against 765 ending
// on `[exited with code N]` — every one of those 121 a run that plainly ended
// and that this reader left open for its silence window to conclude LOST
// instead. The realtest harvest caught exactly that: `bpth8pp8m.output`, 27
// bytes ending `[killed]`, written at 16:21:33 and concluded
// `went_silent` at 16:51:47, thirty minutes to the second later.
//
// A KILL IS ITS OWN ENDING, NOT AN EXIT CODE. `AgentBashTermination` draws the
// two apart on purpose ("an exit code and a kill are different endings and only
// one of them has a number"), so this settles on the `killed` arm rather than
// inventing a status the shell never reported.
var wrapperKilledLine = []byte("[killed]")

// maxExitMarkerDigits bounds the digits accepted after `EXIT=`. A shell exit code
// is 0-255, so anything longer is not the harness's marker.
const maxExitMarkerDigits = 3

// ShellOutputHandler tracks a background shell spool.
type ShellOutputHandler struct {
	// RunOutput is the run's accumulated output, its file coordinates, and the
	// two terminals the reader can conclude. It is EMBEDDED rather than
	// reimplemented so this handler and every other one that can be asked for a
	// seam-minted terminal spell the identical frame from the identical bytes.
	*RunOutput
	log *logging.Bound
	// endedOnNewline reports whether the last batch this handler read ended on a
	// newline. Together with RunOutput.Read it answers the only question the
	// EXIT-marker parser cannot answer from one batch: whether the batch BEGINS
	// a line.
	//
	// THE RAW CODEC CARRIES NOTHING (a spool has no record structure to carry
	// on), so a batch may start mid-line — which is why a marker at the very
	// start of a mid-file batch cannot be trusted on its own. It can be trusted
	// when the previous batch ended on a newline, and only this handler knows
	// that. Without it a spool whose `EXIT=` line simply arrived on its own poll
	// -- the ordinary case for a command that finishes between two polls --
	// never settled on evidence at all and waited out a staleness window.
	endedOnNewline bool
	// onTerminal reports that this handler READ the run's own terminal off the
	// file. The reader owns what that means for the LOST policy; all this side
	// states is that the run ended on evidence rather than on silence.
	onTerminal func(path, run string)
}

// NewShellOutputHandler builds a handler.
func NewShellOutputHandler(log *logging.Bound) *ShellOutputHandler {
	log.With(logging.Context{Operation: "shell-handler-new"}).LogVerbose("constructing shell output handler")
	return &ShellOutputHandler{RunOutput: NewRunOutput(log), log: log}
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
		h.RememberCoords(ctx)
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
	h.RememberCoords(ctx)

	var output bytes.Buffer
	for _, frame := range frames {
		output.Write(frame.Raw)
	}
	atLineStart := h.atLineStart(frames[0].Offset)
	h.observe(output.Bytes())
	// THE RUN'S TAIL, NOT THE BATCH. Output past what is rendered is never
	// stored (owner ruling 2026-09-23), so each batch supersedes the run's one
	// tail row with the window as it is drawn. The row is identified by where
	// the window ends: the tail through a file position is a pure function of
	// the file's prefix, so a batch re-read after an unacknowledged write mints
	// the same identity and the same bytes.
	var entries []*storev1.StoreEntry
	through := frames[0].Offset + int64(output.Len())
	if err := h.Absorb(ctx, frames[0].Offset, output.Bytes()); err != nil {
		// THE TAIL IS WITHHELD, NOT GUESSED. A window missing the file's prefix
		// would state omitted counts that are wrong for the rest of the run, so
		// this batch writes no tail and the next one reseeds from the file. The
		// batch's bytes are not lost: they are in the prefix that reseed reads.
		h.log.With(handleErr("shell-tail", ctx)).With(logging.Context{ActivityID: run, Offset: logging.Off(frames[0].Offset)}).
			Log("the run's tail could not be rebuilt from its spool; no tail row is written for this batch and the next batch reseeds: %v", err)
	} else {
		text, bytesOmitted, linesOmitted := h.Rendered()
		entries = append(entries, h.Conv().BashTail(attribute(ctx, through), run, text, bytesOmitted, linesOmitted))
	}

	end, ok := trailingTerminator(frames[0].Raw, atLineStart)
	if !ok {
		h.log.With(handleCtx("shell-handle", ctx)).
			LogVerbose("no terminal exit marker in batch entries=%d", len(entries))
		return entries
	}
	// The terminal states the RUN's output, not this batch's: a spool whose
	// marker arrives on a later poll than its output would otherwise settle
	// carrying only the last chunk while claiming to carry the whole.
	seen, omitted := h.Seen()
	if end.killed {
		entries = append(entries, h.Conv().BashKilled(at, run, seen, omitted))
	} else {
		entries = append(entries, h.Conv().BashExited(at, run, seen, omitted, end.code))
	}
	if h.onTerminal != nil {
		// A RUN THAT ENDED ON ITS OWN MARKER CAN NEVER BE LOST. Telling the
		// reader here is what stops the staleness policy restating a finished
		// run as LOST once its finished spool inevitably goes quiet.
		h.onTerminal(ctx.Path, run)
	}
	return entries
}

// atLineStart answers whether a batch beginning at offset starts a line.
func (h *ShellOutputHandler) atLineStart(offset int64) bool {
	if !h.Read() {
		// A first batch at the file's start begins a line by construction; one
		// that begins mid-file is a resumed cursor, and nothing here knows what
		// preceded it.
		return offset == 0
	}
	return h.endedOnNewline
}

// observe records what the batch says about the NEXT batch's line alignment.
// It is read BEFORE RunOutput.Remember marks the run as read, which is why the
// two are separate calls rather than one.
func (h *ShellOutputHandler) observe(raw []byte) {
	h.endedOnNewline = len(raw) > 0 && raw[len(raw)-1] == '\n'
}

// spoolEnd is HOW a spool's terminator said the run ended: with a status the
// shell reported, or with a kill that reported none. The two are different
// endings, so the reader carries the difference rather than flattening a kill
// into a code nobody wrote.
type spoolEnd struct {
	killed bool
	code   int
}

// trailingTerminator reads a terminator off the END of a raw spool batch,
// returning how the run ended and whether any terminator was found.
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
// The SAME strictness governs the wrapper's `[exited with code N]` line: it must
// be the batch's last line, it must start a line, the text between the prefix and
// the closing bracket must be nothing but at most maxExitMarkerDigits digits, and
// the bracket must close the line. A run that merely PRINTS that sentence
// mid-stream therefore does not end here. The wrapper's `[killed]` line is held
// to the same rule: the batch's last line and nothing else on it.
//
// WHEN BOTH TERMINATORS ARE PRESENT, `EXIT=` WINS. The bracket reports the
// WRAPPER's exit — it reads `[exited with code 0]` above a harness `EXIT=77`,
// because the wrapper ran fine and the command did not — so taking the bracket's
// code there would report a failed command as a clean one.
//
// A marker split across two polls is NOT matched and is left to the staleness
// policy, which is the pre-existing behavior of the shell spools carrying no
// terminator at all — not a new silent failure mode.
func trailingTerminator(raw []byte, batchAtLineStart bool) (spoolEnd, bool) {
	before, line, ok := finalLine(raw, batchAtLineStart)
	if !ok {
		return spoolEnd{}, false
	}
	if code, ok := parseExitMarker(line); ok {
		return spoolEnd{code: code}, true
	}
	if bytes.Equal(line, wrapperKilledLine) {
		// A kill reports no status, so a harness marker above it is the only
		// status there is — the same precedence the exit line below follows.
		if inner, ok := parseExitMarker(lastLineOf(before, batchAtLineStart)); ok {
			return spoolEnd{code: inner}, true
		}
		return spoolEnd{killed: true}, true
	}
	code, ok := parseWrapperExit(line)
	if !ok {
		return spoolEnd{}, false
	}
	// The wrapper's own line ends the run, but the wrapped command's verdict
	// beats the wrapper's whenever the harness recorded one right above it.
	if inner, ok := parseExitMarker(lastLineOf(before, batchAtLineStart)); ok {
		return spoolEnd{code: inner}, true
	}
	return spoolEnd{code: code}, true
}

// finalLine splits a newline-terminated batch into everything before its last
// line and the last line itself, refusing a last line that cannot be shown to
// START one.
func finalLine(raw []byte, batchAtLineStart bool) (before, line []byte, ok bool) {
	if !bytes.HasSuffix(raw, []byte("\n")) {
		return nil, nil, false
	}
	body := raw[:len(raw)-1]
	start := bytes.LastIndexByte(body, '\n') + 1
	if start == 0 && !batchAtLineStart {
		// The batch does not begin a line and holds no newline before this text,
		// so this may be the tail of a line that began in an earlier batch.
		return nil, nil, false
	}
	return body[:start], body[start:], true
}

// lastLineOf returns the final non-empty line of a batch prefix, skipping the
// blank lines the wrapper leaves between the command's output and its own
// terminator. It returns nil when no line there can be shown to start one, which
// is what keeps an `EXIT=` fragment carried over from an earlier poll out.
func lastLineOf(before []byte, batchAtLineStart bool) []byte {
	trimmed := bytes.TrimRight(before, "\n")
	if len(trimmed) == 0 {
		return nil
	}
	start := bytes.LastIndexByte(trimmed, '\n') + 1
	if start == 0 && !batchAtLineStart {
		return nil
	}
	return trimmed[start:]
}

// parseExitMarker reads `EXIT=<code>` off a whole line.
func parseExitMarker(line []byte) (int, bool) {
	if !bytes.HasPrefix(line, exitMarkerPrefix) {
		return 0, false
	}
	return parseExitDigits(line[len(exitMarkerPrefix):])
}

// parseWrapperExit reads `[exited with code <code>]` off a whole line.
func parseWrapperExit(line []byte) (int, bool) {
	if !bytes.HasPrefix(line, wrapperExitPrefix) || !bytes.HasSuffix(line, []byte("]")) {
		return 0, false
	}
	return parseExitDigits(line[len(wrapperExitPrefix) : len(line)-1])
}

// parseExitDigits accepts the bounded run of digits either terminator's code is
// spelled with, and nothing else.
func parseExitDigits(digits []byte) (int, bool) {
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
