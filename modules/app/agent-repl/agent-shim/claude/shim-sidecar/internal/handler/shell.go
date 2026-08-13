package handler

import (
	"bytes"
	"strconv"

	agentshimv1 "agentrepl/proto/agentshim/v1"
	"agentrepl/shim-claude-sidecar/internal/convert"
	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

// exitMarkerPrefix opens the terminator line the harness appends to a shell
// spool when the wrapped command exits: `EXIT=<code>` on its own line.
var exitMarkerPrefix = []byte("EXIT=")

// maxExitMarkerDigits bounds the digits accepted after `EXIT=`. A shell exit
// code is 0-255, so anything longer is not the harness's marker and must not be
// read as one.
const maxExitMarkerDigits = 3

// ShellOutputHandler tracks a background shell spool.
//
// A spool is UNSTRUCTURED BYTES with exactly one structured thing in it: the
// `EXIT=<code>` terminator the harness appends when the command finishes. So the
// handler does two things — append the bytes to the run's card as output, and
// end the card when the marker arrives.
//
// READING THE MARKER IS THE TOTAL-INGESTION MANDATE APPLIED TO THE ONE
// STRUCTURED BYTE A SPOOL HAS. It used to be ignored, which meant a shell task
// that had plainly finished — and said so, on disk — stayed running until the
// staleness sweep eventually declared it LOST. That is the wrong verdict as well
// as a late one: LOST means we never found out, and here we did.
//
// Completion is still never GUESSED. Absent the marker this handler infers
// nothing and the staleness policy owns the outcome exactly as before.
type ShellOutputHandler struct {
	log *logging.Bound
}

// NewShellOutputHandler builds a handler.
func NewShellOutputHandler(log *logging.Bound) *ShellOutputHandler {
	log.With(logging.Context{Operation: "shell-handler-new"}).LogVerbose("constructing shell output handler")
	return &ShellOutputHandler{log: log}
}

// Handle implements tail.Handler.
func (h *ShellOutputHandler) Handle(frames []tail.Frame, ctx *Context) []*agentshimv1.Entry {
	h.log.With(logging.Context{Operation: "shell-handle", Path: ctx.Path, Session: ctx.SessionID, Task: ctx.TaskID}).
		LogVerbose("handling frames=%d bytes_observed=%d", len(frames), ctx.BytesObserved)
	if len(frames) == 0 {
		h.log.With(logging.Context{Operation: "shell-handle", Path: ctx.Path, Session: ctx.SessionID, Task: ctx.TaskID}).
			LogVerbose("no frames to convert")
		return nil
	}
	if ctx.TaskID == "" {
		// A spool with no task identity names no card, so its bytes have nowhere
		// to accumulate. It is never silently discarded: the sidecar refuses to
		// tail an unattributed spool at all (see the owner index), so reaching
		// here means that guarantee broke.
		h.log.With(logging.Context{Operation: "shell-handle", Path: ctx.Path, Session: ctx.SessionID, Level: "error"}).
			Log("shell spool reached the handler with no task identity; its bytes have no card to append to")
		return nil
	}

	at := attribute(ctx, frames[0].Offset)
	var output bytes.Buffer
	for _, frame := range frames {
		output.Write(frame.Raw)
	}
	entries := []*agentshimv1.Entry{convert.DetachedProgress(at, ctx.TaskID, output.String())}

	code, ok := trailingExitCode(frames[0].Raw, frames[0].Offset)
	if !ok {
		h.log.With(logging.Context{Operation: "shell-handle", Path: ctx.Path, Session: ctx.SessionID, Task: ctx.TaskID}).
			LogVerbose("no terminal exit marker in batch entries=%d", len(entries))
		return entries
	}
	h.log.With(logging.Context{Operation: "exit-marker", Path: ctx.Path, Session: ctx.SessionID, Task: ctx.TaskID}).
		Log("EXIT=%d observed on disk; ending the card on evidence rather than on a silence timeout", code)
	entries = append(entries, convert.DetachedExited(at, ctx.TaskID, code))
	h.log.With(logging.Context{Operation: "shell-handle", Path: ctx.Path, Session: ctx.SessionID, Task: ctx.TaskID}).
		LogVerbose("terminal marker converted entries=%d", len(entries))
	return entries
}

// trailingExitCode reads the `EXIT=<code>` terminator off the END of a raw
// spool batch, returning the code and whether the marker was found.
//
// The matching is deliberately strict, because `EXIT=` is COMMON as ordinary
// command output. Measured over the SHELL spools this parser actually reads
// (`b*.output`; the 1,049 `a*.output` agent transcripts alongside them are
// never fed here, and an earlier version of this note wrongly counted them):
// of 234 shell spools, 44 contain the substring `EXIT=` at all, 23 of those
// carry it ONLY mid-line as script output (`BUILD_EXIT=0`, `WEBAPP_TEST_EXIT=`,
// …), and 21 carry a line-start `EXIT=<digits>`. A loose match would end those
// 23 tasks early and wrongly. So:
//
//   - The marker must be the LAST thing in the batch, newline-terminated. The
//     tailer reads to the file's current EOF, so "end of batch" is "end of
//     file as of this poll" — which is what "the spool ends with it" means.
//     A marker does NOT always terminate its spool: of the 21 shell spools
//     carrying a line-start marker, 19 end on it, 2 have further output after
//     an early one, and 1 of the 19 carries two markers (an early one plus the
//     terminating one) — so 3 of the 21 have a marker that is not the sole
//     final line. The last-line-of-batch rule is what makes those safe: an
//     early marker is not at the end of its batch, so it is not read, and the
//     task ends on the real final marker or not at all.
//   - Between `EXIT=` and the newline there must be ONLY digits, at most
//     maxExitMarkerDigits of them. A stray `EXIT=abc` fails here.
//   - The marker must start a LINE, which is what rejects `BUILD_EXIT=0`:
//     either the preceding byte in the batch is a newline, or the batch begins
//     at file offset 0 — a command that produced no output at all, which is a
//     real observed case (a 7-byte spool that is exactly `EXIT=0\n`).
//
// A marker split across two polls is NOT matched, and is left to the staleness
// policy. That needs a batch boundary to land inside the final ~7 bytes of the
// file, which requires the spool to grow past the tailer's 4MiB per-poll read
// bound in one interval. The result is the pre-existing no-marker behavior,
// which is also the behavior of the ~91% of shell spools carrying no marker at
// all (213 of 234) — not a new silent failure mode.
func trailingExitCode(raw []byte, batchOffset int64) (int, bool) {
	if !bytes.HasSuffix(raw, []byte("\n")) {
		return 0, false
	}
	line := raw[:len(raw)-1]

	// Locate the final line's start, and require it to genuinely BE one.
	start := bytes.LastIndexByte(line, '\n') + 1
	if start == 0 && batchOffset != 0 {
		// The batch begins mid-file with no newline before this text, so it
		// may be the tail of a line that began in an earlier batch.
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
