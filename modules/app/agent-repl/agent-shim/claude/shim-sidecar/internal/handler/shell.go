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

// maxExitMarkerDigits bounds the digits accepted after `EXIT=`. A shell exit code
// is 0-255, so anything longer is not the harness's marker.
const maxExitMarkerDigits = 3

// ShellOutputHandler tracks a background shell spool.
type ShellOutputHandler struct {
	conv *convert.Converter
	log  *logging.Bound
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
	if ctx.TaskID == "" {
		// A spool with no task identity names no run, so its bytes have nowhere
		// to accumulate. They are NEVER silently discarded: they land as residue
		// naming the spool, which is what the aged-unowned-spool policy requires.
		h.log.With(handleErr("shell-handle", ctx)).
			Log("shell spool reached the handler with no task identity; its bytes have no run to append to and are stored as residue")
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
	run := ctx.TaskID

	var output bytes.Buffer
	for _, frame := range frames {
		output.Write(frame.Raw)
	}
	// The delta's from_offset is the file position these bytes START at, which is
	// exactly the count the consumer must already hold for this run.
	entries := []*storev1.StoreEntry{h.conv.BashDelta(at, run, output.String(), frames[0].Offset)}

	code, ok := trailingExitCode(frames[0].Raw, frames[0].Offset)
	if !ok {
		h.log.With(handleCtx("shell-handle", ctx)).
			LogVerbose("no terminal exit marker in batch entries=%d", len(entries))
		return entries
	}
	entries = append(entries, h.conv.BashExited(at, run, output.String(), code))
	return entries
}

// Lost states that a detached run stopped being observable. It is called by the
// staleness policy in the root package, never inferred here.
func (h *ShellOutputHandler) Lost(ctx *Context, reason convert.LostReason) *storev1.StoreEntry {
	at := attribute(ctx, ctx.BytesObserved)
	return h.conv.BashLost(at, ctx.TaskID, "", reason)
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
//     the preceding byte in the batch is a newline, or the batch begins at file
//     offset 0 — a command that produced no output at all, which is a real
//     observed case (a 7-byte spool that is exactly `EXIT=0\n`).
//
// A marker split across two polls is NOT matched and is left to the staleness
// policy, which is the pre-existing behavior of the ~91% of shell spools carrying
// no marker at all — not a new silent failure mode.
func trailingExitCode(raw []byte, batchOffset int64) (int, bool) {
	if !bytes.HasSuffix(raw, []byte("\n")) {
		return 0, false
	}
	line := raw[:len(raw)-1]

	// Locate the final line's start, and require it to genuinely BE one.
	start := bytes.LastIndexByte(line, '\n') + 1
	if start == 0 && batchOffset != 0 {
		// The batch begins mid-file with no newline before this text, so it may
		// be the tail of a line that began in an earlier batch.
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
