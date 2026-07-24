package handler

import (
	"bytes"
	"strconv"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

// exitMarkerPrefix opens the terminator line the harness appends to a shell
// spool when the wrapped command exits: `EXIT=<code>` on its own line.
var exitMarkerPrefix = []byte("EXIT=")

// maxExitMarkerDigits bounds the digits accepted after `EXIT=`. A shell exit
// code is 0-255, so anything longer is not the harness's marker and must not
// be read as one.
const maxExitMarkerDigits = 3

// ShellOutputHandler tracks a background shell spool (b*.output). Shell spools
// carry no structure the sidecar interprets beyond ONE thing (§7.2): the
// handler emits a byte-count TaskProgress per batch, plus a TaskEnded when the
// spool's `EXIT=<code>` terminator arrives.
//
// The terminator used to be ignored, which meant a shell task that had plainly
// finished — and said so, on disk — stayed "running" until the staleness sweep
// eventually declared it LOST. That is the wrong status (LOST means "we never
// found out", and here we did find out), and it arrives late. Reading the
// marker is the total-ingestion mandate applied to the one structured byte a
// shell spool has.
//
// Completion is still never GUESSED: absent the marker this handler infers
// nothing and the staleness policy owns the outcome exactly as before (§7.4).
type ShellOutputHandler struct {
	log Logf
}

// NewShellOutputHandler builds a handler.
func NewShellOutputHandler(log Logf) *ShellOutputHandler {
	return &ShellOutputHandler{log: log}
}

// Handle implements Handler.
func (h *ShellOutputHandler) Handle(frames []tail.Frame, ctx *Context) []*corev1.Event {
	if len(frames) == 0 {
		return nil
	}
	events := []*corev1.Event{taskProgressEvent(ctx.SessionID, corev1.Plane_PLANE_FILE, &corev1.TaskProgress{
		TaskId:        ctx.TaskID,
		Kind:          corev1.TaskKind_TASK_KIND_SHELL,
		BytesObserved: ctx.BytesObserved,
	})}

	code, ok := trailingExitCode(frames[0].Raw, frames[0].Offset)
	if !ok {
		return events
	}
	status := corev1.TerminalStatus_TERMINAL_STATUS_DONE
	if code != 0 {
		status = corev1.TerminalStatus_TERMINAL_STATUS_ERROR
	}
	h.log("shell: task=%s EXIT=%d -> %s (exit-marker)", ctx.TaskID, code, status)
	return append(events, taskEndedEvent(ctx.SessionID, corev1.Plane_PLANE_FILE, &corev1.TaskEnded{
		TaskId:     ctx.TaskID,
		Kind:       corev1.TaskKind_TASK_KIND_SHELL,
		Status:     status,
		OutputPath: ctx.Path,
		Inference:  "exit-marker",
	}))
}

// trailingExitCode reads the `EXIT=<code>` terminator off the END of a raw
// spool batch, returning the code and whether the marker was found.
//
// The matching is deliberately strict, because `EXIT=` is COMMON as ordinary
// command output: across the shell spools on this machine, 109 of the 128
// files containing the substring carry it mid-file as script output
// (`BUILD_EXIT=0`, `WEBAPP_TEST_EXIT=`, …) and only 19 carry it as the real
// terminator. A loose match would end those 109 tasks early and wrongly. So:
//
//   - The marker must be the LAST thing in the batch, newline-terminated. The
//     tailer reads to the file's current EOF, so "end of batch" is "end of
//     file as of this poll" — which is what "the spool ends with it" means.
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
// which is also the behavior of the ~98% of spools carrying no marker at all —
// not a new silent failure mode.
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
