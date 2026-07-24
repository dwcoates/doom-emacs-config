// taskoutput.go holds the pure detached-task-output helpers the
// GET /sessions/{id}/tasks/{taskId}/output route depends on: the two
// path-confinement predicates that keep a caller-supplied output path inside
// the CLI's own spool (or a subagent workflow journal), and the bounded
// tail-read the webapp watcher-poll advances a byte cursor across.
//
// These were ported verbatim from the deleted internal/session tailer when the
// daemon moved to consuming each session's UDS shim: the task-output path now
// comes off the driver's rebuilt TaskEntry rather than a live Session, but the
// confinement rules and the split-rune-safe tail read are unchanged — a
// caller-supplied path is still validated down to a known-safe shape before any
// read happens.
package server

import (
	"io"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"unicode/utf8"
)

// taskSpoolRe matches the CLI's per-uid task spool root
// (/tmp/claude-<uid>/ or its /private-prefixed macOS realpath).
var taskSpoolRe = regexp.MustCompile(`^(/private)?/tmp/claude-\d+/`)

// allowedTaskOutputPath reports whether path is a detached-task output file
// inside the CLI's own spool: rooted at the per-uid spool, under a /tasks/
// segment, and named *.output. A caller-supplied path failing any of these is
// refused rather than read.
func allowedTaskOutputPath(path string) bool {
	clean := filepath.Clean(path)
	return taskSpoolRe.MatchString(clean) && strings.Contains(clean, "/tasks/") && strings.HasSuffix(clean, ".output")
}

// allowedJournalPath reports whether path is a subagent workflow journal under
// the session's own config root: <configRoot>/projects/.../subagents/workflows/
// .../journal.jsonl. An empty configRoot admits nothing (there is no root to
// confine against).
func allowedJournalPath(path, configRoot string) bool {
	if configRoot == "" {
		return false
	}
	clean := filepath.Clean(path)
	return strings.HasPrefix(clean, filepath.Clean(configRoot)+"/projects/") &&
		strings.Contains(clean, "/subagents/workflows/") &&
		strings.HasSuffix(clean, "/journal.jsonl")
}

// taskTailChunkMax bounds a single tail read: a watcher-poll advances a byte
// cursor across polls, so one read never has to return the whole file.
const taskTailChunkMax = 8 * 1024

// readTailChunk reads up to taskTailChunkMax bytes of path starting at offset,
// trimming a trailing split UTF-8 rune so the returned text is always valid
// UTF-8 and the next cursor resumes on a rune boundary. A not-yet-created file
// is not an error: it returns ("", offset, nil) so the poller simply retries.
func readTailChunk(path string, offset int64) (string, int64, error) {
	f, err := os.Open(path)
	if err != nil {
		if os.IsNotExist(err) {
			return "", offset, nil
		}
		return "", offset, err
	}
	defer f.Close()
	if _, err := f.Seek(offset, io.SeekStart); err != nil {
		return "", offset, err
	}
	buf := make([]byte, taskTailChunkMax)
	n, _ := f.Read(buf)
	if n <= 0 {
		return "", offset, nil
	}
	trimmed := n
	for trimmed > 0 && n-trimmed < utf8.UTFMax-1 && !utf8.Valid(buf[:trimmed]) {
		trimmed--
	}
	if trimmed == 0 || !utf8.Valid(buf[:trimmed]) {
		return strings.ToValidUTF8(string(buf[:n]), "�"), offset + int64(n), nil
	}
	return string(buf[:trimmed]), offset + int64(trimmed), nil
}
