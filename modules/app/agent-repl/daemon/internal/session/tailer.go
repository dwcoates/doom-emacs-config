// Detached-task output tailing (§2.6 task-output-delta).
//
// A backgrounded Bash's result announces where its output lands:
// "Command running in background with ID: bg1. Output is being written
// to: /tmp/claude-<uid>/<slug>/<session>/tasks/bg1.output". The SDK
// streams none of that file — the only in-band signals are the elapsed
// heartbeat and the final notification — so the daemon tails the file
// itself and broadcasts the growth as task-output-delta frames.
//
// Confinement is non-negotiable: the tailer reads ONLY inside the
// harness task spool (allowedTaskOutputPath), never an arbitrary path a
// result happens to mention. Emission is coalesced (one frame per poll
// tick, chunk-capped) and budgeted (a total cap ends the tail with a
// truncation notice) so a chatty task cannot flood the retention ring
// and evict the conversation it belongs to.
package session

import (
	"io"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"time"
	"unicode/utf8"

	"claude-repld/internal/protocol"
)

const (
	// DefaultTaskTailInterval is the poll cadence, which is also the
	// coalescing window: at most one delta frame per task per tick.
	DefaultTaskTailInterval = 500 * time.Millisecond
	// taskTailChunkMax caps one frame's text.
	taskTailChunkMax = 8 * 1024
	// taskTailTotalMax caps a whole task's streamed output; the rest
	// stays on disk, reachable by reading the file.
	taskTailTotalMax = 64 * 1024
)

var (
	// Backgrounded Bash: "with ID: bg1"; Workflow: "Task ID: wj7025gze".
	taskSpawnIDRe   = regexp.MustCompile(`(?:with|Task) ID:\s*([A-Za-z0-9_-]+)`)
	taskSpawnPathRe = regexp.MustCompile(`Output is being written to:\s*(\S+\.output)`)
	// Workflow spawn results announce the run's transcript directory;
	// its journal.jsonl is the run's progress stream.
	workflowDirRe = regexp.MustCompile(`Transcript dir:\s*(\S+)`)
	taskSpoolRe   = regexp.MustCompile(`^(/private)?/tmp/claude-\d+/`)
	// Background agents announce differently from shells and workflows:
	// "agentId: a1 … output_file: /tmp/claude-<uid>/…/tasks/a1.output".
	// Their output file lands in the same tasks spool, so the same
	// confinement predicate guards it (see parseAgentSpawn).
	agentSpawnIDRe   = regexp.MustCompile(`agentId:\s*([A-Za-z0-9_-]+)`)
	agentSpawnPathRe = regexp.MustCompile(`output_file:\s*(\S+\.output)`)
)

// spawnAnnouncement is the (task id, output file) pair a backgrounded
// tool result announces, when it announces one this daemon may tail.
type spawnAnnouncement struct {
	TaskID    string
	Path      string
	ToolUseID string
}

// parseSpawnAnnouncement extracts the announcement from a tool result
// frame; nil when the result is an error, announces nothing, or names a
// path outside the trees the tailer may read. configRoot is the
// session's Claude config dir, which is where a workflow journal must
// live (a backgrounded shell's output lives in the tmp spool instead).
func parseSpawnAnnouncement(f protocol.L2Frame, configRoot string) *spawnAnnouncement {
	r, ok := f.(*protocol.ToolUseResultFrame)
	if !ok || r.IsError {
		return nil
	}
	text := contentText(r.Content)
	id := taskSpawnIDRe.FindStringSubmatch(text)
	if id == nil {
		return nil
	}
	if path := taskSpawnPathRe.FindStringSubmatch(text); path != nil && allowedTaskOutputPath(path[1]) {
		return &spawnAnnouncement{
			TaskID:    id[1],
			Path:      filepath.Clean(path[1]),
			ToolUseID: r.ToolUseID,
		}
	}
	if dir := workflowDirRe.FindStringSubmatch(text); dir != nil {
		journal := filepath.Join(filepath.Clean(dir[1]), "journal.jsonl")
		if allowedJournalPath(journal, configRoot) {
			return &spawnAnnouncement{
				TaskID:    id[1],
				Path:      journal,
				ToolUseID: r.ToolUseID,
			}
		}
	}
	return nil
}

// allowedJournalPath confines workflow-journal tailing to the session's
// own config root: <root>/projects/**/subagents/workflows/<run>/journal.jsonl
// and nothing else.
func allowedJournalPath(path, configRoot string) bool {
	if configRoot == "" {
		return false
	}
	clean := filepath.Clean(path)
	return strings.HasPrefix(clean, filepath.Clean(configRoot)+"/projects/") &&
		strings.Contains(clean, "/subagents/workflows/") &&
		strings.HasSuffix(clean, "/journal.jsonl")
}

// allowedTaskOutputPath confines the tailer to the harness task spool:
// a cleaned path under /tmp/claude-<uid>/ (or its /private twin) whose
// file sits in a tasks/ directory with the .output suffix.
func allowedTaskOutputPath(path string) bool {
	clean := filepath.Clean(path)
	return taskSpoolRe.MatchString(clean) &&
		strings.Contains(clean, "/tasks/") &&
		strings.HasSuffix(clean, ".output")
}

// parseAgentSpawn extracts a background agent's (id, output file) from a
// tool result: "agentId: a1 … output_file: <path>". Its output file lands
// in the same tasks spool a shell's does, so allowedTaskOutputPath guards
// it identically. Recorded for the poll route but NOT tailed — an agent's
// output file is its full JSONL transcript, far too verbose to push into
// the retention ring.
func parseAgentSpawn(f protocol.L2Frame) *spawnAnnouncement {
	r, ok := f.(*protocol.ToolUseResultFrame)
	if !ok || r.IsError {
		return nil
	}
	text := contentText(r.Content)
	id := agentSpawnIDRe.FindStringSubmatch(text)
	path := agentSpawnPathRe.FindStringSubmatch(text)
	if id == nil || path == nil || !allowedTaskOutputPath(path[1]) {
		return nil
	}
	return &spawnAnnouncement{TaskID: id[1], Path: filepath.Clean(path[1]), ToolUseID: r.ToolUseID}
}

// taskPathRec is a detached task's output location plus what the poll
// route needs beyond the bytes: when the task was armed (for a live
// elapsed the SDK heartbeat stops feeding once the spawning call settles)
// and whether its completion notification has landed.
type taskPathRec struct {
	path      string
	spawnedAt time.Time
	done      bool
}

// recordTaskPathLocked durably records a detached task's output path (see
// taskPaths). First path wins, so a later notification cannot move a live
// tail's file out from under it. Callers hold s.mu.
func (s *Session) recordTaskPathLocked(taskID, path string) {
	if taskID == "" || path == "" {
		return
	}
	if _, ok := s.taskPaths[taskID]; ok {
		return
	}
	s.taskPaths[taskID] = taskPathRec{path: path, spawnedAt: s.now()}
}

// markTaskDoneLocked flips a recorded task's done flag once its completion
// notification lands. A notification for an unrecorded task id is ignored.
// Callers hold s.mu.
func (s *Session) markTaskDoneLocked(taskID string) {
	if rec, ok := s.taskPaths[taskID]; ok {
		rec.done = true
		s.taskPaths[taskID] = rec
	}
}

// superviseTailersLocked reacts to a translated frame batch: a shell or
// workflow spawn starts a tailer (once per task id) AND records its path;
// a background-agent spawn only records its path (never tailed); and a
// task's completion notification releases its tailer — which does one
// final catch-up read before exiting — while KEEPING the recorded path so
// the final output stays pollable. Callers hold s.mu.
func (s *Session) superviseTailersLocked(frames []protocol.L2Frame) {
	root := ClaudeConfigDir(s.configDir)
	for _, f := range frames {
		if ann := parseSpawnAnnouncement(f, root); ann != nil {
			s.recordTaskPathLocked(ann.TaskID, ann.Path)
			s.startTailerLocked(*ann)
		} else if ann := parseAgentSpawn(f); ann != nil {
			s.recordTaskPathLocked(ann.TaskID, ann.Path)
		}
		if n, ok := f.(*protocol.TaskNotificationFrame); ok && n.TaskID != "" {
			if stop, ok := s.tailers[n.TaskID]; ok {
				delete(s.tailers, n.TaskID)
				close(stop)
			}
			// The completion notification is the fallback path source for a
			// background agent whose spawn result named none, and it marks
			// the task done for the poll route.
			if n.OutputFile != "" && allowedTaskOutputPath(n.OutputFile) {
				s.recordTaskPathLocked(n.TaskID, filepath.Clean(n.OutputFile))
			}
			s.markTaskDoneLocked(n.TaskID)
		}
	}
}

// TaskOutput serves a bounded tail of a detached task's output file for
// the poll route (§ watcher-bubble expansion), reading ONLY a path this
// session recorded for itself and re-validating the confinement
// predicates — never a client-supplied path. It returns the appended text
// from OFFSET, the next cursor, whether the task has completed, and a live
// elapsed. ok is false when the session recorded no such task id.
func (s *Session) TaskOutput(taskID string, offset int64) (text string, next int64, done bool, elapsedMs int64, ok bool) {
	s.mu.Lock()
	rec, present := s.taskPaths[taskID]
	now := s.now()
	s.mu.Unlock()
	if !present {
		return "", offset, false, 0, false
	}
	elapsedMs = now.Sub(rec.spawnedAt).Milliseconds()
	// Belt-and-suspenders on the security boundary: only validated paths
	// are ever recorded, so a failure here is a bug, but a file served over
	// HTTP fails closed rather than trusting the record.
	if !allowedTaskOutputPath(rec.path) && !allowedJournalPath(rec.path, ClaudeConfigDir(s.configDir)) {
		return "", offset, rec.done, elapsedMs, false
	}
	text, next = readTailChunk(rec.path, offset)
	return text, next, rec.done, elapsedMs, true
}

func (s *Session) startTailerLocked(ann spawnAnnouncement) {
	if _, ok := s.tailers[ann.TaskID]; ok {
		return
	}
	stop := make(chan struct{})
	s.tailers[ann.TaskID] = stop
	go s.tailTaskOutput(ann, stop)
}

// tailTaskOutput streams PATH's growth until the task's notification
// lands (STOP), the output budget runs out, or the session ends.
func (s *Session) tailTaskOutput(ann spawnAnnouncement, stop <-chan struct{}) {
	ticker := time.NewTicker(s.tailInterval)
	defer ticker.Stop()
	var offset, total int64
	emit := func() bool {
		text, next := readTailChunk(ann.Path, offset)
		if text == "" {
			return true
		}
		offset = next
		total += int64(len(text))
		frames := []protocol.L2Frame{&protocol.TaskOutputDeltaFrame{
			Envelope:  protocol.Envelope{Type: "task-output-delta"},
			TaskID:    ann.TaskID,
			ToolUseID: ann.ToolUseID,
			Text:      text,
		}}
		if total >= taskTailTotalMax {
			frames = append(frames, &protocol.TaskOutputDeltaFrame{
				Envelope:  protocol.Envelope{Type: "task-output-delta"},
				TaskID:    ann.TaskID,
				ToolUseID: ann.ToolUseID,
				Text:      "\n… output stream capped (64KB); the rest stays in " + ann.Path + "\n",
			})
		}
		s.mu.Lock()
		s.broadcastLocked(frames)
		s.mu.Unlock()
		return total < taskTailTotalMax
	}
	for {
		select {
		case <-stop:
			// The completion notification already landed; one catch-up
			// read collects whatever the task wrote after the last tick.
			emit()
			return
		case <-s.done:
			return
		case <-ticker.C:
			if !emit() {
				return
			}
		}
	}
}

// readTailChunk reads up to taskTailChunkMax appended bytes from PATH
// at OFFSET, trimming a trailing incomplete UTF-8 rune back into the
// next read so multi-byte characters never split across frames. An
// unreadable file yields no text and no offset movement — the file may
// simply not exist yet on the first ticks.
func readTailChunk(path string, offset int64) (string, int64) {
	f, err := os.Open(path)
	if err != nil {
		return "", offset
	}
	defer f.Close()
	if _, err := f.Seek(offset, io.SeekStart); err != nil {
		return "", offset
	}
	buf := make([]byte, taskTailChunkMax)
	n, _ := f.Read(buf)
	if n <= 0 {
		return "", offset
	}
	// Back a split rune out of the chunk (at most 3 bytes); genuinely
	// non-UTF-8 content passes through sanitized instead of truncated.
	trimmed := n
	for trimmed > 0 && n-trimmed < utf8.UTFMax-1 && !utf8.Valid(buf[:trimmed]) {
		trimmed--
	}
	if trimmed == 0 || !utf8.Valid(buf[:trimmed]) {
		return strings.ToValidUTF8(string(buf[:n]), "�"), offset + int64(n)
	}
	return string(buf[:trimmed]), offset + int64(trimmed)
}
