package session

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"claude-repld/internal/protocol"
)

// --- confinement ---------------------------------------------------------------

func TestAllowedTaskOutputPathAcceptsTheSpool(t *testing.T) {
	// Arrange + Act + Assert
	if !allowedTaskOutputPath("/private/tmp/claude-501/slug/sess/tasks/bg1.output") {
		t.Fatal("spool path rejected")
	}
}

func TestAllowedTaskOutputPathAcceptsTheUnprefixedTmpTwin(t *testing.T) {
	// Arrange + Act + Assert
	if !allowedTaskOutputPath("/tmp/claude-501/slug/tasks/bg1.output") {
		t.Fatal("tmp twin rejected")
	}
}

func TestAllowedTaskOutputPathRejectsPathsOutsideTheSpool(t *testing.T) {
	// Arrange + Act + Assert
	if allowedTaskOutputPath("/etc/passwd") {
		t.Fatal("accepted a path outside the spool")
	}
}

func TestAllowedTaskOutputPathRejectsSpoolFilesOutsideTasks(t *testing.T) {
	// Arrange + Act + Assert — right tree, wrong directory.
	if allowedTaskOutputPath("/tmp/claude-501/slug/other/bg1.output") {
		t.Fatal("accepted a non-tasks file")
	}
}

func TestAllowedTaskOutputPathRejectsDotDotEscapes(t *testing.T) {
	// Arrange + Act + Assert — Clean resolves the escape out of the spool.
	if allowedTaskOutputPath("/tmp/claude-501/tasks/../../../etc/x.output") {
		t.Fatal("accepted a dot-dot escape")
	}
}

// --- announcement parsing --------------------------------------------------------

func announcementFrame(t *testing.T, text string, isErr bool) protocol.L2Frame {
	t.Helper()
	content, err := json.Marshal(text)
	if err != nil {
		t.Fatal(err)
	}
	return &protocol.ToolUseResultFrame{
		Envelope:  protocol.Envelope{Type: "tool-use-result"},
		ToolUseID: "t1",
		IsError:   isErr,
		Content:   content,
	}
}

func TestParseSpawnAnnouncementExtractsIDAndPath(t *testing.T) {
	// Arrange
	frame := announcementFrame(t,
		"Command running in background with ID: bg7. Output is being written to: /tmp/claude-501/s/tasks/bg7.output. You will be notified.", false)
	// Act
	ann := parseSpawnAnnouncement(frame, "/cfg")
	// Assert
	if ann == nil || ann.TaskID != "bg7" || ann.Path != "/tmp/claude-501/s/tasks/bg7.output" || ann.ToolUseID != "t1" {
		t.Fatalf("ann = %+v", ann)
	}
}

func TestParseSpawnAnnouncementIgnoresErrorResults(t *testing.T) {
	// Arrange
	frame := announcementFrame(t,
		"with ID: bg7. Output is being written to: /tmp/claude-501/s/tasks/bg7.output", true)
	// Act + Assert
	if parseSpawnAnnouncement(frame, "/cfg") != nil {
		t.Fatal("parsed an announcement from an error result")
	}
}

func TestParseSpawnAnnouncementRefusesPathsOutsideTheSpool(t *testing.T) {
	// Arrange — a result that names a file the tailer must not read.
	frame := announcementFrame(t,
		"with ID: bg7. Output is being written to: /Users/x/secret.output", false)
	// Act + Assert
	if parseSpawnAnnouncement(frame, "/cfg") != nil {
		t.Fatal("accepted a path outside the spool")
	}
}

func TestParseSpawnAnnouncementExtractsWorkflowJournal(t *testing.T) {
	// Arrange — the observed Workflow spawn shape: a Task ID line plus
	// the run's transcript dir, whose journal.jsonl is the progress stream.
	frame := announcementFrame(t,
		"Workflow launched in background. Task ID: wj7025gze\nSummary: s\nTranscript dir: /cfg/projects/slug/uuid/subagents/workflows/wf_bd77b668-e04\nScript file: /cfg/projects/slug/uuid/workflows/scripts/x.js", false)
	// Act
	ann := parseSpawnAnnouncement(frame, "/cfg")
	// Assert
	if ann == nil || ann.TaskID != "wj7025gze" {
		t.Fatalf("ann = %+v", ann)
	}
	if ann.Path != "/cfg/projects/slug/uuid/subagents/workflows/wf_bd77b668-e04/journal.jsonl" {
		t.Fatalf("path = %q", ann.Path)
	}
}

func TestParseSpawnAnnouncementRefusesJournalOutsideTheConfigRoot(t *testing.T) {
	// Arrange — a transcript dir under someone ELSE's tree.
	frame := announcementFrame(t,
		"Task ID: wj1\nTranscript dir: /other/projects/slug/uuid/subagents/workflows/wf_1", false)
	// Act + Assert
	if parseSpawnAnnouncement(frame, "/cfg") != nil {
		t.Fatal("accepted a journal outside the config root")
	}
}

func TestParseSpawnAnnouncementRefusesNonWorkflowDirsInTheRoot(t *testing.T) {
	// Arrange — right root, but not a workflow transcript dir.
	frame := announcementFrame(t,
		"Task ID: wj1\nTranscript dir: /cfg/projects/slug/uuid/other", false)
	// Act + Assert
	if parseSpawnAnnouncement(frame, "/cfg") != nil {
		t.Fatal("accepted a non-workflow dir")
	}
}

func TestAllowedJournalPathRejectsDotDotEscapes(t *testing.T) {
	// Arrange + Act + Assert — Clean resolves the escape out of the root.
	if allowedJournalPath("/cfg/projects/x/subagents/workflows/wf/../../../../../../etc/journal.jsonl", "/cfg") {
		t.Fatal("accepted a dot-dot escape")
	}
}

// --- chunk reads -----------------------------------------------------------------

func TestReadTailChunkReadsAppendedBytesIncrementally(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "t.output")
	if err := os.WriteFile(path, []byte("hello "), 0o644); err != nil {
		t.Fatal(err)
	}
	// Act
	first, off := readTailChunk(path, 0)
	f, err := os.OpenFile(path, os.O_APPEND|os.O_WRONLY, 0o644)
	if err != nil {
		t.Fatal(err)
	}
	if _, err := f.WriteString("world"); err != nil {
		t.Fatal(err)
	}
	if err := f.Close(); err != nil {
		t.Fatal(err)
	}
	second, _ := readTailChunk(path, off)
	// Assert
	if first != "hello " || second != "world" {
		t.Fatalf("first = %q, second = %q", first, second)
	}
}

func TestReadTailChunkLeavesOffsetOnMissingFile(t *testing.T) {
	// Arrange + Act
	text, off := readTailChunk(filepath.Join(t.TempDir(), "absent.output"), 3)
	// Assert — the file may simply not exist yet on early ticks.
	if text != "" || off != 3 {
		t.Fatalf("text = %q, off = %d", text, off)
	}
}

func TestReadTailChunkNeverSplitsARune(t *testing.T) {
	// Arrange — fill to the chunk cap so a multi-byte rune straddles it.
	path := filepath.Join(t.TempDir(), "t.output")
	body := strings.Repeat("a", taskTailChunkMax-1) + "é" // 2-byte rune at the boundary
	if err := os.WriteFile(path, []byte(body), 0o644); err != nil {
		t.Fatal(err)
	}
	// Act
	first, off := readTailChunk(path, 0)
	rest, _ := readTailChunk(path, off)
	// Assert — the rune arrives whole in the second chunk.
	if strings.Contains(first, "�") || rest != "é" {
		t.Fatalf("first ends %q, rest = %q", first[len(first)-4:], rest)
	}
}

// --- session integration ------------------------------------------------------------

// spoolFile creates a writable file inside the ALLOWED spool tree and
// registers its cleanup. The confinement regex pins the /tmp/claude-<uid>
// prefix, so t.TempDir cannot be used here.
func spoolFile(t *testing.T, name string) string {
	t.Helper()
	dir := fmt.Sprintf("/tmp/claude-0/test-%s/tasks", strings.ReplaceAll(t.Name(), "/", "-"))
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Skipf("cannot create spool dir: %v", err)
	}
	t.Cleanup(func() { _ = os.RemoveAll(filepath.Dir(dir)) })
	path := filepath.Join(dir, name)
	if err := os.WriteFile(path, nil, 0o644); err != nil {
		t.Fatal(err)
	}
	return path
}

// nextFrameOfType reads frames from C until one of TYPE arrives,
// failing after a bounded wait.
func nextFrameOfType(t *testing.T, c *Client, frameType string) map[string]any {
	t.Helper()
	deadline := time.After(5 * time.Second)
	for {
		select {
		case raw, ok := <-c.Send:
			if !ok {
				t.Fatal("client channel closed")
			}
			var decoded map[string]any
			if err := json.Unmarshal(raw, &decoded); err != nil {
				t.Fatal(err)
			}
			if decoded["type"] == frameType {
				return decoded
			}
		case <-deadline:
			t.Fatalf("no %s frame arrived", frameType)
		}
	}
}

func TestSessionTailsAnnouncedTaskOutput(t *testing.T) {
	// Arrange — a session whose tool result announces a spool file.
	path := spoolFile(t, "bg9.output")
	if err := os.WriteFile(path, []byte("line one\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	shim := newFakeShim()
	s := New(Config{
		ID: "s1", Shim: shim,
		ModelReconcileInterval: -1,
		TaskTailInterval:       5 * time.Millisecond,
	})
	client := s.NewReplayClient()
	s.Attach(client)
	go s.Run()
	defer func() {
		shim.end()
		<-s.Done()
	}()
	// Act — announce the spawn through the normal event path.
	text := "Command running in background with ID: bg9. Output is being written to: " + path + "."
	content, _ := json.Marshal(text)
	shim.events <- &protocol.L1Event{Type: "tool-result", ToolUseID: "tu9", Content: content}
	// Assert — the file's contents stream back as a delta frame.
	frame := nextFrameOfType(t, client, "task-output-delta")
	if frame["task_id"] != "bg9" || frame["tool_use_id"] != "tu9" {
		t.Fatalf("frame = %v", frame)
	}
	if !strings.Contains(frame["text"].(string), "line one") {
		t.Fatalf("text = %q", frame["text"])
	}
}

func TestSessionTailerStopsAfterTheCompletionNotification(t *testing.T) {
	// Arrange — a tailed task whose notification lands.
	path := spoolFile(t, "bg10.output")
	if err := os.WriteFile(path, []byte("early\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	shim := newFakeShim()
	s := New(Config{
		ID: "s1", Shim: shim,
		ModelReconcileInterval: -1,
		TaskTailInterval:       5 * time.Millisecond,
	})
	client := s.NewReplayClient()
	s.Attach(client)
	go s.Run()
	defer func() {
		shim.end()
		<-s.Done()
	}()
	text := "with ID: bg10. Output is being written to: " + path + "."
	content, _ := json.Marshal(text)
	shim.events <- &protocol.L1Event{Type: "tool-result", ToolUseID: "tu10", Content: content}
	nextFrameOfType(t, client, "task-output-delta")
	// Act — the notification releases the tailer (with one final read).
	note, _ := json.Marshal(map[string]string{
		"text": "<task-notification><task-id>bg10</task-id></task-notification>",
	})
	shim.events <- &protocol.L1Event{Type: "system", Subtype: "task_notification", Data: note}
	nextFrameOfType(t, client, "task-notification")
	// Assert — the release channel left the supervision map.
	s.mu.Lock()
	_, live := s.tailers["bg10"]
	s.mu.Unlock()
	if live {
		t.Fatal("tailer still supervised after its notification")
	}
}

func TestParseAgentSpawnExtractsIDAndPath(t *testing.T) {
	// Arrange — a background agent's result names agentId and output_file.
	frame := announcementFrame(t, "Launched. agentId: a1 output_file: /tmp/claude-501/s/tasks/a1.output", false)
	// Act
	ann := parseAgentSpawn(frame)
	// Assert
	if ann == nil || ann.TaskID != "a1" || ann.Path != "/tmp/claude-501/s/tasks/a1.output" || ann.ToolUseID != "t1" {
		t.Fatalf("ann = %+v", ann)
	}
}

func TestParseAgentSpawnIgnoresErrorResults(t *testing.T) {
	// Arrange
	frame := announcementFrame(t, "agentId: a1 output_file: /tmp/claude-501/s/tasks/a1.output", true)
	// Act + Assert
	if parseAgentSpawn(frame) != nil {
		t.Fatal("an error result announces no tailable agent")
	}
}

func TestParseAgentSpawnRefusesPathsOutsideTheSpool(t *testing.T) {
	// Arrange
	frame := announcementFrame(t, "agentId: a1 output_file: /etc/passwd.output", false)
	// Act + Assert
	if parseAgentSpawn(frame) != nil {
		t.Fatal("a path outside the task spool must be refused")
	}
}

// awaitTaskOutput polls TaskOutput until the session has recorded TASKID,
// which lands a moment after the announcing event crosses the shim channel.
func awaitTaskOutput(t *testing.T, s *Session, taskID string) string {
	t.Helper()
	deadline := time.After(5 * time.Second)
	for {
		if text, _, _, _, ok := s.TaskOutput(taskID, 0); ok {
			return text
		}
		select {
		case <-deadline:
			t.Fatalf("task %s never recorded", taskID)
		case <-time.After(5 * time.Millisecond):
		}
	}
}

func TestSessionServesRecordedTaskOutput(t *testing.T) {
	// Arrange — a backgrounded shell whose spool file already has output.
	path := spoolFile(t, "bg11.output")
	if err := os.WriteFile(path, []byte("progress\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	shim := newFakeShim()
	s := New(Config{ID: "s1", Shim: shim, ModelReconcileInterval: -1, TaskTailInterval: 5 * time.Millisecond})
	s.Attach(s.NewReplayClient())
	go s.Run()
	defer func() { shim.end(); <-s.Done() }()
	// Act — announce the spawn, then poll the tail.
	content, _ := json.Marshal("with ID: bg11. Output is being written to: " + path + ".")
	shim.events <- &protocol.L1Event{Type: "tool-result", ToolUseID: "tu11", Content: content}
	text := awaitTaskOutput(t, s, "bg11")
	// Assert
	if !strings.Contains(text, "progress") {
		t.Fatalf("text = %q", text)
	}
}

func TestSessionTaskOutputStaysPollableAfterCompletion(t *testing.T) {
	// Arrange — a tailed task whose completion notification will land.
	path := spoolFile(t, "bg12.output")
	if err := os.WriteFile(path, []byte("final output\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	shim := newFakeShim()
	s := New(Config{ID: "s1", Shim: shim, ModelReconcileInterval: -1, TaskTailInterval: 5 * time.Millisecond})
	s.Attach(s.NewReplayClient())
	go s.Run()
	defer func() { shim.end(); <-s.Done() }()
	content, _ := json.Marshal("with ID: bg12. Output is being written to: " + path + ".")
	shim.events <- &protocol.L1Event{Type: "tool-result", ToolUseID: "tu12", Content: content}
	awaitTaskOutput(t, s, "bg12")
	// Act — the notification settles the task; the path is kept, not dropped.
	note, _ := json.Marshal(map[string]string{
		"text": "<task-notification><task-id>bg12</task-id></task-notification>",
	})
	shim.events <- &protocol.L1Event{Type: "system", Subtype: "task_notification", Data: note}
	// Assert — TaskOutput still serves the final output and reports done.
	deadline := time.After(5 * time.Second)
	for {
		text, _, done, _, ok := s.TaskOutput("bg12", 0)
		if ok && done {
			if !strings.Contains(text, "final output") {
				t.Fatalf("text = %q", text)
			}
			return
		}
		select {
		case <-deadline:
			t.Fatal("task never reported done")
		case <-time.After(5 * time.Millisecond):
		}
	}
}

func TestSessionRecordsAgentPathWithoutTailing(t *testing.T) {
	// Arrange — a background agent's verbose transcript is polled, never streamed.
	path := spoolFile(t, "a2.output")
	if err := os.WriteFile(path, []byte("agent transcript\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	shim := newFakeShim()
	s := New(Config{ID: "s1", Shim: shim, ModelReconcileInterval: -1, TaskTailInterval: 5 * time.Millisecond})
	s.Attach(s.NewReplayClient())
	go s.Run()
	defer func() { shim.end(); <-s.Done() }()
	// Act
	content, _ := json.Marshal("agentId: a2 output_file: " + path)
	shim.events <- &protocol.L1Event{Type: "tool-result", ToolUseID: "tua2", Content: content}
	text := awaitTaskOutput(t, s, "a2")
	// Assert — recorded and pollable, but no live tailer was started for it.
	if !strings.Contains(text, "agent transcript") {
		t.Fatalf("text = %q", text)
	}
	s.mu.Lock()
	_, tailed := s.tailers["a2"]
	s.mu.Unlock()
	if tailed {
		t.Fatal("a background agent should be recorded, not tailed")
	}
}

func TestSessionTaskOutputUnknownIDIsNotOk(t *testing.T) {
	// Arrange — a session that recorded no tasks.
	s := New(Config{ID: "s1", Shim: newFakeShim(), ModelReconcileInterval: -1})
	// Act + Assert
	if _, _, _, _, ok := s.TaskOutput("nope", 0); ok {
		t.Fatal("an unrecorded task id must not resolve")
	}
}

// --- record-on-read (grandchild spawns in served chunks) -----------------------

// jsonlLine encodes one transcript entry whose content is TEXT, matching
// how an announcement is embedded in a real subagent transcript.
func jsonlLine(text string) string {
	content, _ := json.Marshal(text)
	return `{"type":"user","message":{"content":` + string(content) + `}}` + "\n"
}

func TestScanChunkSpawnsExtractsAShellSpawnFromJSONL(t *testing.T) {
	// Arrange — the announcement rides inside a JSON-encoded string.
	chunk := jsonlLine("Command running in background with ID: bg7. Output is being written to: /tmp/claude-501/s/tasks/bg7.output\nMore text.")
	// Act
	anns := scanChunkSpawns(chunk, "/cfg")
	// Assert
	if len(anns) != 1 || anns[0].TaskID != "bg7" || anns[0].Path != "/tmp/claude-501/s/tasks/bg7.output" {
		t.Fatalf("anns = %+v", anns)
	}
}

func TestScanChunkSpawnsExtractsAnAgentSpawn(t *testing.T) {
	// Arrange
	chunk := jsonlLine("Launched. agentId: a7 output_file: /tmp/claude-501/s/tasks/a7.output")
	// Act
	anns := scanChunkSpawns(chunk, "/cfg")
	// Assert
	if len(anns) != 1 || anns[0].TaskID != "a7" || anns[0].Path != "/tmp/claude-501/s/tasks/a7.output" {
		t.Fatalf("anns = %+v", anns)
	}
}

func TestScanChunkSpawnsExtractsAWorkflowJournal(t *testing.T) {
	// Arrange
	chunk := jsonlLine("Workflow started. Task ID: wj7 Transcript dir: /cfg/projects/p/subagents/workflows/wf1")
	// Act
	anns := scanChunkSpawns(chunk, "/cfg")
	// Assert
	if len(anns) != 1 || anns[0].TaskID != "wj7" || anns[0].Path != "/cfg/projects/p/subagents/workflows/wf1/journal.jsonl" {
		t.Fatalf("anns = %+v", anns)
	}
}

func TestScanChunkSpawnsRefusesPathsOutsideTheSpool(t *testing.T) {
	// Arrange
	chunk := jsonlLine("with ID: bg7. Output is being written to: /etc/evil.output")
	// Act + Assert
	if anns := scanChunkSpawns(chunk, "/cfg"); len(anns) != 0 {
		t.Fatalf("outside-spool path recorded: %+v", anns)
	}
}

func TestScanChunkSpawnsNeverPairsAcrossJSONStrings(t *testing.T) {
	// Arrange — the id and the path live in DIFFERENT transcript strings.
	chunk := jsonlLine("with ID: bg7. And nothing else.") +
		jsonlLine("Output is being written to: /tmp/claude-501/s/tasks/bg7.output")
	// Act + Assert
	if anns := scanChunkSpawns(chunk, "/cfg"); len(anns) != 0 {
		t.Fatalf("cross-string pairing: %+v", anns)
	}
}

func TestTaskOutputRecordsGrandchildSpawnsOnRead(t *testing.T) {
	// Arrange — a parent transcript announcing a grandchild's spool file.
	parent := spoolFile(t, "a9.output")
	child := spoolFile(t, "bg7.output")
	if err := os.WriteFile(child, []byte("child says hi"), 0o644); err != nil {
		t.Fatal(err)
	}
	line := jsonlLine("Command running in background with ID: bg7. Output is being written to: " + child + ".")
	if err := os.WriteFile(parent, []byte(line), 0o644); err != nil {
		t.Fatal(err)
	}
	s := New(Config{ID: "s1", Shim: newFakeShim(), ModelReconcileInterval: -1})
	s.mu.Lock()
	s.recordTaskPathLocked("a9", parent)
	s.mu.Unlock()
	// Act — serving the parent's tail records the grandchild's path.
	if _, _, _, _, ok := s.TaskOutput("a9", 0); !ok {
		t.Fatal("parent tail not served")
	}
	// Assert — the grandchild is pollable in turn.
	text, _, _, _, ok := s.TaskOutput("bg7", 0)
	if !ok || text != "child says hi" {
		t.Fatalf("grandchild tail = %q ok=%v", text, ok)
	}
}

func TestTaskOutputJoinsAnAnnouncementSplitAcrossChunks(t *testing.T) {
	// Arrange — the announcement straddles the 8KB chunk boundary.
	parent := spoolFile(t, "a10.output")
	child := spoolFile(t, "bg8.output")
	ann := "with ID: bg8. Output is being written to: " + child + "."
	pad := strings.Repeat("x", taskTailChunkMax-20)
	if err := os.WriteFile(parent, []byte(pad+ann), 0o644); err != nil {
		t.Fatal(err)
	}
	s := New(Config{ID: "s1", Shim: newFakeShim(), ModelReconcileInterval: -1})
	s.mu.Lock()
	s.recordTaskPathLocked("a10", parent)
	s.mu.Unlock()
	// Act — two reads: the carry re-joins the split announcement.
	_, next, _, _, ok := s.TaskOutput("a10", 0)
	if !ok {
		t.Fatal("first chunk not served")
	}
	if _, _, _, _, ok := s.TaskOutput("a10", next); !ok {
		t.Fatal("second chunk not served")
	}
	// Assert
	if _, _, _, _, ok := s.TaskOutput("bg8", 0); !ok {
		t.Fatal("split announcement never recorded the grandchild")
	}
}
