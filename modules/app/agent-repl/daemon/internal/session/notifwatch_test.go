package session

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// marshalNoEscape mirrors the CLI's JSON.stringify: literal `<`/`>`, not
// Go's < HTML escapes, so fixture lines match real transcript bytes.
func marshalNoEscape(t *testing.T, v any) string {
	t.Helper()
	var buf bytes.Buffer
	enc := json.NewEncoder(&buf)
	enc.SetEscapeHTML(false)
	if err := enc.Encode(v); err != nil {
		t.Fatal(err)
	}
	return strings.TrimSuffix(buf.String(), "\n")
}

// notifSession builds a session whose transcript lives under a temp config
// root, with the watch and reconciler tickers disabled so ticks are driven
// by hand.
func notifSession(t *testing.T) (*Session, string) {
	t.Helper()
	root := t.TempDir()
	s := New(Config{
		ID:                     "s1",
		Shim:                   newFakeShim(),
		ModelReconcileInterval: -1,
		NotifWatchInterval:     -1,
		ConfigDir:              root,
	})
	s.translator.ClaudeSessionID = "c1"
	s.translator.CWD = "/w"
	path := TranscriptPath(root, "/w", "c1")
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(path, nil, 0o644); err != nil {
		t.Fatal(err)
	}
	// The first tick tails the file; growth from here on is watched.
	s.notifWatchTick()
	return s, path
}

// wakeEntry is a transcript wake line for TASKID, in the string-content
// shape the CLI writes (uuid u<taskID> unless overridden).
func wakeEntry(t *testing.T, taskID, uuid string) string {
	t.Helper()
	text := "<task-notification>\n<task-id>" + taskID + "</task-id>\n<tool-use-id>t1</tool-use-id>\n<status>completed</status>\n</task-notification>"
	return marshalNoEscape(t, map[string]any{
		"type":    "user",
		"uuid":    uuid,
		"message": map[string]any{"role": "user", "content": text},
	}) + "\n"
}

func appendFile(t *testing.T, path, data string) {
	t.Helper()
	f, err := os.OpenFile(path, os.O_APPEND|os.O_WRONLY, 0o644)
	if err != nil {
		t.Fatal(err)
	}
	defer f.Close()
	if _, err := f.WriteString(data); err != nil {
		t.Fatal(err)
	}
}

func TestNotifWatchSettlesATaskFromAnAppendedWakeEntry(t *testing.T) {
	// Arrange — an unsettled recorded task, then the wake lands on disk only.
	s, path := notifSession(t)
	s.mu.Lock()
	s.recordTaskPathLocked("bg1", "/tmp/claude-0/x/tasks/bg1.output")
	s.mu.Unlock()
	appendFile(t, path, wakeEntry(t, "bg1", "u1"))
	// Act
	s.notifWatchTick()
	// Assert
	s.mu.Lock()
	rec := s.taskPaths["bg1"]
	s.mu.Unlock()
	if !rec.done {
		t.Fatal("wake entry on disk did not settle the task")
	}
}

func TestNotifWatchReadsABlockContentEntryToo(t *testing.T) {
	// Arrange — the notification text as a content block array.
	s, path := notifSession(t)
	s.mu.Lock()
	s.recordTaskPathLocked("bg2", "/tmp/claude-0/x/tasks/bg2.output")
	s.mu.Unlock()
	text := "<task-notification>\n<task-id>bg2</task-id>\n<status>completed</status>\n</task-notification>"
	line := marshalNoEscape(t, map[string]any{
		"type": "user",
		"uuid": "u2",
		"message": map[string]any{
			"role":    "user",
			"content": []map[string]any{{"type": "text", "text": text}},
		},
	})
	appendFile(t, path, line+"\n")
	// Act
	s.notifWatchTick()
	// Assert
	s.mu.Lock()
	rec := s.taskPaths["bg2"]
	s.mu.Unlock()
	if !rec.done {
		t.Fatal("block-content wake entry did not settle the task")
	}
}

func TestNotifWatchStartsAtTheTailAndSkipsHistory(t *testing.T) {
	// Arrange — the wake is ALREADY in the transcript when the watch arms;
	// history is the replay path's business.
	root := t.TempDir()
	s := New(Config{
		ID:                     "s1",
		Shim:                   newFakeShim(),
		ModelReconcileInterval: -1,
		NotifWatchInterval:     -1,
		ConfigDir:              root,
	})
	s.translator.ClaudeSessionID = "c1"
	s.translator.CWD = "/w"
	path := TranscriptPath(root, "/w", "c1")
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(path, []byte(wakeEntry(t, "bg3", "u3")), 0o644); err != nil {
		t.Fatal(err)
	}
	s.mu.Lock()
	s.recordTaskPathLocked("bg3", "/tmp/claude-0/x/tasks/bg3.output")
	s.mu.Unlock()
	// Act — first tick arms at EOF, second reads nothing new.
	s.notifWatchTick()
	s.notifWatchTick()
	// Assert
	s.mu.Lock()
	rec := s.taskPaths["bg3"]
	s.mu.Unlock()
	if rec.done {
		t.Fatal("pre-existing history entry must not be re-emitted by the watch")
	}
}

func TestNotifWatchEmitsADuplicatedUuidOnce(t *testing.T) {
	// Arrange — the same entry appended twice (a rewritten tail, a resume
	// artifact); the uuid dedupe keeps the second silent.
	s, path := notifSession(t)
	s.mu.Lock()
	s.recordTaskPathLocked("bg4", "/tmp/claude-0/x/tasks/bg4.output")
	s.mu.Unlock()
	appendFile(t, path, wakeEntry(t, "bg4", "u4"))
	s.notifWatchTick()
	s.mu.Lock()
	s.taskPaths["bg4"] = taskPathRec{path: s.taskPaths["bg4"].path}
	s.mu.Unlock()
	appendFile(t, path, wakeEntry(t, "bg4", "u4"))
	// Act
	s.notifWatchTick()
	// Assert — the un-settle above stays: the duplicate uuid was skipped.
	s.mu.Lock()
	rec := s.taskPaths["bg4"]
	s.mu.Unlock()
	if rec.done {
		t.Fatal("a duplicated uuid must emit only once")
	}
}

func TestNotifWatchSkipsATaskTheStreamAlreadySettled(t *testing.T) {
	// Arrange — mid-turn delivery already settled the task; the transcript
	// entry that mirrors it must not double the frames.
	s, path := notifSession(t)
	s.mu.Lock()
	s.recordTaskPathLocked("bg5", "/tmp/claude-0/x/tasks/bg5.output")
	s.markTaskDoneLocked("bg5")
	s.mu.Unlock()
	appendFile(t, path, wakeEntry(t, "bg5", "u5"))
	// Act
	s.notifWatchTick()
	// Assert — skipped, but the uuid is still consumed.
	s.mu.Lock()
	_, seen := s.notifSeen["u5"]
	s.mu.Unlock()
	if !seen {
		t.Fatal("an already-settled task's entry should still be marked seen")
	}
}

func TestNotifWatchJoinsAWakeEntrySplitAcrossTicks(t *testing.T) {
	// Arrange — the wake line lands in two partial writes.
	s, path := notifSession(t)
	s.mu.Lock()
	s.recordTaskPathLocked("bg6", "/tmp/claude-0/x/tasks/bg6.output")
	s.mu.Unlock()
	line := wakeEntry(t, "bg6", "u6")
	cut := len(line) / 2
	appendFile(t, path, line[:cut])
	s.notifWatchTick()
	appendFile(t, path, line[cut:])
	// Act
	s.notifWatchTick()
	// Assert
	s.mu.Lock()
	rec := s.taskPaths["bg6"]
	s.mu.Unlock()
	if !rec.done {
		t.Fatal("a wake entry split across ticks was not re-joined")
	}
}

func TestNotifWatchIgnoresANonUserLineCarryingTheMarker(t *testing.T) {
	// Arrange — an assistant entry QUOTING the marker (a debugging session
	// about notifications) must never settle anything.
	s, path := notifSession(t)
	s.mu.Lock()
	s.recordTaskPathLocked("bg7", "/tmp/claude-0/x/tasks/bg7.output")
	s.mu.Unlock()
	line := marshalNoEscape(t, map[string]any{
		"type": "assistant",
		"uuid": "u7",
		"message": map[string]any{
			"role":    "assistant",
			"content": "<task-notification>\n<task-id>bg7</task-id>\n</task-notification>",
		},
	})
	appendFile(t, path, line+"\n")
	// Act
	s.notifWatchTick()
	// Assert
	s.mu.Lock()
	rec := s.taskPaths["bg7"]
	s.mu.Unlock()
	if rec.done {
		t.Fatal("an assistant entry quoting the marker must not settle a task")
	}
}

func TestNotifWatchReTailsOnAClaudeSessionChange(t *testing.T) {
	// Arrange — a /clear mints a new claude session id mid-life; the watch
	// must re-tail the NEW file rather than replay its head.
	s, _ := notifSession(t)
	s.mu.Lock()
	s.translator.ClaudeSessionID = "c2"
	s.mu.Unlock()
	newPath := TranscriptPath(ClaudeConfigDir(s.configDir), "/w", "c2")
	if err := os.MkdirAll(filepath.Dir(newPath), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(newPath, []byte(wakeEntry(t, "bg8", "u8")), 0o644); err != nil {
		t.Fatal(err)
	}
	s.mu.Lock()
	s.recordTaskPathLocked("bg8", "/tmp/claude-0/x/tasks/bg8.output")
	s.mu.Unlock()
	// Act — first tick re-tails at the new file's EOF, second reads nothing.
	s.notifWatchTick()
	s.notifWatchTick()
	// Assert
	s.mu.Lock()
	rec := s.taskPaths["bg8"]
	s.mu.Unlock()
	if rec.done {
		t.Fatal("the new transcript's pre-existing head must not be replayed")
	}
}

func TestNotifWatchDropsAnOversizedCarryAndResyncs(t *testing.T) {
	// Arrange — a mega-line (a huge pasted tool result) exceeds the carry
	// cap mid-line; the watch drops it and recovers on the next real line.
	s, path := notifSession(t)
	s.mu.Lock()
	s.recordTaskPathLocked("bg9", "/tmp/claude-0/x/tasks/bg9.output")
	s.mu.Unlock()
	appendFile(t, path, fmt.Sprintf(`{"type":"assistant","message":{"content":"%s`, strings.Repeat("x", notifCarryMax+1)))
	s.notifWatchTick()
	appendFile(t, path, "\"}}\n"+wakeEntry(t, "bg9", "u9"))
	// Act
	s.notifWatchTick()
	// Assert — the mega-line is gone, the wake after it still lands.
	s.mu.Lock()
	rec := s.taskPaths["bg9"]
	s.mu.Unlock()
	if !rec.done {
		t.Fatal("the watch failed to resync after dropping an oversized carry")
	}
}
