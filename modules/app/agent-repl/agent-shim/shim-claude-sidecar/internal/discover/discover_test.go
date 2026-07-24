package discover

import (
	"os"
	"path/filepath"
	"testing"
	"time"

	"agentrepl/shim-claude-sidecar/internal/tail"
)

// mkfile creates path (and parents) with some content.
func mkfile(t *testing.T, path string) {
	t.Helper()
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	if err := os.WriteFile(path, []byte("{}\n"), 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
}

func TestClassifySessionTranscript(t *testing.T) {
	// Arrange
	root := t.TempDir()
	p := filepath.Join(root, "projects", "-proj", "sess-abc.jsonl")
	d := New([]string{root}, "/tmp", nil)
	// Act
	got, ok := d.Classify(p)
	// Assert
	if !ok || got.Kind != tail.KindSessionTranscript {
		t.Fatalf("classify = %+v ok=%v, want session transcript", got, ok)
	}
	if got.SessionID != "sess-abc" || got.Raw {
		t.Fatalf("session = %q raw=%v", got.SessionID, got.Raw)
	}
}

func TestClassifyAgentSidechain(t *testing.T) {
	// Arrange
	root := t.TempDir()
	p := filepath.Join(root, "projects", "-proj", "S1", "subagents", "agent-abc123.jsonl")
	d := New([]string{root}, "/tmp", nil)
	// Act
	got, ok := d.Classify(p)
	// Assert: session from the PATH segment; task id from the filename; meta companion.
	if !ok || got.Kind != tail.KindAgentTranscript {
		t.Fatalf("classify = %+v ok=%v", got, ok)
	}
	if got.SessionID != "S1" || got.TaskID != "abc123" {
		t.Fatalf("session=%q task=%q", got.SessionID, got.TaskID)
	}
	if !filepath.IsAbs(got.MetaPath) || filepath.Base(got.MetaPath) != "agent-abc123.meta.json" {
		t.Fatalf("meta path = %q", got.MetaPath)
	}
}

func TestClassifyWorkflowJournal(t *testing.T) {
	// Arrange
	root := t.TempDir()
	p := filepath.Join(root, "projects", "-proj", "S2", "subagents", "workflows", "wf_deadbeef", "journal.jsonl")
	d := New([]string{root}, "/tmp", nil)
	// Act
	got, ok := d.Classify(p)
	// Assert
	if !ok || got.Kind != tail.KindWorkflowJournal {
		t.Fatalf("classify = %+v ok=%v", got, ok)
	}
	if got.SessionID != "S2" || got.RunID != "wf_deadbeef" {
		t.Fatalf("session=%q run=%q", got.SessionID, got.RunID)
	}
}

func TestClassifySpoolKinds(t *testing.T) {
	// Arrange
	spool := t.TempDir()
	d := New(nil, spool, nil)
	cases := []struct {
		name   string
		file   string
		want   tail.Kind
		raw    bool
	}{
		{"agent", "a1234.output", tail.KindAgentTranscript, false},
		{"shell", "b5678.output", tail.KindShellSpool, true},
		{"workflow", "w9012.output", tail.KindWorkflowJournal, false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			p := filepath.Join(spool, "claude-501", "the-slug", "SESS", "tasks", tc.file)
			// Act
			got, ok := d.Classify(p)
			// Assert: kind by a/b/w prefix; session from the PATH; spool dir set.
			if !ok || got.Kind != tc.want || got.Raw != tc.raw {
				t.Fatalf("%s: got %+v ok=%v", tc.name, got, ok)
			}
			if got.SessionID != "SESS" {
				t.Fatalf("%s: session = %q, want SESS", tc.name, got.SessionID)
			}
			if got.SpoolDir != filepath.Dir(p) {
				t.Fatalf("%s: spool dir = %q", tc.name, got.SpoolDir)
			}
		})
	}
}

func TestClassifyRejectsUnknownSpoolPrefix(t *testing.T) {
	// Arrange: a task id whose first char is not a/b/w.
	spool := t.TempDir()
	d := New(nil, spool, nil)
	p := filepath.Join(spool, "claude-501", "slug", "SESS", "tasks", "x999.output")
	// Act
	_, ok := d.Classify(p)
	// Assert
	if ok {
		t.Fatalf("expected classify to reject a non-a/b/w spool")
	}
}

func TestClassifyMetaJsonNotTailed(t *testing.T) {
	// Arrange: the sidechain meta.json companion is not a tail target.
	root := t.TempDir()
	p := filepath.Join(root, "projects", "-proj", "S1", "subagents", "agent-abc.meta.json")
	d := New([]string{root}, "/tmp", nil)
	// Act
	_, ok := d.Classify(p)
	// Assert
	if ok {
		t.Fatalf("meta.json should not classify as a tail target")
	}
}

func TestScanUnionsRootsAndSpools(t *testing.T) {
	// Arrange: a config root with a session + sidechain + journal, and a spool.
	root := t.TempDir()
	spool := t.TempDir()
	mkfile(t, filepath.Join(root, "projects", "-p", "S.jsonl"))
	mkfile(t, filepath.Join(root, "projects", "-p", "S", "subagents", "agent-a1.jsonl"))
	mkfile(t, filepath.Join(root, "projects", "-p", "S", "subagents", "workflows", "wf_x", "journal.jsonl"))
	mkfile(t, filepath.Join(spool, "claude-501", "sl", "S", "tasks", "b7.output"))
	d := New([]string{root}, spool, nil)
	// Act
	got := d.Scan()
	// Assert: all four shapes discovered.
	if len(got) != 4 {
		t.Fatalf("scan found %d targets, want 4: %+v", len(got), got)
	}
	kinds := map[tail.Kind]int{}
	for _, tg := range got {
		kinds[tg.Kind]++
	}
	if kinds[tail.KindSessionTranscript] != 1 || kinds[tail.KindShellSpool] != 1 || kinds[tail.KindWorkflowJournal] != 1 {
		t.Fatalf("kind distribution = %v", kinds)
	}
}

func TestWatcherReportsCreatedFile(t *testing.T) {
	// Arrange: watch an existing directory.
	dir := t.TempDir()
	w, err := NewWatcher([]string{dir}, nil)
	if err != nil {
		t.Fatalf("watcher: %v", err)
	}
	defer w.Close()
	// Act: create a file in the watched dir.
	if err := os.WriteFile(filepath.Join(dir, "new.jsonl"), []byte("x"), 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	// Assert: an event arrives (block on the channel; timeout is only a failsafe).
	select {
	case ev := <-w.Events():
		if filepath.Base(ev.Name) != "new.jsonl" {
			t.Fatalf("event for %q, want new.jsonl", ev.Name)
		}
	case err := <-w.Errors():
		t.Fatalf("watch error: %v", err)
	case <-time.After(5 * time.Second):
		t.Fatal("no fsnotify event within failsafe window")
	}
}
