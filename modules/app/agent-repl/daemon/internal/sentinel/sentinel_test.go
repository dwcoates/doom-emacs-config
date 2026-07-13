package sentinel

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func newTestWriter(t *testing.T) (*Writer, string) {
	t.Helper()
	dir := t.TempDir()
	t.Setenv("AGENT_REPL_STATE_DIR", dir)
	w, err := NewWriter(t.Logf)
	if err != nil {
		t.Fatalf("NewWriter: %v", err)
	}
	return w, filepath.Join(dir, "workspace-notifications")
}

func readDir(t *testing.T, dir string) []string {
	t.Helper()
	entries, err := os.ReadDir(dir)
	if err != nil {
		t.Fatalf("ReadDir(%s): %v", dir, err)
	}
	names := make([]string, 0, len(entries))
	for _, e := range entries {
		names = append(names, e.Name())
	}
	return names
}

func TestDirResolution(t *testing.T) {
	// Arrange / Act / Assert — table over env configurations.
	tests := []struct {
		name    string
		env     string
		wantSub string
	}{
		{name: "env override", env: "/tmp/some-state", wantSub: "/tmp/some-state/workspace-notifications"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Setenv("AGENT_REPL_STATE_DIR", tt.env)
			got, err := Dir()
			if err != nil {
				t.Fatalf("Dir: %v", err)
			}
			if got != tt.wantSub {
				t.Fatalf("Dir = %q, want %q", got, tt.wantSub)
			}
		})
	}
}

func TestDirDefaultsToHomeClaudeEmacs(t *testing.T) {
	// Arrange
	t.Setenv("AGENT_REPL_STATE_DIR", "")
	home := t.TempDir()
	t.Setenv("HOME", home)
	// Act
	got, err := Dir()
	// Assert
	if err != nil {
		t.Fatalf("Dir: %v", err)
	}
	want := filepath.Join(home, ".claude-emacs", "workspace-notifications")
	if got != want {
		t.Fatalf("Dir = %q, want %q", got, want)
	}
}

func TestWriteFilenamesAndContent(t *testing.T) {
	// Arrange — table over the three sentinel kinds.
	tests := []struct {
		name     string
		write    func(w *Writer)
		wantName string
	}{
		{
			name:     "permission requested",
			write:    func(w *Writer) { w.PermissionRequested("/repo", "sid1", "req42") },
			wantName: "permission_request_sid1_req42",
		},
		{
			name:     "permission resolved",
			write:    func(w *Writer) { w.PermissionResolved("/repo", "sid1", "req42") },
			wantName: "permission_resolved_sid1_req42",
		},
		{
			name:     "session dead",
			write:    func(w *Writer) { w.SessionDead("/repo", "sid1") },
			wantName: "session_dead_sid1",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			w, dir := newTestWriter(t)
			// Act
			tt.write(w)
			w.Close() // drains the queue
			// Assert
			data, err := os.ReadFile(filepath.Join(dir, tt.wantName))
			if err != nil {
				t.Fatalf("expected sentinel %s: %v (dir has %v)", tt.wantName, err, readDir(t, dir))
			}
			// Line 3 is the ownership marker: daemon-driven sessions are
			// module-owned by definition, so Emacs accepts their session ids.
			if got, want := string(data), "/repo\nsid1\nowned\n"; got != want {
				t.Fatalf("content = %q, want %q", got, want)
			}
		})
	}
}

func TestWriteEmptySidStillPrefixMatched(t *testing.T) {
	// Arrange — pre-init sessions have no claude session id yet.
	w, dir := newTestWriter(t)
	// Act
	w.PermissionRequested("/repo", "", "req1")
	w.Close()
	// Assert — the filename must still start with the dispatch prefix.
	names := readDir(t, dir)
	if len(names) != 1 || !strings.HasPrefix(names[0], "permission_request") {
		t.Fatalf("expected one permission_request* file, got %v", names)
	}
	data, err := os.ReadFile(filepath.Join(dir, names[0]))
	if err != nil {
		t.Fatalf("read: %v", err)
	}
	if got, want := string(data), "/repo\n\nowned\n"; got != want {
		t.Fatalf("content = %q, want %q", got, want)
	}
}

func TestNoTempFilesLeftBehind(t *testing.T) {
	// Arrange
	w, dir := newTestWriter(t)
	// Act
	w.PermissionRequested("/repo", "sid", "r1")
	w.PermissionResolved("/repo", "sid", "r1")
	w.SessionDead("/repo", "sid")
	w.Close()
	// Assert — direct-write strategy: only final names, nothing hidden.
	for _, name := range readDir(t, dir) {
		if strings.HasPrefix(name, ".") || strings.HasSuffix(name, ".tmp") {
			t.Fatalf("unexpected temp artifact %q", name)
		}
	}
}

func TestWriteSelfHealsWipedDir(t *testing.T) {
	// Arrange — wipe the notifications dir after writer construction.
	w, dir := newTestWriter(t)
	w.PermissionRequested("/repo", "sid", "r1")
	w.Close()
	if err := os.RemoveAll(dir); err != nil {
		t.Fatalf("RemoveAll: %v", err)
	}
	// Act — a fresh writer instance writes into the recreated dir.
	w2, err := NewWriter(t.Logf)
	if err != nil {
		t.Fatalf("NewWriter: %v", err)
	}
	w2.SessionDead("/repo", "sid")
	w2.Close()
	// Assert
	if _, err := os.Stat(filepath.Join(dir, "session_dead_sid")); err != nil {
		t.Fatalf("expected self-healed write: %v", err)
	}
}

// The sentinel channel no longer carries a login request. The login runs on
// a pty the daemon owns and is rendered by the webapp, so there is nothing
// left to ask Emacs to do — see internal/login.
