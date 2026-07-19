package workspacecmd

import (
	"encoding/json"
	"os"
	"path/filepath"
	"reflect"
	"regexp"
	"strings"
	"testing"
)

// watcherRE mirrors `agent-repl-workspace-commands-file-regexp' in
// worktree.el. A name this does not match is a name Emacs never drains.
var watcherRE = regexp.MustCompile(`^workspace_commands_.*\.json$`)

func TestEmitWritesAWatchableFile(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	cmds := []Entry{NewCreate("add-support-status", "/repo", "investigate /status")}

	// Act.
	path, err := Emit(dir, cmds)

	// Assert.
	if err != nil {
		t.Fatalf("Emit() error = %v, want nil", err)
	}
	if got := filepath.Base(path); !watcherRE.MatchString(got) {
		t.Errorf("Emit() wrote %q, which the Emacs watcher regexp does not match", got)
	}
}

func TestEmitWritesTheCreateEntryVerbatim(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	want := NewCreate("add-support-status", "/repo", "investigate /status")

	// Act.
	path, err := Emit(dir, []Entry{want})
	if err != nil {
		t.Fatalf("Emit() error = %v, want nil", err)
	}
	raw, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("ReadFile: %v", err)
	}
	var got []Create
	if err := json.Unmarshal(raw, &got); err != nil {
		t.Fatalf("Unmarshal: %v", err)
	}

	// Assert.
	if len(got) != 1 || got[0] != want {
		t.Errorf("Emit() wrote %+v, want [%+v]", got, want)
	}
}

func TestEmitWritesAMixedArrayVerbatim(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	cmds := []Entry{
		NewCreate("n", "/repo", "p"),
		NewSwitch("/some/project"),
		NewFold("repo-key", false),
	}

	// Act.
	path, err := Emit(dir, cmds)
	if err != nil {
		t.Fatalf("Emit() error = %v, want nil", err)
	}
	raw, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("ReadFile: %v", err)
	}
	var got []map[string]any
	if err := json.Unmarshal(raw, &got); err != nil {
		t.Fatalf("Unmarshal: %v", err)
	}

	// Assert.
	want := []map[string]any{
		{"type": "create", "name": "n", "git_root": "/repo", "prompt": "p"},
		{"type": "switch", "dir": "/some/project"},
		{"type": "fold", "repo_key": "repo-key", "folded": false},
	}
	if !reflect.DeepEqual(got, want) {
		t.Errorf("Emit() wrote %v, want %v", got, want)
	}
}

func TestEmitAcceptsAFoldedFalseEntry(t *testing.T) {
	// Arrange.
	dir := t.TempDir()

	// Act.
	// folded=false must be VALID (it means "unfold"); only an absent
	// folded is refused.
	_, err := Emit(dir, []Entry{NewFold("repo-key", false)})

	// Assert.
	if err != nil {
		t.Errorf("Emit() error = %v, want nil for folded=false", err)
	}
}

func TestEmitLeavesNoWatchableTempBehind(t *testing.T) {
	// Arrange.
	dir := t.TempDir()

	// Act.
	if _, err := Emit(dir, []Entry{NewCreate("n", "/repo", "p")}); err != nil {
		t.Fatalf("Emit() error = %v, want nil", err)
	}
	entries, err := os.ReadDir(dir)
	if err != nil {
		t.Fatalf("ReadDir: %v", err)
	}

	// Assert.
	var watchable []string
	for _, e := range entries {
		if watcherRE.MatchString(e.Name()) {
			watchable = append(watchable, e.Name())
		}
	}
	if len(watchable) != 1 {
		t.Errorf("Emit() left %d watchable files %v, want exactly 1", len(watchable), watchable)
	}
}

func TestEmitCreatesTheDirWhenAbsent(t *testing.T) {
	// Arrange.
	dir := filepath.Join(t.TempDir(), "output")

	// Act.
	_, err := Emit(dir, []Entry{NewCreate("n", "/repo", "p")})

	// Assert.
	if err != nil {
		t.Fatalf("Emit() error = %v, want nil", err)
	}
	if _, err := os.Stat(dir); err != nil {
		t.Errorf("Emit() did not create %s: %v", dir, err)
	}
}

func TestEmitRejectsInvalidCommands(t *testing.T) {
	tests := []struct {
		name    string
		cmds    []Entry
		wantErr string
	}{
		{
			name:    "empty slice",
			cmds:    nil,
			wantErr: "no commands to emit",
		},
		{
			name:    "missing name",
			cmds:    []Entry{NewCreate("", "/repo", "p")},
			wantErr: "name is required",
		},
		{
			name:    "missing git_root",
			cmds:    []Entry{NewCreate("n", "", "p")},
			wantErr: "git_root is required",
		},
		{
			name:    "missing prompt",
			cmds:    []Entry{NewCreate("n", "/repo", "")},
			wantErr: "prompt is required",
		},
		{
			name:    "wrong type tag on create",
			cmds:    []Entry{Create{Type: "prompt", Name: "n", GitRoot: "/repo", Prompt: "p"}},
			wantErr: `type must be "create"`,
		},
		{
			name:    "missing dir",
			cmds:    []Entry{NewSwitch("")},
			wantErr: "dir is required",
		},
		{
			name:    "wrong type tag on switch",
			cmds:    []Entry{Switch{Type: "create", Dir: "/repo"}},
			wantErr: `type must be "switch"`,
		},
		{
			name:    "missing repo_key",
			cmds:    []Entry{NewFold("", true)},
			wantErr: "repo_key is required",
		},
		{
			name:    "absent folded",
			cmds:    []Entry{Fold{Type: "fold", RepoKey: "repo-key"}},
			wantErr: "folded is required",
		},
		{
			name:    "wrong type tag on fold",
			cmds:    []Entry{Fold{Type: "switch", RepoKey: "repo-key"}},
			wantErr: `type must be "fold"`,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			dir := t.TempDir()

			// Act.
			_, err := Emit(dir, tc.cmds)

			// Assert.
			if err == nil {
				t.Fatalf("Emit() error = nil, want %q", tc.wantErr)
			}
			if !strings.Contains(err.Error(), tc.wantErr) {
				t.Errorf("Emit() error = %q, want it to contain %q", err, tc.wantErr)
			}
		})
	}
}

func TestEmitRejectingAnInvalidCommandWritesNothing(t *testing.T) {
	// Arrange.
	dir := t.TempDir()

	// Act.
	if _, err := Emit(dir, []Entry{NewCreate("n", "/repo", "")}); err == nil {
		t.Fatal("Emit() error = nil, want a validation error")
	}
	entries, err := os.ReadDir(dir)
	if err != nil {
		t.Fatalf("ReadDir: %v", err)
	}

	// Assert.
	if len(entries) != 0 {
		t.Errorf("Emit() wrote %d files on a rejected command, want 0", len(entries))
	}
}

func TestEmitMintsAUniqueNamePerCall(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	cmd := []Entry{NewCreate("n", "/repo", "p")}

	// Act.
	first, err := Emit(dir, cmd)
	if err != nil {
		t.Fatalf("first Emit() error = %v, want nil", err)
	}
	second, err := Emit(dir, cmd)
	if err != nil {
		t.Fatalf("second Emit() error = %v, want nil", err)
	}

	// Assert.
	if first == second {
		t.Errorf("Emit() reused path %q, want a distinct file per call", first)
	}
}

func TestDirIsUnderTheStateRoot(t *testing.T) {
	// Arrange.
	t.Setenv("AGENT_REPL_STATE_DIR", "/tmp/some-root")

	// Act.
	got, err := Dir()

	// Assert.
	if err != nil {
		t.Fatalf("Dir() error = %v, want nil", err)
	}
	if want := "/tmp/some-root/output"; got != want {
		t.Errorf("Dir() = %q, want %q", got, want)
	}
}
