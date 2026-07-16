package sidebaraction

import (
	"encoding/json"
	"os"
	"path/filepath"
	"reflect"
	"regexp"
	"strings"
	"testing"
)

// watcherRE mirrors the Emacs watcher's regexp in sidebar.el. A name
// this does not match is a name Emacs never drains.
var watcherRE = regexp.MustCompile(`^sidebar_action_.*\.json$`)

// testAction builds a fully-populated valid action.
func testAction(t *testing.T) Action {
	t.Helper()
	id, err := NewID()
	if err != nil {
		t.Fatalf("NewID() error = %v, want nil", err)
	}
	return Action{
		ID:        id,
		Action:    "send-prompt",
		Targets:   []string{"ws-a"},
		Args:      map[string]any{"prompt": "hi"},
		Confirmed: true,
		TS:        1752690000,
	}
}

func TestEmitWritesAWatchableFile(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	a := testAction(t)

	// Act.
	path, err := Emit(dir, a)

	// Assert.
	if err != nil {
		t.Fatalf("Emit() error = %v, want nil", err)
	}
	if got := filepath.Base(path); !watcherRE.MatchString(got) {
		t.Errorf("Emit() wrote %q, which the Emacs watcher regexp does not match", got)
	}
}

func TestEmitNamesTheFileAfterTheID(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	a := testAction(t)

	// Act.
	path, err := Emit(dir, a)

	// Assert.
	if err != nil {
		t.Fatalf("Emit() error = %v, want nil", err)
	}
	if want := filepath.Join(dir, "sidebar_action_"+a.ID+".json"); path != want {
		t.Errorf("Emit() = %q, want %q", path, want)
	}
}

func TestEmitWritesTheActionVerbatim(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	want := testAction(t)

	// Act — read the FINAL path back in full: a partial file under the
	// final name would fail the roundtrip.
	path, err := Emit(dir, want)
	if err != nil {
		t.Fatalf("Emit() error = %v, want nil", err)
	}
	raw, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("ReadFile: %v", err)
	}
	var got Action
	if err := json.Unmarshal(raw, &got); err != nil {
		t.Fatalf("Unmarshal %s: %v", raw, err)
	}

	// Assert.
	if !reflect.DeepEqual(got, want) {
		t.Errorf("Emit() wrote %+v, want %+v", got, want)
	}
}

func TestEmitSerializesNilTargetsAsAnEmptyArray(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	a := testAction(t)
	a.Targets = nil

	// Act.
	path, err := Emit(dir, a)
	if err != nil {
		t.Fatalf("Emit() error = %v, want nil", err)
	}
	raw, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("ReadFile: %v", err)
	}

	// Assert.
	if !strings.Contains(string(raw), `"targets":[]`) {
		t.Errorf("Emit() wrote %s, want targets serialized as []", raw)
	}
}

func TestEmitSerializesNilArgsAsAnEmptyObject(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	a := testAction(t)
	a.Args = nil

	// Act.
	path, err := Emit(dir, a)
	if err != nil {
		t.Fatalf("Emit() error = %v, want nil", err)
	}
	raw, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("ReadFile: %v", err)
	}

	// Assert.
	if !strings.Contains(string(raw), `"args":{}`) {
		t.Errorf("Emit() wrote %s, want args serialized as {}", raw)
	}
}

func TestEmitRejectsInvalidActions(t *testing.T) {
	tests := []struct {
		name    string
		mutate  func(*Action)
		wantErr string
	}{
		{
			name:    "empty action",
			mutate:  func(a *Action) { a.Action = "" },
			wantErr: "action is required",
		},
		{
			name:    "missing id",
			mutate:  func(a *Action) { a.ID = "" },
			wantErr: "id is required",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			dir := t.TempDir()
			a := testAction(t)
			tc.mutate(&a)

			// Act.
			_, err := Emit(dir, a)

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

func TestEmitRejectingAnInvalidActionWritesNothing(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	a := testAction(t)
	a.Action = ""

	// Act.
	if _, err := Emit(dir, a); err == nil {
		t.Fatal("Emit() error = nil, want a validation error")
	}
	entries, err := os.ReadDir(dir)
	if err != nil {
		t.Fatalf("ReadDir: %v", err)
	}

	// Assert.
	if len(entries) != 0 {
		t.Errorf("Emit() wrote %d files on a rejected action, want 0", len(entries))
	}
}

func TestEmitLeavesNoWatchableTempBehind(t *testing.T) {
	// Arrange.
	dir := t.TempDir()

	// Act.
	if _, err := Emit(dir, testAction(t)); err != nil {
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
	dir := filepath.Join(t.TempDir(), DirName)

	// Act.
	_, err := Emit(dir, testAction(t))

	// Assert.
	if err != nil {
		t.Fatalf("Emit() error = %v, want nil", err)
	}
	if _, err := os.Stat(dir); err != nil {
		t.Errorf("Emit() did not create %s: %v", dir, err)
	}
}

func TestNewIDMintsDistinctIDs(t *testing.T) {
	// Arrange & Act.
	first, err := NewID()
	if err != nil {
		t.Fatalf("first NewID() error = %v, want nil", err)
	}
	second, err := NewID()
	if err != nil {
		t.Fatalf("second NewID() error = %v, want nil", err)
	}

	// Assert.
	if first == second {
		t.Errorf("NewID() minted %q twice, want distinct ids", first)
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
	if want := "/tmp/some-root/sidebar-actions"; got != want {
		t.Errorf("Dir() = %q, want %q", got, want)
	}
}
