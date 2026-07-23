package server

import (
	"io"
	"net/http"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"
)

func postWorkspaceCommand(t *testing.T, h *harness, body string) *http.Response {
	t.Helper()
	resp, err := http.Post(h.ts.URL+"/workspace-command", "application/json",
		strings.NewReader(body))
	if err != nil {
		t.Fatalf("POST /workspace-command: %v", err)
	}
	t.Cleanup(func() { resp.Body.Close() })
	return resp
}

func responseBody(t *testing.T, resp *http.Response) string {
	t.Helper()
	raw, err := io.ReadAll(resp.Body)
	if err != nil {
		t.Fatalf("ReadAll: %v", err)
	}
	return string(raw)
}

func TestWorkspaceCommandEmitsASwitchForEmacs(t *testing.T) {
	// Arrange.
	root := t.TempDir()
	t.Setenv("AGENT_REPL_STATE_DIR", root)
	h := newHarness(t)

	// Act.
	resp := postWorkspaceCommand(t, h, `[{"type":"switch","dir":"/some/project"}]`)

	// Assert.
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("status = %d, want 200", resp.StatusCode)
	}
	got := emittedCommands(t, root)
	want := []map[string]any{{"type": "switch", "dir": "/some/project"}}
	if !reflect.DeepEqual(got, want) {
		t.Errorf("emitted %v, want %v", got, want)
	}
}

func TestWorkspaceCommandEmitsAFoldForEmacs(t *testing.T) {
	// Arrange.
	root := t.TempDir()
	t.Setenv("AGENT_REPL_STATE_DIR", root)
	h := newHarness(t)

	// Act.
	resp := postWorkspaceCommand(t, h, `[{"type":"fold","repo_key":"rk","folded":true}]`)

	// Assert.
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("status = %d, want 200", resp.StatusCode)
	}
	got := emittedCommands(t, root)
	want := []map[string]any{{"type": "fold", "repo_key": "rk", "folded": true}}
	if !reflect.DeepEqual(got, want) {
		t.Errorf("emitted %v, want %v", got, want)
	}
}

func TestWorkspaceCommandPreservesFoldedFalse(t *testing.T) {
	// Arrange.
	root := t.TempDir()
	t.Setenv("AGENT_REPL_STATE_DIR", root)
	h := newHarness(t)

	// Act.
	// folded=false is a real request (unfold), distinct from folded being
	// absent, and must land in the file as an explicit false.
	resp := postWorkspaceCommand(t, h, `[{"type":"fold","repo_key":"rk","folded":false}]`)

	// Assert.
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("status = %d, want 200", resp.StatusCode)
	}
	got := emittedCommands(t, root)
	if len(got) != 1 {
		t.Fatalf("emitted %d entries, want 1", len(got))
	}
	if folded, ok := got[0]["folded"].(bool); !ok || folded {
		t.Errorf("folded = %v, want explicit false", got[0]["folded"])
	}
}

func TestWorkspaceCommandEmitsAMixedArrayAsOneFile(t *testing.T) {
	// Arrange.
	root := t.TempDir()
	t.Setenv("AGENT_REPL_STATE_DIR", root)
	h := newHarness(t)

	// Act.
	resp := postWorkspaceCommand(t, h,
		`[{"type":"switch","dir":"/p"},{"type":"fold","repo_key":"rk","folded":true}]`)

	// Assert.
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("status = %d, want 200", resp.StatusCode)
	}
	files, err := os.ReadDir(filepath.Join(root, "output"))
	if err != nil {
		t.Fatalf("ReadDir: %v", err)
	}
	if len(files) != 1 {
		t.Fatalf("wrote %d files, want the whole batch in exactly 1", len(files))
	}
	got := emittedCommands(t, root)
	want := []map[string]any{
		{"type": "switch", "dir": "/p"},
		{"type": "fold", "repo_key": "rk", "folded": true},
	}
	if !reflect.DeepEqual(got, want) {
		t.Errorf("emitted %v, want %v", got, want)
	}
}

func TestWorkspaceCommandEmitsASetViewForEmacs(t *testing.T) {
	// Arrange.
	root := t.TempDir()
	t.Setenv("AGENT_REPL_STATE_DIR", root)
	h := newHarness(t)

	// Act.
	resp := postWorkspaceCommand(t, h, `[{"type":"set-view","view":"task"}]`)

	// Assert.
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("status = %d, want 200", resp.StatusCode)
	}
	got := emittedCommands(t, root)
	want := []map[string]any{{"type": "set-view", "view": "task"}}
	if !reflect.DeepEqual(got, want) {
		t.Errorf("emitted %v, want %v", got, want)
	}
}

func TestWorkspaceCommandEmitsTheTaskGesturesForEmacs(t *testing.T) {
	// Arrange.
	root := t.TempDir()
	t.Setenv("AGENT_REPL_STATE_DIR", root)
	h := newHarness(t)

	// Act — the Task view's four id-bearing (or id-less) gestures.
	resp := postWorkspaceCommand(t, h,
		`[{"type":"task-create"},{"type":"task-toggle-done","id":"t1"},`+
			`{"type":"task-open","id":"t1"},{"type":"task-add-workspace","id":"t1"}]`)

	// Assert.
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("status = %d, want 200", resp.StatusCode)
	}
	got := emittedCommands(t, root)
	want := []map[string]any{
		{"type": "task-create"},
		{"type": "task-toggle-done", "id": "t1"},
		{"type": "task-open", "id": "t1"},
		{"type": "task-add-workspace", "id": "t1"},
	}
	if !reflect.DeepEqual(got, want) {
		t.Errorf("emitted %v, want %v", got, want)
	}
}

func TestWorkspaceCommandRefusesABadView(t *testing.T) {
	// Arrange.
	root := t.TempDir()
	t.Setenv("AGENT_REPL_STATE_DIR", root)
	h := newHarness(t)

	// Act.
	resp := postWorkspaceCommand(t, h, `[{"type":"set-view","view":"kanban"}]`)

	// Assert.
	if resp.StatusCode != http.StatusBadRequest {
		t.Fatalf("status = %d, want 400", resp.StatusCode)
	}
	if got := emittedCommands(t, root); len(got) != 0 {
		t.Errorf("emitted %d entries for a bad view, want 0", len(got))
	}
}

func TestWorkspaceCommandRefusesATaskToggleMissingID(t *testing.T) {
	// Arrange.
	root := t.TempDir()
	t.Setenv("AGENT_REPL_STATE_DIR", root)
	h := newHarness(t)

	// Act.
	resp := postWorkspaceCommand(t, h, `[{"type":"task-toggle-done"}]`)

	// Assert.
	if resp.StatusCode != http.StatusBadRequest {
		t.Fatalf("status = %d, want 400", resp.StatusCode)
	}
	if body := responseBody(t, resp); !strings.Contains(body, "id is required") {
		t.Errorf("body = %q, want it to name the missing id", body)
	}
}

func TestWorkspaceCommandRefusesAnEmptyArray(t *testing.T) {
	// Arrange.
	root := t.TempDir()
	t.Setenv("AGENT_REPL_STATE_DIR", root)
	h := newHarness(t)

	// Act.
	resp := postWorkspaceCommand(t, h, `[]`)

	// Assert.
	if resp.StatusCode != http.StatusBadRequest {
		t.Fatalf("status = %d, want 400", resp.StatusCode)
	}
	if got := emittedCommands(t, root); len(got) != 0 {
		t.Errorf("emitted %d entries for an empty array, want 0", len(got))
	}
}

func TestWorkspaceCommandRefusesAnUnsupportedType(t *testing.T) {
	// Arrange.
	root := t.TempDir()
	t.Setenv("AGENT_REPL_STATE_DIR", root)
	h := newHarness(t)

	// Act.
	// "create" is a real workspace-command kind, but this route does not
	// compose creates; it must be refused, not passed through.
	resp := postWorkspaceCommand(t, h,
		`[{"type":"create","name":"n","git_root":"/r","prompt":"p"}]`)

	// Assert.
	if resp.StatusCode != http.StatusBadRequest {
		t.Fatalf("status = %d, want 400", resp.StatusCode)
	}
	if body := responseBody(t, resp); !strings.Contains(body, "entry 0") {
		t.Errorf("body = %q, want it to name entry 0", body)
	}
	if got := emittedCommands(t, root); len(got) != 0 {
		t.Errorf("emitted %d entries for an unsupported type, want 0", len(got))
	}
}

func TestWorkspaceCommandRefusesASwitchMissingDir(t *testing.T) {
	// Arrange.
	root := t.TempDir()
	t.Setenv("AGENT_REPL_STATE_DIR", root)
	h := newHarness(t)

	// Act.
	resp := postWorkspaceCommand(t, h, `[{"type":"switch"}]`)

	// Assert.
	if resp.StatusCode != http.StatusBadRequest {
		t.Fatalf("status = %d, want 400", resp.StatusCode)
	}
	if body := responseBody(t, resp); !strings.Contains(body, "dir is required") {
		t.Errorf("body = %q, want it to name the missing dir", body)
	}
	if got := emittedCommands(t, root); len(got) != 0 {
		t.Errorf("emitted %d entries for a dir-less switch, want 0", len(got))
	}
}

func TestWorkspaceCommandRefusesAFoldMissingRepoKey(t *testing.T) {
	// Arrange.
	root := t.TempDir()
	t.Setenv("AGENT_REPL_STATE_DIR", root)
	h := newHarness(t)

	// Act.
	resp := postWorkspaceCommand(t, h, `[{"type":"fold","folded":true}]`)

	// Assert.
	if resp.StatusCode != http.StatusBadRequest {
		t.Fatalf("status = %d, want 400", resp.StatusCode)
	}
	if body := responseBody(t, resp); !strings.Contains(body, "repo_key is required") {
		t.Errorf("body = %q, want it to name the missing repo_key", body)
	}
}

func TestWorkspaceCommandRefusesAFoldMissingFolded(t *testing.T) {
	// Arrange.
	root := t.TempDir()
	t.Setenv("AGENT_REPL_STATE_DIR", root)
	h := newHarness(t)

	// Act.
	resp := postWorkspaceCommand(t, h, `[{"type":"fold","repo_key":"rk"}]`)

	// Assert.
	if resp.StatusCode != http.StatusBadRequest {
		t.Fatalf("status = %d, want 400", resp.StatusCode)
	}
	if body := responseBody(t, resp); !strings.Contains(body, "folded is required") {
		t.Errorf("body = %q, want it to name the missing folded", body)
	}
}

func TestWorkspaceCommandNamesTheOffendingIndex(t *testing.T) {
	// Arrange.
	root := t.TempDir()
	t.Setenv("AGENT_REPL_STATE_DIR", root)
	h := newHarness(t)

	// Act.
	// The first entry is valid; the refusal must name the second AND drop
	// the whole batch — a partially-honored array would silently lose
	// entry 1 while looking like success.
	resp := postWorkspaceCommand(t, h,
		`[{"type":"switch","dir":"/p"},{"type":"fold","repo_key":""}]`)

	// Assert.
	if resp.StatusCode != http.StatusBadRequest {
		t.Fatalf("status = %d, want 400", resp.StatusCode)
	}
	if body := responseBody(t, resp); !strings.Contains(body, "entry 1") {
		t.Errorf("body = %q, want it to name entry 1", body)
	}
	if got := emittedCommands(t, root); len(got) != 0 {
		t.Errorf("emitted %d entries from a refused batch, want 0", len(got))
	}
}

func TestWorkspaceCommandRefusesAMalformedBody(t *testing.T) {
	// Arrange.
	root := t.TempDir()
	t.Setenv("AGENT_REPL_STATE_DIR", root)
	h := newHarness(t)

	// Act.
	resp := postWorkspaceCommand(t, h, `{not json`)

	// Assert.
	if resp.StatusCode != http.StatusBadRequest {
		t.Fatalf("status = %d, want 400", resp.StatusCode)
	}
}

func TestWorkspaceCommandRefusesANonArrayBody(t *testing.T) {
	// Arrange.
	root := t.TempDir()
	t.Setenv("AGENT_REPL_STATE_DIR", root)
	h := newHarness(t)

	// Act.
	resp := postWorkspaceCommand(t, h, `{"type":"switch","dir":"/p"}`)

	// Assert.
	if resp.StatusCode != http.StatusBadRequest {
		t.Fatalf("status = %d, want 400", resp.StatusCode)
	}
	if got := emittedCommands(t, root); len(got) != 0 {
		t.Errorf("emitted %d entries for a non-array body, want 0", len(got))
	}
}
