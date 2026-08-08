package server

import (
	"bytes"
	"encoding/json"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// emittedCommands reads every workspace-commands file under root's output
// dir and returns the entries found, so a test asserts on what Emacs
// would actually drain rather than on the handler's return value.
func emittedCommands(t *testing.T, root string) []map[string]any {
	t.Helper()
	entries, err := os.ReadDir(filepath.Join(root, "output"))
	if os.IsNotExist(err) {
		return nil
	}
	if err != nil {
		t.Fatalf("ReadDir: %v", err)
	}
	var out []map[string]any
	for _, e := range entries {
		if !strings.HasPrefix(e.Name(), "workspace_commands_") {
			continue
		}
		raw, err := os.ReadFile(filepath.Join(root, "output", e.Name()))
		if err != nil {
			t.Fatalf("ReadFile: %v", err)
		}
		var cmds []map[string]any
		if err := json.Unmarshal(raw, &cmds); err != nil {
			t.Fatalf("Unmarshal %s: %v", e.Name(), err)
		}
		out = append(out, cmds...)
	}
	return out
}

func postAddSupport(t *testing.T, h *harness, id, body string) *http.Response {
	t.Helper()
	resp, err := http.Post(h.ts.URL+"/sessions/"+id+"/add-support", "application/json",
		bytes.NewBufferString(body))
	if err != nil {
		t.Fatalf("POST add-support: %v", err)
	}
	t.Cleanup(func() { resp.Body.Close() })
	return resp
}

func TestAddSupportEmitsACreateForEmacs(t *testing.T) {
	usePrompts(t)
	// Arrange.
	root := t.TempDir()
	t.Setenv("AGENT_REPL_STATE_DIR", root)
	h := newHarness(t)
	id := createSession(t, h, `{"cwd":"/w"}`)

	// Act.
	resp := postAddSupport(t, h, id, `{"command":"status"}`)

	// Assert.
	if resp.StatusCode != http.StatusAccepted {
		t.Fatalf("status = %d, want 202", resp.StatusCode)
	}
	got := emittedCommands(t, root)
	if len(got) != 1 {
		t.Fatalf("emitted %d creates, want 1", len(got))
	}
	if got[0]["type"] != "create" {
		t.Errorf("type = %v, want create", got[0]["type"])
	}
	if got[0]["name"] != "add-support-status" {
		t.Errorf("name = %v, want add-support-status", got[0]["name"])
	}
}

func TestAddSupportUsesTheSessionCwdAsGitRoot(t *testing.T) {
	usePrompts(t)
	// Arrange.
	root := t.TempDir()
	t.Setenv("AGENT_REPL_STATE_DIR", root)
	h := newHarness(t)
	id := createSession(t, h, `{"cwd":"/some/checkout"}`)

	// Act.
	postAddSupport(t, h, id, `{"command":"status"}`)

	// Assert.
	got := emittedCommands(t, root)
	if len(got) != 1 {
		t.Fatalf("emitted %d creates, want 1", len(got))
	}
	if got[0]["git_root"] != "/some/checkout" {
		t.Errorf("git_root = %v, want /some/checkout", got[0]["git_root"])
	}
}

func TestAddSupportPromptNamesTheSessionConfigDir(t *testing.T) {
	usePrompts(t)
	// Arrange.
	root := t.TempDir()
	t.Setenv("AGENT_REPL_STATE_DIR", root)
	h := newHarness(t)
	id := createSession(t, h, `{"cwd":"/w","config_dir":"/home/u/.claude-chesscom"}`)

	// Act.
	postAddSupport(t, h, id, `{"command":"status"}`)

	// Assert.
	got := emittedCommands(t, root)
	if len(got) != 1 {
		t.Fatalf("emitted %d creates, want 1", len(got))
	}
	prompt, _ := got[0]["prompt"].(string)
	if !strings.Contains(prompt, "/home/u/.claude-chesscom") {
		t.Errorf("prompt = %q, want it to name the session's config dir", prompt)
	}
}

func TestAddSupportRefusesAnUnknownSession(t *testing.T) {
	usePrompts(t)
	// Arrange.
	root := t.TempDir()
	t.Setenv("AGENT_REPL_STATE_DIR", root)
	h := newHarness(t)

	// Act.
	resp := postAddSupport(t, h, "s_nosuch", `{"command":"status"}`)

	// Assert.
	if resp.StatusCode != http.StatusNotFound {
		t.Fatalf("status = %d, want 404", resp.StatusCode)
	}
	if got := emittedCommands(t, root); len(got) != 0 {
		t.Errorf("emitted %d creates for an unknown session, want 0", len(got))
	}
}

func TestAddSupportRefusesAnInvalidCommand(t *testing.T) {
	usePrompts(t)
	// Arrange.
	root := t.TempDir()
	t.Setenv("AGENT_REPL_STATE_DIR", root)
	h := newHarness(t)
	id := createSession(t, h, `{"cwd":"/w"}`)

	// Act.
	resp := postAddSupport(t, h, id, `{"command":"../../etc/passwd"}`)

	// Assert.
	if resp.StatusCode != http.StatusBadRequest {
		t.Fatalf("status = %d, want 400", resp.StatusCode)
	}
	if got := emittedCommands(t, root); len(got) != 0 {
		t.Errorf("emitted %d creates for an invalid command, want 0", len(got))
	}
}

func TestAddSupportRefusesAnEmptyCommand(t *testing.T) {
	usePrompts(t)
	// Arrange.
	root := t.TempDir()
	t.Setenv("AGENT_REPL_STATE_DIR", root)
	h := newHarness(t)
	id := createSession(t, h, `{"cwd":"/w"}`)

	// Act.
	resp := postAddSupport(t, h, id, `{"command":""}`)

	// Assert.
	if resp.StatusCode != http.StatusBadRequest {
		t.Fatalf("status = %d, want 400", resp.StatusCode)
	}
}

func TestAddSupportRefusesAMalformedBody(t *testing.T) {
	usePrompts(t)
	// Arrange.
	root := t.TempDir()
	t.Setenv("AGENT_REPL_STATE_DIR", root)
	h := newHarness(t)
	id := createSession(t, h, `{"cwd":"/w"}`)

	// Act.
	resp := postAddSupport(t, h, id, `{not json`)

	// Assert.
	if resp.StatusCode != http.StatusBadRequest {
		t.Fatalf("status = %d, want 400", resp.StatusCode)
	}
}

func TestAddSupportRefusesASessionWithNoWorkingDirectory(t *testing.T) {
	usePrompts(t)
	// Arrange.
	root := t.TempDir()
	t.Setenv("AGENT_REPL_STATE_DIR", root)
	h := newHarness(t)
	id := createSession(t, h, `{}`)

	// Act.
	resp := postAddSupport(t, h, id, `{"command":"status"}`)

	// Assert.
	// git_root is mandatory for Emacs's create handler, so a cwd-less
	// session must be refused rather than served a guessed root.
	if resp.StatusCode != http.StatusConflict {
		t.Fatalf("status = %d, want 409", resp.StatusCode)
	}
	if got := emittedCommands(t, root); len(got) != 0 {
		t.Errorf("emitted %d creates for a cwd-less session, want 0", len(got))
	}
}

func TestAddSupportRefusesWhenThePromptFileIsUnreadable(t *testing.T) {
	// Arrange — an empty prompts directory stands for a deleted or misnamed
	// prompt file. Opening a workspace around an empty brief would burn a
	// worktree, a branch, and a session on nothing, so the request is refused.
	t.Setenv("AGENT_REPL_PROMPTS_DIR", t.TempDir())
	root := t.TempDir()
	t.Setenv("AGENT_REPL_STATE_DIR", root)
	h := newHarness(t)
	id := createSession(t, h, `{"cwd":"/w"}`)

	// Act.
	resp := postAddSupport(t, h, id, `{"command":"status"}`)

	// Assert.
	if resp.StatusCode != http.StatusInternalServerError {
		t.Fatalf("status = %d, want 500", resp.StatusCode)
	}
	if got := emittedCommands(t, root); len(got) != 0 {
		t.Fatalf("emitted %d creates, want none when the brief could not be composed", len(got))
	}
}
