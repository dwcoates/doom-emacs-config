package main

import (
	"path/filepath"
	"testing"

	"claude-repld/internal/registry"
)

// TestRegistrySessionViewsPopulatesRegistryFields verifies the SessionView
// carries the model, permission mode, claude_session_id, and cwd fields from
// the registry record (design §14.2 step 3), and leaves slug/title blank
// because the registry does not retain them.
func TestRegistrySessionViewsPopulatesRegistryFields(t *testing.T) {
	// Arrange.
	reg := registry.Open(filepath.Join(t.TempDir(), "registry.json"), func(string, ...any) {})
	if err := reg.Put(registry.Record{
		SessionID:       "s_abc",
		CWD:             "/work/ws",
		Model:           "sonnet",
		PermissionMode:  "plan",
		ClaudeSessionID: "cli-uuid-1",
	}); err != nil {
		t.Fatalf("registry Put: %v", err)
	}

	// Act.
	views := registrySessions{reg: reg}.SessionViews()

	// Assert.
	if len(views) != 1 {
		t.Fatalf("expected 1 session view, got %d", len(views))
	}
	v := views[0]
	if got := v.GetSessionId(); got != "s_abc" {
		t.Errorf("session_id: got %q, want s_abc", got)
	}
	if got := v.GetWorkspace(); got != "/work/ws" {
		t.Errorf("workspace: got %q, want /work/ws", got)
	}
	if got := v.GetCwd(); got != "/work/ws" {
		t.Errorf("cwd: got %q, want /work/ws", got)
	}
	if got := v.GetModel(); got != "sonnet" {
		t.Errorf("model: got %q, want sonnet", got)
	}
	if got := v.GetPermissionMode(); got != "plan" {
		t.Errorf("permission_mode: got %q, want plan", got)
	}
	if got := v.GetClaudeSessionId(); got != "cli-uuid-1" {
		t.Errorf("claude_session_id: got %q, want cli-uuid-1", got)
	}
	if got := v.GetSlug(); got != "" {
		t.Errorf("slug: got %q, want blank (registry does not retain it)", got)
	}
	if got := v.GetTitle(); got != "" {
		t.Errorf("title: got %q, want blank (registry does not retain it)", got)
	}
}

// TestRegistrySessionViewsIncludesTerminalRecords verifies the S7 parity
// change: a terminal record IS included now (the orphan/reattach sweep re-keys
// on its terminal + death_reason fields), carrying terminal=true and its death
// reason, so Emacs can drop the GET /sessions poller.
func TestRegistrySessionViewsIncludesTerminalRecords(t *testing.T) {
	// Arrange.
	reg := registry.Open(filepath.Join(t.TempDir(), "registry.json"), func(string, ...any) {})
	if err := reg.Put(registry.Record{SessionID: "s_terminal", CWD: "/work/x", Terminal: true, DeathReason: "delete session"}); err != nil {
		t.Fatalf("registry Put terminal: %v", err)
	}

	// Act.
	views := registrySessions{reg: reg}.SessionViews()

	// Assert.
	if len(views) != 1 {
		t.Fatalf("expected 1 session view (terminal included), got %d", len(views))
	}
	v := views[0]
	if !v.GetTerminal() {
		t.Errorf("terminal: got false, want true")
	}
	if got := v.GetDeathReason(); got != "delete session" {
		t.Errorf("death_reason: got %q, want %q", got, "delete session")
	}
}

// TestRegistrySessionViewsSkipsDirlessRecords verifies a cwd-less record is
// excluded (no workspace to key by), even though terminal records are included.
func TestRegistrySessionViewsSkipsDirlessRecords(t *testing.T) {
	// Arrange.
	reg := registry.Open(filepath.Join(t.TempDir(), "registry.json"), func(string, ...any) {})
	if err := reg.Put(registry.Record{SessionID: "s_nodir"}); err != nil {
		t.Fatalf("registry Put dirless: %v", err)
	}

	// Act.
	views := registrySessions{reg: reg}.SessionViews()

	// Assert.
	if len(views) != 0 {
		t.Fatalf("expected 0 session views (dirless excluded), got %d", len(views))
	}
}
