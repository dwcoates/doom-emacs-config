package server

import (
	"errors"
	"fmt"
	"path/filepath"
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/errclass"
	"claude-repld/internal/registry"
)

type fixedSessionTokenUsageSource struct {
	records []*frontendv1.TokenUtilization
	err     error
}

func (s fixedSessionTokenUsageSource) List(string) ([]*frontendv1.TokenUtilization, error) {
	return s.records, s.err
}

func TestSessionTokenUtilizationReadFailureLogsAndFailsHard(t *testing.T) {
	var logs []string
	defer func() {
		if recover() == nil {
			t.Fatal("durable usage read failure did not panic")
		}
		joined := strings.Join(logs, "\n")
		if !strings.Contains(joined, "session token utilization read FAILED") || !strings.Contains(joined, `session="s_usage"`) || !strings.Contains(joined, "disk unavailable") {
			t.Fatalf("logs = %v", logs)
		}
	}()
	sessionTokenUtilization(func(format string, args ...any) { logs = append(logs, fmt.Sprintf(format, args...)) }, fixedSessionTokenUsageSource{err: errors.New("disk unavailable")}, "s_usage")
}

func TestSessionTokenUtilizationRejectsPoisonedDurableModelBeforeFrameConstruction(t *testing.T) {
	var logs []string
	record := &frontendv1.TokenUtilization{
		AgentReplSessionId: "daemon-session",
		ClaudeSessionId:    "claude-session",
		ApiMessageId:       "api-message",
		Model:              " \n ",
		Usage:              &frontendv1.VendorTokenUsage{InputTokens: 1},
	}
	defer func() {
		if recover() == nil {
			t.Fatal("poisoned durable model did not abort SessionView construction")
		}
		joined := strings.Join(logs, "\n")
		for _, want := range []string{
			"SessionView token utilization aggregation REFUSED",
			"source_plane=durable-store",
			`requested_session_id="daemon-session"`,
			"field_path=TokenUtilization.model",
			`agent_repl_session_id="daemon-session"`,
			`claude_session_id="claude-session"`,
			`api_message_id="api-message"`,
			`model=" \n "`,
		} {
			if !strings.Contains(joined, want) {
				t.Fatalf("diagnostic logs = %q, missing %q", joined, want)
			}
		}
	}()
	_ = sessionTokenUtilization(func(format string, args ...any) { logs = append(logs, fmt.Sprintf(format, args...)) }, fixedSessionTokenUsageSource{records: []*frontendv1.TokenUtilization{record}}, "daemon-session")
}

// TestRegistrySessionViewsPopulatesRegistryFields verifies the SessionView
// carries the model, permission mode, claude_session_id, and cwd fields from
// the registry record (design §14.2 step 3), and leaves slug/title blank
// because the registry does not retain them.
func TestRegistrySessionViewsPopulatesRegistryFields(t *testing.T) {
	// Arrange.
	reg := registry.Open(filepath.Join(t.TempDir(), "registry.db"), func(string, ...any) {})
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
	views := RegistrySessions{Reg: reg}.SessionViews()

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

func TestRegistrySessionViewsCarriesDurableCompletedUsage(t *testing.T) {
	reg := registry.Open(filepath.Join(t.TempDir(), "registry.db"), func(string, ...any) {})
	if err := reg.Put(registry.Record{SessionID: "s_usage", CWD: "/work/ws"}); err != nil {
		t.Fatal(err)
	}
	records := []*frontendv1.TokenUtilization{
		{ApiMessageId: "m1", Model: "fable", Actor: &frontendv1.TokenUtilization_MainAgent{MainAgent: &frontendv1.TokenUtilizationMainAgent{}}, Usage: &frontendv1.VendorTokenUsage{InputTokens: 3}},
		{ApiMessageId: "m2", Model: "opus", Actor: &frontendv1.TokenUtilization_Subagent{Subagent: &frontendv1.TokenUtilizationSubagent{AgentId: "agent"}}, Usage: &frontendv1.VendorTokenUsage{CacheReadInputTokens: 7}},
	}
	// The aggregate LEFT SessionView with the rest of the durable evidence
	// layer; economics reach a rendering frontend as TokenBreakdownView. What
	// this test still owns is that the roster path READS the durable source and
	// aggregates it correctly, which is asserted against the same aggregation
	// the view path calls.
	views := RegistrySessions{Reg: reg, TokenUsage: fixedSessionTokenUsageSource{records: records}}.SessionViews()
	if len(views) != 1 {
		t.Fatalf("session views = %d, want 1", len(views))
	}
	usage := sessionTokenUtilization(nil, fixedSessionTokenUsageSource{records: records}, "s_usage")
	if usage.GetAllAgents().GetInputTokens() != 3 || usage.GetAllAgents().GetCacheReadInputTokens() != 7 || len(usage.GetSubagents()) != 1 || len(usage.GetSubagents()[0].GetModels()) != 1 {
		t.Fatalf("token utilization = %+v", usage)
	}
}

// TestRegistrySessionViewsIncludesTerminalRecords verifies the S7 parity
// change: a terminal record IS included now (the orphan/reattach sweep
// re-keys on its terminal field), carrying terminal=true, so Emacs can drop
// the GET /sessions poller.
func TestRegistrySessionViewsIncludesTerminalRecords(t *testing.T) {
	// Arrange.
	reg := registry.Open(filepath.Join(t.TempDir(), "registry.db"), func(string, ...any) {})
	if err := reg.Put(registry.Record{SessionID: "s_terminal", CWD: "/work/x", Terminal: true, DeathReason: "delete session"}); err != nil {
		t.Fatalf("registry Put terminal: %v", err)
	}

	// Act.
	views := RegistrySessions{Reg: reg}.SessionViews()

	// Assert.
	if len(views) != 1 {
		t.Fatalf("expected 1 session view (terminal included), got %d", len(views))
	}
	v := views[0]
	if !v.GetTerminal() {
		t.Errorf("terminal: got false, want true")
	}
	if got := errclass.TypeName(v.GetDeath()); got != string(errclass.TypeSessionDeleted) {
		t.Errorf("death.error_type: got %q, want %q", got, errclass.TypeSessionDeleted)
	}
}

// TestRegistrySessionViewsSkipsDirlessRecords verifies a cwd-less record is
// excluded (no workspace to key by), even though terminal records are included.
func TestRegistrySessionViewsSkipsDirlessRecords(t *testing.T) {
	// Arrange.
	reg := registry.Open(filepath.Join(t.TempDir(), "registry.db"), func(string, ...any) {})
	if err := reg.Put(registry.Record{SessionID: "s_nodir"}); err != nil {
		t.Fatalf("registry Put dirless: %v", err)
	}

	// Act.
	views := RegistrySessions{Reg: reg}.SessionViews()

	// Assert.
	if len(views) != 0 {
		t.Fatalf("expected 0 session views (dirless excluded), got %d", len(views))
	}
}

// THE CONNECT SNAPSHOT IS WHERE THE RESTART SHAPE BITES. It is the first thing
// a frontend sees after a daemon restart, and it is what a switch-ensure
// consults before deciding a workspace has nothing to bootstrap. A record whose
// durable fields all say "up" must still report no shim attached when this
// daemon holds no session controller for it — otherwise every switch skips and no workspace
// ever bootstraps.
func TestRegistrySessionViewsReportNoShimWithoutASessionController(t *testing.T) {
	// Arrange — a fully-settled record and no session controller at all (a fresh boot).
	reg := registry.Open(filepath.Join(t.TempDir(), "sessions.db"), nil)
	if err := reg.Put(registry.Record{
		SessionID:       "s_restart",
		CWD:             "/work/ws",
		ClaudeSessionID: "cli-uuid-1",
	}); err != nil {
		t.Fatalf("registry Put: %v", err)
	}

	// Act.
	views := RegistrySessions{Reg: reg}.SessionViews()

	// Assert.
	if len(views) != 1 {
		t.Fatalf("expected 1 session view, got %d", len(views))
	}
	if views[0].GetShimAttached() {
		t.Fatal("the connect snapshot claimed a shim was attached with no session controller wired")
	}
}
