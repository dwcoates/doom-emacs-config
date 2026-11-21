package server

import (
	"context"
	"testing"

	"connectrpc.com/connect"

	"agent-repld/internal/dlog"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	workspacev1 "agentrepl/proto/workspace/v1"
)

// summaryCall is one warm headless prompt-summary call.
func summaryCall() *conversationv1.VendorSpend {
	return &conversationv1.VendorSpend{
		Model:        "claude-haiku-5-5",
		Agent:        conversationv1.VendorCallAgent_VENDOR_CALL_AGENT_HEADLESS,
		Cause:        conversationv1.VendorCallCause_VENDOR_CALL_CAUSE_PROMPT_SUMMARY,
		CacheOutcome: conversationv1.VendorCacheOutcome_VENDOR_CACHE_OUTCOME_WARM,
		Calls:        1,
		Tokens:       &conversationv1.VendorTokens{CacheRead: 100, Output: 10},
	}
}

func reportSpend(t *testing.T, h *harness, ws *workspacev1.WorkspaceRef, configDir string, entries ...*conversationv1.VendorSpend) (*agentreplv1.ReportVendorSpendResponse, error) {
	t.Helper()
	resp, err := h.Client.ReportVendorSpend(context.Background(), connect.NewRequest(&agentreplv1.ReportVendorSpendRequest{Workspace: ws, ConfigDir: configDir, Spend: entries}))
	if err != nil {
		return nil, err
	}
	return resp.Msg, nil
}

func TestReportVendorSpendRecordsAValidReport(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	resp, err := reportSpend(t, h, ref(), "/home/u/.claude", summaryCall())
	// Assert
	if err != nil || resp.GetSuccess().GetRecorded() == nil {
		t.Fatalf("ReportVendorSpend = %v, %v; want recorded", resp, err)
	}
	got := h.VendorSpend.reports()
	if len(got) != 1 || got[0].ws != testWorkspaceID || got[0].configDir != "/home/u/.claude" || got[0].entries != 1 {
		t.Fatalf("recorded %+v, want the one report", got)
	}
}

func TestReportVendorSpendRefusesAnInvalidReportOnTheInvalidArmAndRecordsIt(t *testing.T) {
	broken := summaryCall()
	broken.Model = ""
	tests := []struct {
		name      string
		configDir string
		entries   []*conversationv1.VendorSpend
		cause     string
	}{
		{name: "an entry naming no model", configDir: "/home/u/.claude", entries: []*conversationv1.VendorSpend{broken}, cause: "names no model"},
		{name: "no account root", configDir: "", entries: []*conversationv1.VendorSpend{summaryCall()}, cause: "names no account root"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			log := dlog.NewTestLogger()
			h := newHarness(t, func(deps *Deps) { deps.Log = &fakeSurfaces{global: log} })
			// Act
			resp, err := reportSpend(t, h, ref(), tt.configDir, tt.entries...)
			// Assert
			if err != nil || resp.GetError().GetInvalid() == nil {
				t.Fatalf("ReportVendorSpend = %v, %v; want the invalid arm", resp, err)
			}
			if len(h.VendorSpend.reports()) != 0 {
				t.Fatal("an invalid report was recorded")
			}
			record, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: "ReportVendorSpend"})
			if !ok || record.Context["workspace"] != string(testWorkspaceID) || record.Context["config_dir"] != tt.configDir {
				t.Fatalf("no ERROR record naming the workspace and the root: %+v", record)
			}
		})
	}
}

func TestReportVendorSpendRefusesAReportWithNoWorkspace(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	_, err := reportSpend(t, h, nil, "/home/u/.claude", summaryCall())
	// Assert
	if connectCode(t, err) != connect.CodeInvalidArgument {
		t.Fatalf("ReportVendorSpend with no workspace = %v, want InvalidArgument", err)
	}
}

func TestReportVendorSpendSaysTheMetricsAreOff(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.VendorSpend.enabled = false
	// Act
	resp, err := reportSpend(t, h, ref(), "/home/u/.claude", summaryCall())
	// Assert
	if err != nil || resp.GetSuccess().GetMetricsOff() == nil || len(h.VendorSpend.reports()) != 0 {
		t.Fatalf("ReportVendorSpend = %v, %v; want metrics off and nothing recorded", resp, err)
	}
}
