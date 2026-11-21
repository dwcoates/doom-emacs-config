package server

import (
	"context"
	"strings"
	"testing"

	"connectrpc.com/connect"

	"agent-repld/internal/dlog"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	workspacev1 "agentrepl/proto/workspace/v1"
)

// summaryCall is one warm prompt-summary call's raw usage.
func summaryCall() *agentreplv1.ReportedVendorCall {
	return &agentreplv1.ReportedVendorCall{Model: "claude-haiku-5-5", Tokens: &conversationv1.VendorTokens{CacheRead: 100, Output: 10}}
}

const summaryCause = conversationv1.VendorCallCause_VENDOR_CALL_CAUSE_PROMPT_SUMMARY

func reportSpend(t *testing.T, h *harness, ws *workspacev1.WorkspaceRef, configDir string, cause conversationv1.VendorCallCause, calls ...*agentreplv1.ReportedVendorCall) (*agentreplv1.ReportVendorSpendResponse, error) {
	t.Helper()
	resp, err := h.Client.ReportVendorSpend(context.Background(), connect.NewRequest(&agentreplv1.ReportVendorSpendRequest{Workspace: ws, ConfigDir: configDir, Cause: cause, Calls: calls}))
	if err != nil {
		return nil, err
	}
	return resp.Msg, nil
}

func TestReportVendorSpendRecordsAValidReport(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	resp, err := reportSpend(t, h, ref(), "/home/u/.claude", summaryCause, summaryCall())
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
	noModel := summaryCall()
	noModel.Model = ""
	tests := []struct {
		name      string
		configDir string
		cause     conversationv1.VendorCallCause
		calls     []*agentreplv1.ReportedVendorCall
		want      string
	}{
		{name: "a call naming no model", configDir: "/home/u/.claude", cause: summaryCause, calls: []*agentreplv1.ReportedVendorCall{noModel}, want: "names no model"},
		{name: "a call with no usage", configDir: "/home/u/.claude", cause: summaryCause, calls: []*agentreplv1.ReportedVendorCall{{Model: "claude-haiku-5-5"}}, want: "no usage"},
		{name: "no account root", configDir: "", cause: summaryCause, calls: []*agentreplv1.ReportedVendorCall{summaryCall()}, want: "names no account root"},
		{name: "no cause", configDir: "/home/u/.claude", calls: []*agentreplv1.ReportedVendorCall{summaryCall()}, want: "states no cause"},
		{name: "the unstated cause", configDir: "/home/u/.claude", cause: conversationv1.VendorCallCause_VENDOR_CALL_CAUSE_UNSTATED, calls: []*agentreplv1.ReportedVendorCall{summaryCall()}, want: "states no cause"},
		{name: "an undeclared cause", configDir: "/home/u/.claude", cause: 99, calls: []*agentreplv1.ReportedVendorCall{summaryCall()}, want: "not a declared cause"},
		{name: "no call", configDir: "/home/u/.claude", cause: summaryCause, want: "carries no call"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			log := dlog.NewTestLogger()
			h := newHarness(t, func(deps *Deps) { deps.Log = &fakeSurfaces{global: log} })
			// Act
			resp, err := reportSpend(t, h, ref(), tt.configDir, tt.cause, tt.calls...)
			// Assert
			if err != nil || !strings.Contains(resp.GetError().GetInvalid().GetDetail(), tt.want) {
				t.Fatalf("ReportVendorSpend = %v, %v; want the invalid arm naming %q", resp, err, tt.want)
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
	_, err := reportSpend(t, h, nil, "/home/u/.claude", summaryCause, summaryCall())
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
	resp, err := reportSpend(t, h, ref(), "/home/u/.claude", summaryCause, summaryCall())
	// Assert
	if err != nil || resp.GetSuccess().GetMetricsOff() == nil || len(h.VendorSpend.reports()) != 0 {
		t.Fatalf("ReportVendorSpend = %v, %v; want metrics off and nothing recorded", resp, err)
	}
}
