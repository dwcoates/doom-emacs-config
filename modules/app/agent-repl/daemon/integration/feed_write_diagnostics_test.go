//go:build integration

package integration

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"google.golang.org/protobuf/proto"
)

func TestDiagnosticsAfterTheirWriteCardLandOnThatCard(t *testing.T) {
	t.Parallel()
	// Arrange: the order the shim emits them in, the diagnostics report on the
	// write's own activity AFTER its settled success.
	f := newCanonical(t)
	f.submit("go", "k-diagnostics-after-write", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()
	write := func(result *conversationv1.AgentWrite) *conversationv1.AgentFrame {
		return activityFrame(mainAgent, &conversationv1.AgentActivity{
			ActivityId: activityID("write-diagnostics-after"),
			Item:       &conversationv1.AgentActivity_Write{Write: result},
		})
	}

	// Act
	f.shim.PushAgentFrame(mainAgent, write(&conversationv1.AgentWrite{Result: &conversationv1.AgentWrite_Start{
		Start: &conversationv1.AgentWriteStart{Path: &conversationv1.ReadPath{Path: "created.ts"}, StartedAt: startedAt(1)},
	}}))
	f.shim.PushAgentFrame(mainAgent, write(&conversationv1.AgentWrite{Result: &conversationv1.AgentWrite_Success{
		Success: &conversationv1.AgentWriteSuccess{
			Path:      &conversationv1.ReadPath{Path: "created.ts"},
			Outcome:   &conversationv1.AgentWriteSuccess_Created{Created: &conversationv1.AgentWriteCreated{}},
			Patch:     []*conversationv1.FilePatchHunk{{Lines: []string{"+export const created = missing;"}}},
			SettledAt: settledAt(2),
		},
	}}))
	f.shim.PushAgentFrame(mainAgent, write(&conversationv1.AgentWrite{Result: &conversationv1.AgentWrite_Diagnostics{
		Diagnostics: &conversationv1.AgentDiagnosticsReport{Files: []*conversationv1.AgentDiagnosticsFile{{
			Path: "created.ts",
			Diagnostics: []*conversationv1.AgentDiagnostic{{
				Severity: conversationv1.AgentDiagnosticSeverity_AGENT_DIAGNOSTIC_SEVERITY_ERROR,
				Message:  "Cannot find name 'missing'.",
				Source:   proto.String("typescript"),
			}},
		}}},
	}}))

	// Assert: the one Write card carries the diagnostics, composed as lines.
	row := awaitRow(t, f, tail, "the write card carrying its later diagnostics", func(r *frontendv1.FeedRow) bool {
		return r.GetActivity().GetSimpleToolCall().GetReturned().GetDiagnostics() != nil
	})
	card := row.GetActivity().GetSimpleToolCall()
	if card.GetName().GetText() != "Write" {
		t.Fatalf("the diagnostics-carrying card is headed %q, want %q", card.GetName().GetText(), "Write")
	}
	if lines := card.GetReturned().GetDiagnostics().GetLines(); len(lines) == 0 {
		t.Fatalf("the write card's diagnostics carry no composed lines: %v", card.GetReturned())
	}
}
