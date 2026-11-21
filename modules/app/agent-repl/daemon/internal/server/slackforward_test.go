package server

import (
	"context"
	"errors"
	"reflect"
	"strings"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/lifecycleconfig"
	"agent-repld/internal/prompthandler"
	"agent-repld/internal/workspace"
	"agent-repld/internal/wsm"
)

// forwardedThread is the Slack message a forward hands its recipient.
const forwardedThread = "https://acme.slack.com/archives/C0123/p1763300000000100"

// slackForwardRequest is an agent-sent prompt from SENTBY forwarding
// forwardedThread to the harness workspace.
func slackForwardRequest(sentBy *workspacev1.WorkspaceRef) *agentreplv1.SubmitPromptRequest {
	return &agentreplv1.SubmitPromptRequest{
		Workspace: ref(), Said: workspace.SaidText("another reaction came in"), IdempotencyKey: "k-1",
		Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_AGENT_SENT, SentBy: sentBy,
		SlackForward: &agentreplv1.SubmitPromptSlackForward{ThreadUrl: forwardedThread},
	}
}

// forwardHarness is a harness whose handler mints a turn for every prompt,
// with a registered sender, and records the logs in LOG.
func forwardHarness(t *testing.T, log *dlog.TestLogger) (*harness, *workspacev1.WorkspaceRef) {
	t.Helper()
	h := newHarness(t, func(deps *Deps) { deps.Log = &fakeSurfaces{global: log, workspace: log} })
	h.Prompts.outcome = prompthandler.Outcome{Recognition: prompthandler.RecognizedNone, Turn: "turn-7"}
	return h, registerSender(h)
}

// saidTexts answers SAID's text blocks, in order.
func saidTexts(said *conversationv1.UserSaid) []string {
	var out []string
	for _, b := range said.GetContent().GetBlocks() {
		out = append(out, b.GetText().GetText())
	}
	return out
}

func TestAForwardAppendsTheDirectiveAfterTheSendersWords(t *testing.T) {
	// Arrange
	h, sentBy := forwardHarness(t, dlog.NewTestLogger())

	// Act
	if _, err := h.Client.SubmitPrompt(context.Background(), connect.NewRequest(slackForwardRequest(sentBy))); err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}

	// Assert
	want := []string{"another reaction came in", lifecycleconfig.SlackForwardPrompt(forwardedThread, "test")}
	if got := saidTexts(h.Prompts.lastSaid); !reflect.DeepEqual(got, want) {
		t.Fatalf("delivered blocks = %q, want %q", got, want)
	}
}

func TestAForwardStoresTheThreadAsAReferenceLink(t *testing.T) {
	// Arrange
	h, sentBy := forwardHarness(t, dlog.NewTestLogger())

	// Act
	if _, err := h.Client.SubmitPrompt(context.Background(), connect.NewRequest(slackForwardRequest(sentBy))); err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}

	// Assert
	want := []string{"set " + forwardedThread + " " + ForwardedSlackThreadLabel + " reference"}
	if !reflect.DeepEqual(h.Verbs.linkChanges, want) {
		t.Fatalf("link changes = %q, want %q", h.Verbs.linkChanges, want)
	}
}

func TestAForwardOfAThreadAlreadyLinkedKeepsItsLabel(t *testing.T) {
	// Arrange: the recipient was created for this very thread.
	h, sentBy := forwardHarness(t, dlog.NewTestLogger())
	h.DB.workspaceLinks = map[wsm.WorkspaceID][]wsm.WorkspaceLink{testWorkspaceID: {{URL: forwardedThread, Label: workspace.SlackOriginLabel}}}

	// Act
	if _, err := h.Client.SubmitPrompt(context.Background(), connect.NewRequest(slackForwardRequest(sentBy))); err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}

	// Assert
	if len(h.Verbs.linkChanges) != 0 {
		t.Fatalf("link changes = %q, want none", h.Verbs.linkChanges)
	}
}

func TestARefusedForwardStoresNoLink(t *testing.T) {
	// Arrange
	h, sentBy := forwardHarness(t, dlog.NewTestLogger())
	h.Prompts.err = &workspace.Refusal{Arm: "cold_gate", Reason: "cold"}

	// Act
	resp, err := h.Client.SubmitPrompt(context.Background(), connect.NewRequest(slackForwardRequest(sentBy)))

	// Assert
	if err != nil || resp.Msg.GetError() == nil {
		t.Fatalf("SubmitPrompt = %v, %v; want a refusal", resp, err)
	}
	if len(h.Verbs.linkChanges) != 0 {
		t.Fatalf("link changes = %q, want none for a refused prompt", h.Verbs.linkChanges)
	}
}

func TestAForwardWhoseLinkCannotBeStoredFailsAndRecordsWhy(t *testing.T) {
	tests := []struct {
		name  string
		edit  func(*harness)
		cause string
	}{
		{name: "the links cannot be read", edit: func(h *harness) { h.DB.workspaceLinksErr = errors.New("disk gone") }, cause: "disk gone"},
		{name: "the link cannot be set", edit: func(h *harness) { h.Verbs.linksErr = errors.New("disk full") }, cause: "disk full"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			log := dlog.NewTestLogger()
			h, sentBy := forwardHarness(t, log)
			tt.edit(h)

			// Act
			_, err := h.Client.SubmitPrompt(context.Background(), connect.NewRequest(slackForwardRequest(sentBy)))

			// Assert
			if connectCode(t, err) != connect.CodeInternal {
				t.Fatalf("err = %v, want an internal failure", err)
			}
			rec, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: opSlackForward})
			if !ok || rec.Context["thread_url"] != forwardedThread || !strings.Contains(rec.Context["cause"].(string), tt.cause) {
				t.Fatalf("ERROR record = %v (%v), want the thread and cause %q", rec.Context, ok, tt.cause)
			}
		})
	}
}

func TestSubmitPromptRefusesAMalformedForward(t *testing.T) {
	sentBy := &workspacev1.WorkspaceRef{Id: string(senderWorkspaceID), Dir: "/tmp/sender"}
	tests := []struct {
		name string
		edit func(*agentreplv1.SubmitPromptRequest)
	}{
		{name: "a forward no agent sent", edit: func(r *agentreplv1.SubmitPromptRequest) {
			r.Origin = conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT
			r.SentBy = nil
		}},
		{name: "a forward to a subagent's feed", edit: func(r *agentreplv1.SubmitPromptRequest) { r.Feed = feedIDFor(testWorkspaceID) }},
		{name: "a forward whose link is no Slack message", edit: func(r *agentreplv1.SubmitPromptRequest) {
			r.SlackForward.ThreadUrl = "https://example.com/not-slack"
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			req := slackForwardRequest(sentBy)
			tt.edit(req)

			// Act
			err := validateSubmitPromptRequest(req)

			// Assert
			if err == nil || err.Code() != connect.CodeInvalidArgument {
				t.Fatalf("validate = %v, want InvalidArgument", err)
			}
		})
	}
}
