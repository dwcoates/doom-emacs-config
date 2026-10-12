package server

import (
	"context"
	"errors"
	"testing"
	"time"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/feed"
	"agent-repld/internal/wsm"
)

// listRollbackPrompts calls ListRollbackPrompts for the harness workspace.
func listRollbackPrompts(t *testing.T, h *harness) (*agentreplv1.ListRollbackPromptsResponse, error) {
	t.Helper()
	resp, err := h.Client.ListRollbackPrompts(context.Background(),
		connect.NewRequest(&agentreplv1.ListRollbackPromptsRequest{Workspace: ref()}))
	if err != nil {
		return nil, err
	}
	return resp.Msg, nil
}

func TestListRollbackPromptsAnswersNewestFirstWithWhatTellsThemApart(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	tokens := uint64(42_000)
	h.Feed.rollbackChoices = []feed.RollbackChoice{
		{Row: feedIDs("p1")[0], Turn: "t1", Said: said("the first prompt")},
		{Row: feedIDs("p2")[0], Turn: "t2", Said: said("the second prompt"), ContextTokens: &tokens},
	}
	h.DB.turnStarted = map[ids.TurnID]time.Time{"t1": time.UnixMilli(1_000), "t2": time.UnixMilli(2_000)}

	// Act.
	msg, err := listRollbackPrompts(t, h)

	// Assert.
	if err != nil {
		t.Fatalf("ListRollbackPrompts: %v", err)
	}
	got := msg.GetSuccess().GetPrompts()
	if len(got) != 2 {
		t.Fatalf("prompts = %v, want 2", got)
	}
	if got[0].GetPrompt().GetValue() != "p2" || got[0].GetExcerpt() != "the second prompt" ||
		got[0].GetSentAtMs() != 2_000 || got[0].ContextTokens == nil || got[0].GetContextTokens() != 42_000 {
		t.Fatalf("newest entry = %v, want p2 sent at 2000 with 42000 tokens", got[0])
	}
	if got[1].GetPrompt().GetValue() != "p1" || got[1].ContextTokens != nil {
		t.Fatalf("oldest entry = %v, want p1 with no context figure", got[1])
	}
}

func TestListRollbackPromptsLeavesAnAdoptedPromptsSendTimeUnset(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Feed.rollbackChoices = []feed.RollbackChoice{{Row: feedIDs("p1")[0], Turn: "t1", Said: said("adopted")}}
	h.DB.turnStartedErr = wsm.ErrNotFound

	// Act.
	msg, err := listRollbackPrompts(t, h)

	// Assert.
	if err != nil {
		t.Fatalf("ListRollbackPrompts: %v", err)
	}
	if entry := msg.GetSuccess().GetPrompts()[0]; entry.SentAtMs != nil {
		t.Fatalf("sent_at_ms = %d, want unset for a prompt the workspace never recorded", entry.GetSentAtMs())
	}
}

func TestListRollbackPromptsWithNoneAnswersAnEmptySuccess(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	msg, err := listRollbackPrompts(t, h)

	// Assert.
	if err != nil {
		t.Fatalf("ListRollbackPrompts: %v", err)
	}
	if msg.GetSuccess() == nil || len(msg.GetSuccess().GetPrompts()) != 0 {
		t.Fatalf("result = %v, want an empty success", msg.GetResult())
	}
}

func TestListRollbackPromptsSurfacesATurnReadFailure(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()
	h := newHarness(t)
	h.Surfaces.workspace = log
	h.Feed.rollbackChoices = []feed.RollbackChoice{{Row: feedIDs("p1")[0], Turn: "t1", Said: said("x")}}
	h.DB.turnStartedErr = errors.New("database is locked")

	// Act.
	_, err := listRollbackPrompts(t, h)

	// Assert.
	if code := connectCode(t, err); code != connect.CodeInternal {
		t.Fatalf("code = %v, want Internal", code)
	}
	for _, record := range log.Records() {
		if record.Level == "error" && record.Operation == "ListRollbackPrompts" && record.Context["cause"] == "database is locked" {
			return
		}
	}
	t.Fatalf("no error record named the failure: %+v", log.Records())
}

func TestListRollbackPromptsRequiresAValidWorkspace(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	resp, err := h.Client.ListRollbackPrompts(context.Background(), connect.NewRequest(
		&agentreplv1.ListRollbackPromptsRequest{Workspace: &workspacev1.WorkspaceRef{Id: "ws-nope"}}))

	// Assert.
	if err != nil {
		t.Fatalf("ListRollbackPrompts: %v", err)
	}
	if resp.Msg.GetError().GetUnknownWorkspace() == nil {
		t.Fatalf("result = %v, want unknown_workspace", resp.Msg.GetResult())
	}
}
