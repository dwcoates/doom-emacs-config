package workspace

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/ids"
	"claude-repld/internal/resolve/feed"
	"claude-repld/internal/resolve/topbar"
)

// fakeServedFeed is a feed.Resolver that answers only the served-ask reads.
type fakeServedFeed struct {
	feed.Resolver
	agent    *conversationv1.AgentId
	standing *conversationv1.AgentPermissionStanding
	batch    *conversationv1.AgentQuestionBatch
	known    bool
}

func (f *fakeServedFeed) ServedPermission(ids.WorkspaceID, string) (*conversationv1.AgentId, *conversationv1.AgentPermissionStanding, bool) {
	return f.agent, f.standing, f.known
}

func (f *fakeServedFeed) ServedQuestion(ids.WorkspaceID, string) (*conversationv1.AgentId, *conversationv1.AgentQuestionBatch, bool) {
	return f.agent, f.batch, f.known
}

// fakeServedTopbar is a topbar.Resolver that answers only the served mode set.
type fakeServedTopbar struct {
	topbar.Resolver
	modes []string
	known bool
}

func (f *fakeServedTopbar) PermissionModes(ids.WorkspaceID) ([]string, bool) {
	return f.modes, f.known
}

// TestCardsPermissionCarriesTheAskingAgent covers the fact an answer cannot be
// delivered without: the client sends only the ask id.
func TestCardsPermissionCarriesTheAskingAgent(t *testing.T) {
	// Arrange.
	c := NewCards(&fakeServedFeed{agent: &conversationv1.AgentId{Value: "agent-1"}, known: true}, &fakeServedTopbar{}, nil)

	// Act.
	served, ok := c.Permission("ws-1", &conversationv1.AgentPermissionId{Value: "ask-1"})

	// Assert.
	if !ok {
		t.Fatal("Permission reported no served ask, want the drawn one")
	}
	if served.Agent.GetValue() != "agent-1" {
		t.Fatalf("served agent = %q, want agent-1", served.Agent.GetValue())
	}
}

// TestCardsPermissionReportsAnUnknownAsk covers the ask this workspace never
// drew: it is reported absent rather than answered with a zero card.
func TestCardsPermissionReportsAnUnknownAsk(t *testing.T) {
	// Arrange.
	c := NewCards(&fakeServedFeed{}, &fakeServedTopbar{}, nil)

	// Act.
	_, ok := c.Permission("ws-1", &conversationv1.AgentPermissionId{Value: "ask-1"})

	// Assert.
	if ok {
		t.Fatal("Permission answered an ask the workspace never drew")
	}
}

// TestCardsQuestionCarriesTheServedBatch covers the echo an answer is checked
// against: a question the batch never carried must be refusable.
func TestCardsQuestionCarriesTheServedBatch(t *testing.T) {
	// Arrange.
	batch := &conversationv1.AgentQuestionBatch{
		Questions: []*conversationv1.AgentQuestionAsked{{Header: "which branch"}},
	}
	c := NewCards(&fakeServedFeed{agent: &conversationv1.AgentId{Value: "agent-1"}, batch: batch, known: true}, &fakeServedTopbar{}, nil)

	// Act.
	served, ok := c.Question("ws-1", &conversationv1.AgentQuestionId{Value: "ask-1"})

	// Assert.
	if !ok {
		t.Fatal("Question reported no served ask, want the drawn one")
	}
	if len(served.Batch.GetQuestions()) != 1 {
		t.Fatalf("served batch = %v, want the one question that was served", served.Batch)
	}
}

// TestCardsPermissionModesComeFromTheTopbar covers the served picker set: a
// mode switch is validated against exactly what was offered.
func TestCardsPermissionModesComeFromTheTopbar(t *testing.T) {
	// Arrange.
	c := NewCards(&fakeServedFeed{}, &fakeServedTopbar{modes: []string{"default", "plan"}, known: true}, nil)

	// Act.
	modes, ok := c.PermissionModes("ws-1")

	// Assert.
	if !ok || len(modes) != 2 || modes[0] != "default" || modes[1] != "plan" {
		t.Fatalf("PermissionModes = %v, %v, want the served set", modes, ok)
	}
}

// TestCardsPermissionModesReportAnUnservedPicker covers the workspace whose
// session never stated a picker.
func TestCardsPermissionModesReportAnUnservedPicker(t *testing.T) {
	// Arrange.
	c := NewCards(&fakeServedFeed{}, &fakeServedTopbar{}, nil)

	// Act.
	_, ok := c.PermissionModes("ws-1")

	// Assert.
	if ok {
		t.Fatal("PermissionModes answered a set no picker served")
	}
}
