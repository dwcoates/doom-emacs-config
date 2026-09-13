package workspace

import (
	"context"
	"errors"
	"testing"

	"claude-repld/internal/account"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// forkFixture arranges a parent with a conversation to fork and answers the
// spec that forks it. The parent's prompt rows are stated directly, because
// what this file is about is what CROSSES to the child.
func forkFixture(t *testing.T, parentPrompts []wsm.PortedPrompt) (*fixture, ids.WorkspaceID, CreateSpec) {
	t.Helper()
	f := newFixture(t)
	parent := f.workspace("parent", t.TempDir())
	f.db.sessions[parent.ID] = wsm.Session{Workspace: parent.ID, VendorSessionID: "vendor-1"}
	f.account.transcript = account.Transcript{Path: "/transcripts/vendor-1.jsonl", ConfigDir: "/roots/default"}
	f.db.conversations = map[ids.WorkspaceID][]wsm.PortedPrompt{parent.ID: parentPrompts}
	spec := standardSpec(t, f)
	id := parent.ID
	spec.ForkFrom = &id
	return f, parent.ID, spec
}

// TestCreateForkPortsTheParentsPromptRows covers the defect itself: the
// forked feed showed the parent's settled ANSWER and not the parent's
// QUESTION, because the transcript was the only thing a fork carried.
func TestCreateForkPortsTheParentsPromptRows(t *testing.T) {
	// Arrange.
	f, _, spec := forkFixture(t, []wsm.PortedPrompt{
		{Turn: "turn-1", Ordinal: 0, Text: "what is 2+2", Origin: "webapp"},
	})

	// Act.
	created, err := f.verbs.Create(context.Background(), spec)
	if err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert.
	ported := f.db.portedPrompts[created.ID]
	if len(ported) != 1 || ported[0].Text != "what is 2+2" {
		t.Fatalf("ported prompts = %+v, want the parent's question carried to the child", ported)
	}
}

// TestCreateForkPortsThePromptRowsUnderTheMappedIds covers the identity rule:
// the prompt rows are re-minted under the SAME mapping the transcript was
// ported under, so the child's questions and its ported answers name one
// another exactly as the parent's did.
func TestCreateForkPortsThePromptRowsUnderTheMappedIds(t *testing.T) {
	// Arrange.
	f, _, spec := forkFixture(t, []wsm.PortedPrompt{
		{Turn: "turn-1", Ordinal: 0, Text: "what is 2+2", Origin: "webapp"},
	})
	f.account.mint = func(old string) string { return "mapped:" + old }

	// Act.
	created, err := f.verbs.Create(context.Background(), spec)
	if err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert.
	ported := f.db.portedPrompts[created.ID]
	if len(ported) != 1 || string(ported[0].Turn) != "mapped:turn-1" {
		t.Fatalf("ported prompt turn = %+v, want the fork mapping's id", ported)
	}
}

// TestCreateForkLeavesTheParentsOwnRowsAlone covers the fork's whole point:
// the parent keeps its conversation, so nothing is written back to it.
func TestCreateForkLeavesTheParentsOwnRowsAlone(t *testing.T) {
	// Arrange.
	f, parentID, spec := forkFixture(t, []wsm.PortedPrompt{
		{Turn: "turn-1", Ordinal: 0, Text: "what is 2+2", Origin: "webapp"},
	})

	// Act.
	if _, err := f.verbs.Create(context.Background(), spec); err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert.
	if rows, ok := f.db.portedPrompts[parentID]; ok {
		t.Fatalf("the parent's ported prompts = %+v, want the parent untouched by its own fork", rows)
	}
}

// TestCreateForkOfAWorkspaceWithNoPromptsPortsNothing covers the empty
// conversation: a parent that was asked nothing has no question to carry, and
// the fork writes no row rather than an empty one.
func TestCreateForkOfAWorkspaceWithNoPromptsPortsNothing(t *testing.T) {
	// Arrange.
	f, _, spec := forkFixture(t, nil)

	// Act.
	created, err := f.verbs.Create(context.Background(), spec)
	if err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert.
	if rows, ok := f.db.portedPrompts[created.ID]; ok {
		t.Fatalf("ported prompts = %+v, want no row written for a parent with no prompts", rows)
	}
}

// TestCreateForkFailsWhenTheConversationCannotBeRead covers the refusal: a
// half-ported conversation is the defect wearing a different face, so the
// create fails rather than proceeding with part of the history.
func TestCreateForkFailsWhenTheConversationCannotBeRead(t *testing.T) {
	// Arrange.
	f, _, spec := forkFixture(t, nil)
	f.db.conversationErr = errors.New("the state database is unreadable")

	// Act.
	_, err := f.verbs.Create(context.Background(), spec)

	// Assert.
	if err == nil {
		t.Fatal("Create() = nil error, want the unreadable conversation surfaced")
	}
}

// TestCreateForkFailsWhenThePortedRowsCannotBeWritten covers the other half of
// the same refusal, on the write.
func TestCreateForkFailsWhenThePortedRowsCannotBeWritten(t *testing.T) {
	// Arrange.
	f, _, spec := forkFixture(t, []wsm.PortedPrompt{
		{Turn: "turn-1", Ordinal: 0, Text: "what is 2+2", Origin: "webapp"},
	})
	f.db.putPortedErr = errors.New("the state database is unwritable")

	// Act.
	_, err := f.verbs.Create(context.Background(), spec)

	// Assert.
	if err == nil {
		t.Fatal("Create() = nil error, want the unwritable ported conversation surfaced")
	}
}
