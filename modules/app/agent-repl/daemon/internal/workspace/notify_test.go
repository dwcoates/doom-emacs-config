package workspace

import (
	"context"
	"testing"

	"claude-repld/internal/sessionwatcher"
)

func TestNotifyRelaysOntoTheHostStream(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	err := f.verbs.Notify(context.Background(), "w1", sessionwatcher.HostNotification{
		Kind: "agent_addressed", Text: "the agent is waiting on you",
	})

	// Assert.
	if err != nil {
		t.Fatalf("Notify: %v", err)
	}
	if len(f.host.notes) != 1 || f.host.notes[0].Kind != "agent_addressed" {
		t.Fatalf("relayed notifications = %+v, want one agent_addressed", f.host.notes)
	}
}

func TestNotifySetsTheAttentionMarker(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Notify(context.Background(), "w1", sessionwatcher.HostNotification{
		Kind: "permission_requested", ToolName: "Bash",
	}); err != nil {
		t.Fatalf("Notify: %v", err)
	}

	// Assert.
	if !f.db.attention["w1"] {
		t.Fatal("Notify() did not set the attention marker")
	}
}

func TestNotifyCarriesTheNamedTool(t *testing.T) {
	// Arrange: a permission notification names the tool it gates.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Notify(context.Background(), "w1", sessionwatcher.HostNotification{
		Kind: "permission_requested", ToolName: "Bash",
	}); err != nil {
		t.Fatalf("Notify: %v", err)
	}

	// Assert.
	if f.host.notes[0].Tool != "Bash" {
		t.Fatalf("relayed tool = %q, want Bash", f.host.notes[0].Tool)
	}
}

func TestNotifyRefusesAnUntypedNotification(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	err := f.verbs.Notify(context.Background(), "w1", sessionwatcher.HostNotification{Text: "something"})

	// Assert.
	asRefusal(t, err, ArmUnservedAnswer)
}

func TestNotifyRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange.
	f := newFixture(t)

	// Act.
	err := f.verbs.Notify(context.Background(), "nope", sessionwatcher.HostNotification{Kind: "agent_addressed"})

	// Assert.
	asRefusal(t, err, ArmUnknownWorkspace)
}
