package workspace

import (
	"context"
	"testing"

	"claude-repld/internal/sessionwatcher"
)

func TestNotifyRaisesADesktopBanner(t *testing.T) {
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
	want := raisedBanner{WS: "w1", Kind: "agent_addressed", Text: "the agent is waiting on you"}
	if len(f.banners.raised) != 1 || f.banners.raised[0] != want {
		t.Fatalf("raised banners = %+v, want [%+v]", f.banners.raised, want)
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

func TestNotifyRefusesAnUntypedNotification(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	err := f.verbs.Notify(context.Background(), "w1", sessionwatcher.HostNotification{Text: "something"})

	// Assert.
	asRefusal(t, err, ArmUnservedAnswer)
	if len(f.banners.raised) != 0 {
		t.Fatalf("a refused notification raised %d banners", len(f.banners.raised))
	}
}

func TestNotifyRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange.
	f := newFixture(t)

	// Act.
	err := f.verbs.Notify(context.Background(), "nope", sessionwatcher.HostNotification{Kind: "agent_addressed"})

	// Assert.
	asRefusal(t, err, ArmUnknownWorkspace)
	if len(f.banners.raised) != 0 {
		t.Fatalf("a refused notification raised %d banners", len(f.banners.raised))
	}
}

func TestAsksSettledClearsTheAttentionMarker(t *testing.T) {
	// Arrange: an ask raised the marker.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	if err := f.verbs.Notify(context.Background(), "w1", sessionwatcher.HostNotification{
		Kind: "permission_requested", ToolName: "Bash",
	}); err != nil {
		t.Fatalf("Notify: %v", err)
	}

	// Act.
	if err := f.verbs.AsksSettled(context.Background(), "w1"); err != nil {
		t.Fatalf("AsksSettled: %v", err)
	}

	// Assert.
	if f.db.attention["w1"] {
		t.Fatal("AsksSettled() left the attention marker standing")
	}
}

func TestAsksSettledRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange.
	f := newFixture(t)

	// Act.
	err := f.verbs.AsksSettled(context.Background(), "nope")

	// Assert.
	asRefusal(t, err, ArmUnknownWorkspace)
}
