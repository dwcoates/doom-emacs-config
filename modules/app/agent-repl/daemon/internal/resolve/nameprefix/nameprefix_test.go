package nameprefix

import (
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/wsm"
)

func priorityPtr(p wsm.Priority) *wsm.Priority { return &p }

func TestOfDrawsThePriorityIconForEachLevel(t *testing.T) {
	cases := []struct {
		name  string
		level wsm.Priority
		arm   string
	}{
		{name: "p05", level: wsm.PriorityP05, arm: "p05"},
		{name: "p1", level: wsm.PriorityP1, arm: "p1"},
		{name: "p2", level: wsm.PriorityP2, arm: "p2"},
		{name: "p3", level: wsm.PriorityP3, arm: "p3"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			rec := wsm.Workspace{Priority: priorityPtr(tc.level)}

			// Act
			got := Of(rec)

			// Assert
			level := got.GetPriority().GetLevel()
			if level == nil {
				t.Fatalf("Of(%v) drew no priority icon: %v", tc.level, got)
			}
			if arm := armName(level); arm != tc.arm {
				t.Fatalf("Of(%v) drew arm %q, want %q", tc.level, arm, tc.arm)
			}
		})
	}
}

func TestOfIsUnsetForAnUnprioritizedWorkspace(t *testing.T) {
	// Arrange
	rec := wsm.Workspace{}

	// Act
	got := Of(rec)

	// Assert: no mark is no prefix, never an empty one.
	if got != nil {
		t.Fatalf("Of(unprioritized) = %v, want nil", got)
	}
}

func TestPriorityPanicsOnALevelWsmNeverRecords(t *testing.T) {
	// Arrange
	defer func() {
		// Assert
		if recover() == nil {
			t.Fatal("Priority(99) drew an icon instead of failing hard")
		}
	}()

	// Act
	Priority(wsm.Priority(99))
}

func armName(level any) string {
	switch level.(type) {
	case *frontendv1.WorkspaceNamePriority_P05:
		return "p05"
	case *frontendv1.WorkspaceNamePriority_P1:
		return "p1"
	case *frontendv1.WorkspaceNamePriority_P2:
		return "p2"
	case *frontendv1.WorkspaceNamePriority_P3:
		return "p3"
	}
	return ""
}

func TestOfDrawsTheEmojiAckGlyph(t *testing.T) {
	// Arrange
	rec := wsm.Workspace{EmojiAck: true}

	// Act
	got := Of(rec)

	// Assert: the thumbs up alone, no priority icon.
	if got.GetEmojiAck().GetText() != "👍" || got.GetPriority() != nil {
		t.Fatalf("Of(emoji-ack) = %v, want the thumbs up alone", got)
	}
}

func TestOfDrawsBothMarksTogether(t *testing.T) {
	// Arrange: the marks are independent, so a workspace may wear both.
	rec := wsm.Workspace{Priority: priorityPtr(wsm.PriorityP1), EmojiAck: true}

	// Act
	got := Of(rec)

	// Assert
	if got.GetPriority().GetP1() == nil || got.GetEmojiAck().GetText() != EmojiAck {
		t.Fatalf("Of(p1, emoji-ack) = %v, want the p1 icon and the thumbs up", got)
	}
}
