package turnlatch

import (
	"testing"

	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// isClosed reports whether LATCH is closed.
func isClosed(latch <-chan struct{}) bool {
	select {
	case <-latch:
		return true
	default:
		return false
	}
}

func TestALatchIsReleasedOnlyByItsWorkspacesTurnEndOrClose(t *testing.T) {
	const ws, other = ids.WorkspaceID("ws"), ids.WorkspaceID("other")
	tests := []struct {
		name     string
		edge     func(l *Latches)
		released bool
	}{
		{name: "a turn ending", edge: func(l *Latches) { l.TurnEnded(ws) }, released: true},
		{name: "turns an adoption found ended", edge: func(l *Latches) { l.TurnEndedUnobserved(ws) }, released: true},
		{name: "the workspace closing", edge: func(l *Latches) { l.Closed(wsm.Workspace{ID: ws}) }, released: true},
		{name: "a turn starting", edge: func(l *Latches) { l.TurnStarted(ws) }},
		{name: "an ask opening", edge: func(l *Latches) { l.NeedsInput(ws) }},
		{name: "a landing", edge: func(l *Latches) { l.Landed(ws) }},
		{name: "another workspace's turn ending", edge: func(l *Latches) { l.TurnEnded(other) }},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			l := New()
			first, second := l.Next(ws), l.Next(ws)

			// Act
			tc.edge(l)

			// Assert
			if isClosed(first) != tc.released || isClosed(second) != tc.released {
				t.Fatalf("released = %v, %v, want both %v", isClosed(first), isClosed(second), tc.released)
			}
		})
	}
}

func TestALatchTakenAfterAnEndWaitsForTheNextOne(t *testing.T) {
	// Arrange
	const ws = ids.WorkspaceID("ws")
	l := New()
	l.TurnEnded(ws)

	// Act
	latch := l.Next(ws)

	// Assert
	if isClosed(latch) {
		t.Fatal("a latch taken after a turn end was already released")
	}
	l.TurnEnded(ws)
	if !isClosed(latch) {
		t.Fatal("the next turn end did not release the latch")
	}
}
