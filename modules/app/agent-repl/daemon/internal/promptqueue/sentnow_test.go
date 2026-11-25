package promptqueue

import (
	"testing"
)

func TestNoteSentNowStandingClearsTheReplaysShortcut(t *testing.T) {
	// Arrange: the last replay found no "Send now" waiting.
	h := newHarness(t)
	h.q.mu.Lock()
	h.q.stateLocked(theWorkspace).sentNowNone = true
	h.q.mu.Unlock()

	// Act
	h.q.noteSentNowStanding(theWorkspace)

	// Assert
	h.q.mu.Lock()
	defer h.q.mu.Unlock()
	if h.q.stateLocked(theWorkspace).sentNowNone {
		t.Fatal("sentNowNone = true, want the next replay to take the mark")
	}
}
