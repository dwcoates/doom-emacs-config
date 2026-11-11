package sidebar_test

import (
	"sync"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/shimclient"
	"agent-repld/internal/viewquiet"
)

type skipCounter struct {
	mu      sync.Mutex
	skipped map[string]int
}

func (s *skipCounter) CountViewRenderSkipped(view string) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.skipped[view]++
}

// quietResolver is a live one-workspace roster behind a quiet gate, recording
// every status change it reports.
func quietResolver(t *testing.T) (sidebar.Resolver, *viewquiet.Gate, *skipCounter, func() []ids.WorkspaceID) {
	t.Helper()
	counter := &skipCounter{skipped: map[string]int{}}
	gate := viewquiet.New(counter)
	var mu sync.Mutex
	var changes []ids.WorkspaceID
	r, _ := newResolver(t, sidebar.WithQuiet(gate),
		sidebar.WithStatusChangeSink(func(ws ids.WorkspaceID, _ time.Time) {
			mu.Lock()
			defer mu.Unlock()
			changes = append(changes, ws)
		}))
	r.SetRegistry(registry(workspace(string(theWS), "one")))
	r.OnLink(theWS, shimclient.LinkConnected)
	r.OnSessionStarted(theWS, &conversationv1.SessionStarted{VendorSessionId: "vendor-1"})
	mu.Lock()
	changes = nil
	mu.Unlock()
	return r, gate, counter, func() []ids.WorkspaceID {
		mu.Lock()
		defer mu.Unlock()
		return append([]ids.WorkspaceID(nil), changes...)
	}
}

func TestAQuietRosterPublishesNothingButStillReportsTheStatusChange(t *testing.T) {
	// Arrange
	r, gate, counter, changes := quietResolver(t)
	before := latest(t, r).GetRevision()
	gate.Set(true)

	// Act: the arm moves from ready to submitting while the lid is closed.
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})

	// Assert
	if got := latest(t, r).GetRevision(); got != before {
		t.Fatalf("revision = %d, want %d: a quiet roster publishes nothing", got, before)
	}
	if got := changes(); len(got) != 1 || got[0] != theWS {
		t.Fatalf("status changes = %v, want the one move of %s", got, theWS)
	}
	if counter.skipped[sidebar.QuietViewName] == 0 {
		t.Fatal("no skipped render was counted")
	}
}

func TestLeavingTheQuietStatePublishesTheCurrentRosterOnce(t *testing.T) {
	// Arrange
	r, gate, _, changes := quietResolver(t)
	before := latest(t, r).GetRevision()
	gate.Set(true)
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})
	r.SetSummary(theWS, "a summary while the lid is closed")
	updates := subscribe(t, r)
	<-updates // the topic's replay of the last roster published

	// Act
	gate.Set(false)

	// Assert
	roster := <-updates
	if roster.GetRevision() != before+1 {
		t.Fatalf("revision = %d, want one roster after %d", roster.GetRevision(), before)
	}
	if got := statusName(onlyRow(t, r)); got != "submitting" {
		t.Fatalf("status = %q, want the current submitting", got)
	}
	select {
	case extra := <-updates:
		t.Fatalf("a second roster was published: %v", extra.GetRevision())
	default:
	}
	if got := changes(); len(got) != 1 {
		t.Fatalf("status changes = %v, want the one move, not repeated by the resync", got)
	}
}

func TestLeavingAQuietStateThatSkippedNothingPublishesNothing(t *testing.T) {
	// Arrange
	r, gate, _, _ := quietResolver(t)
	before := latest(t, r).GetRevision()
	gate.Set(true)

	// Act
	gate.Set(false)

	// Assert
	if got := latest(t, r).GetRevision(); got != before {
		t.Fatalf("revision = %d, want %d: nothing moved", got, before)
	}
}
