package sidebar_test

import (
	"sync"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
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
func quietResolver(t *testing.T) (sidebar.Resolver, *viewquiet.Gate, *skipCounter, func() []ids.WorkspaceID, *dlog.TestSurfaces) {
	t.Helper()
	counter := &skipCounter{skipped: map[string]int{}}
	gate := viewquiet.New(counter)
	var mu sync.Mutex
	var changes []ids.WorkspaceID
	r, surfaces := newResolver(t, sidebar.WithQuiet(gate),
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
	}, surfaces
}

func TestAQuietRosterPublishesNothingButStillReportsTheStatusChange(t *testing.T) {
	// Arrange
	r, gate, counter, changes, _ := quietResolver(t)
	before := latestPush(t, r).GetRevision()
	gate.Set(true)

	// Act: the arm moves from ready to submitting while the lid is closed.
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})

	// Assert
	if got := latestPush(t, r).GetRevision(); got != before {
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
	r, gate, _, changes, _ := quietResolver(t)
	before := latestPush(t, r).GetRevision()
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
	r, gate, _, _, _ := quietResolver(t)
	before := latestPush(t, r).GetRevision()
	gate.Set(true)

	// Act
	gate.Set(false)

	// Assert
	if got := latestPush(t, r).GetRevision(); got != before {
		t.Fatalf("revision = %d, want %d: nothing moved", got, before)
	}
}

func TestAQuietRosterPublishesARowMovingOntoADepartureArm(t *testing.T) {
	tests := []struct {
		name  string
		merge []string
		want  string
	}{
		{name: "a merge starting", merge: []string{"queued", "merging"}, want: "merging"},
		{name: "a merge landing", merge: []string{"queued", "merging", "merged"}, want: "merged"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: every merge state before the last is set while the lid is open.
			r, gate, _, _, surfaces := quietResolver(t)
			for _, state := range tt.merge[:len(tt.merge)-1] {
				r.SetMerge(theWS, mergeStates[state])
			}
			before := latestPush(t, r).GetRevision()
			gate.Set(true)

			// Act
			r.SetMerge(theWS, mergeStates[tt.merge[len(tt.merge)-1]])

			// Assert
			if got := latestPush(t, r).GetRevision(); got != before+1 {
				t.Fatalf("revision = %d, want %d: a departure is published while quiet", got, before+1)
			}
			if got := statusName(onlyRow(t, r)); got != tt.want {
				t.Fatalf("status = %q, want %q", got, tt.want)
			}
			if _, ok := dlog.FindRecord(surfaces.Records(), dlog.RecordQuery{Level: "info", Operation: "daemon.sidebar.quiet_departure"}); !ok {
				t.Fatalf("no daemon.sidebar.quiet_departure record: %v", surfaces.Records())
			}
		})
	}
}

func TestAQuietRosterHoldsBackAMergeThatHasNotStarted(t *testing.T) {
	// Arrange
	r, gate, _, _, surfaces := quietResolver(t)
	before := latestPush(t, r).GetRevision()
	gate.Set(true)

	// Act: the merge is only queued, so no worktree is leaving yet.
	r.SetMerge(theWS, mergeStates["queued"])

	// Assert
	if got := latestPush(t, r).GetRevision(); got != before {
		t.Fatalf("revision = %d, want %d: a queued merge is not a departure", got, before)
	}
	if _, ok := dlog.FindRecord(surfaces.Records(), dlog.RecordQuery{Operation: "daemon.sidebar.quiet_departure"}); ok {
		t.Fatal("a queued merge was recorded as a departure")
	}
}

func TestLeavingTheQuietStateAfterAPublishedDepartureRepublishesNothing(t *testing.T) {
	// Arrange
	r, gate, _, _, _ := quietResolver(t)
	r.SetMerge(theWS, mergeStates["queued"])
	gate.Set(true)
	r.SetMerge(theWS, mergeStates["merging"])
	published := latestPush(t, r).GetRevision()

	// Act
	gate.Set(false)

	// Assert
	if got := latestPush(t, r).GetRevision(); got != published {
		t.Fatalf("revision = %d, want %d: the departure's roster was already current", got, published)
	}
}

func TestDepartureArmNamesTheArmsAMergeRemovesAWorktreeUnder(t *testing.T) {
	tests := []struct {
		arm  string
		want bool
	}{
		{arm: "merging", want: true},
		{arm: "merged", want: true},
		{arm: "merge_queued", want: false},
		{arm: "merge_failed", want: false},
		{arm: "thinking", want: false},
	}
	for _, tt := range tests {
		t.Run(tt.arm, func(t *testing.T) {
			// Act
			got := sidebar.DepartureArm(tt.arm)

			// Assert
			if got != tt.want {
				t.Fatalf("DepartureArm(%q) = %v, want %v", tt.arm, got, tt.want)
			}
		})
	}
}

func TestAQuietRosterPublishesAChangedMergedSet(t *testing.T) {
	tests := []struct {
		name      string
		after     sidebar.Registry
		wantMoved bool
	}{
		{name: "a merged workspace nuked while quiet is published", after: registry(workspace(string(theWS), "one")), wantMoved: true},
		{name: "a change that leaves the merged set alone waits for the lid", after: registry(workspace(string(theWS), "renamed"), mergedAt("m", time.Hour)), wantMoved: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: a merged workspace beside the live one, then the lid
			// closes.
			r, gate, _, _, surfaces := quietResolver(t)
			r.SetRegistry(registry(workspace(string(theWS), "one"), mergedAt("m", time.Hour)))
			before := latestPush(t, r).GetRevision()
			gate.Set(true)

			// Act.
			r.SetRegistry(tt.after)

			// Assert: a changed set is published while quiet, so a page and
			// the summary it is drawn beside never name two sets.
			if moved := latestPush(t, r).GetRevision() != before; moved != tt.wantMoved {
				t.Fatalf("published = %v, want %v", moved, tt.wantMoved)
			}
			_, recorded := dlog.FindRecord(surfaces.Records(), dlog.RecordQuery{Level: "info", Operation: "daemon.sidebar.quiet_merged_set"})
			if recorded != tt.wantMoved {
				t.Fatalf("quiet_merged_set recorded = %v, want %v", recorded, tt.wantMoved)
			}
			if tt.wantMoved {
				page, err := r.MergedPage(sidebar.MergedPageQuery{Order: sidebar.MergedByMerge})
				if err != nil || page.Revision != latestPush(t, r).GetRecentlyMerged().GetMergedRevision().GetRevision() || len(page.Rows) != 0 {
					t.Fatalf("page = %v (%v), want no rows under the published summary's token", page, err)
				}
			}
		})
	}
}
