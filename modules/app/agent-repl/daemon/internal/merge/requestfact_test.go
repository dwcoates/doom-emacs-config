package merge

import (
	"context"
	"errors"
	"slices"
	"testing"

	"agent-repld/internal/dlog"
)

func TestTheRosterIsToldOfTheRequestFact(t *testing.T) {
	cases := []struct {
		name string
		act  func(t *testing.T, h *harness)
		want []bool
	}{
		{
			name: "a request recorded while its turn runs is raised",
			act: func(t *testing.T, h *harness) {
				h.inFlight = "turn-asking"
				if err := h.request(t, ownBranch, RequestedByAgent); err != nil {
					t.Fatalf("Enqueue: %v", err)
				}
			},
			want: []bool{true},
		},
		{
			name: "a request the store cannot record is not raised",
			act: func(t *testing.T, h *harness) {
				h.db.enqueueErr = errors.New("database is locked")
				if err := h.request(t, ownBranch, RequestedByAgent); err == nil {
					t.Fatal("Enqueue succeeded, want the store's failure")
				}
			},
			want: nil,
		},
		{
			name: "an evicted request is raised then lowered",
			act: func(t *testing.T, h *harness) {
				h.inFlight = "turn-asking"
				if err := h.request(t, ownBranch, RequestedByAgent); err != nil {
					t.Fatalf("Enqueue: %v", err)
				}
				if err := h.o.Evict(context.Background(), theWorkspace); err != nil {
					t.Fatalf("Evict: %v", err)
				}
			},
			want: []bool{true, false},
		},
		{
			name: "a request recovered at boot is raised",
			act: func(t *testing.T, h *harness) {
				if err := h.db.RequestMerge(context.Background(), h.repoKey(), theWorkspace, ownBranch, h.clock()); err != nil {
					t.Fatalf("RequestMerge: %v", err)
				}
				if err := h.o.Recover(context.Background()); err != nil {
					t.Fatalf("Recover: %v", err)
				}
			},
			want: []bool{true},
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			tc.act(t, h)

			// Assert.
			if got := h.sidebar.requestedSeen(); !slices.Equal(got, tc.want) {
				t.Fatalf("request facts = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestTheRequestFactIsRaisedBeforeTheRequestingTurnEnds(t *testing.T) {
	// Arrange: the requesting turn is in flight and never ends here.
	h := newHarness(t)
	h.inFlight = "turn-asking"
	h.awaitGate = make(chan struct{})

	// Act.
	if err := h.request(t, ownBranch, RequestedByAgent); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}

	// Assert: raised while the turn still runs, with no merge fact published.
	if got := h.sidebar.requestedSeen(); !slices.Equal(got, []bool{true}) {
		t.Fatalf("request facts = %v, want [true] before the turn ends", got)
	}
	if len(h.sidebar.facts) != 0 {
		t.Fatalf("merge facts were published before the turn ended: %+v", h.sidebar.facts)
	}
}

func TestRecoveringAMergeWorkspacesEntryRaisesItsRequestersRequestFact(t *testing.T) {
	cases := []struct {
		name   string
		queued bool
	}{
		{name: "an entry still requested", queued: false},
		{name: "an entry in line", queued: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the requester's detached merge was handed off before the
			// restart, so the only entry is the merge workspace's.
			h := newHarness(t)
			h.registerMergeWorkspace(theWorkspace, "request-1", "feature")
			ctx := context.Background()
			if err := h.db.RequestMerge(ctx, h.repoKey(), theMergeWorkspace, ownBranch, h.clock()); err != nil {
				t.Fatalf("RequestMerge: %v", err)
			}
			if tc.queued {
				if _, err := h.db.QueueMerge(ctx, h.repoKey(), theMergeWorkspace, "ledger-1"); err != nil {
					t.Fatalf("QueueMerge: %v", err)
				}
			}

			// Act.
			if err := h.o.Recover(ctx); err != nil {
				t.Fatalf("Recover: %v", err)
			}

			// Assert.
			if got, want := h.sidebar.requestedSeenFor(theWorkspace), []bool{true}; !slices.Equal(got, want) {
				t.Fatalf("requester's request facts = %v, want %v", got, want)
			}
			if !recordedRequesterRaise(h) {
				t.Fatal("the requester's re-raised request fact was not recorded at INFO")
			}
		})
	}
}

// recordedRequesterRaise reports an INFO daemon.merge.recover record naming
// the harness workspace as a recovered merge workspace's requester.
func recordedRequesterRaise(h *harness) bool {
	for _, rec := range h.logs.Records() {
		if rec.Level == dlog.LevelInfo && rec.Operation == "daemon.merge.recover" &&
			rec.Context["requester"] == string(theWorkspace) && rec.Context["workspace"] == string(theMergeWorkspace) {
			return true
		}
	}
	return false
}

func TestRecoveringAMergeWorkspacesEntryThatCannotGoBackEndsItsRequestersMerge(t *testing.T) {
	// Arrange: the merge workspace's entry waits in line, but its geometry is
	// gone, so the restart abandons it.
	h := newHarness(t)
	h.registerMergeWorkspace(theWorkspace, "request-1", "feature")
	ctx := context.Background()
	if err := h.db.RequestMerge(ctx, h.repoKey(), theMergeWorkspace, ownBranch, h.clock()); err != nil {
		t.Fatalf("RequestMerge: %v", err)
	}
	if _, err := h.db.QueueMerge(ctx, h.repoKey(), theMergeWorkspace, "ledger-1"); err != nil {
		t.Fatalf("QueueMerge: %v", err)
	}
	h.db.mu.Lock()
	delete(h.db.jobs, theMergeWorkspace)
	h.db.mu.Unlock()

	// Act.
	if err := h.o.Recover(ctx); err != nil {
		t.Fatalf("Recover: %v", err)
	}

	// Assert: raised for the recovery, then ended without landing.
	if got, want := h.sidebar.requestedSeenFor(theWorkspace), []bool{true}; !slices.Equal(got, want) {
		t.Fatalf("requester's request facts = %v, want %v", got, want)
	}
	if got, want := h.sidebar.detachedEndsSeen(), []detachedEnd{{ws: theWorkspace, landed: false}}; !slices.Equal(got, want) {
		t.Fatalf("detached ends = %+v, want %+v", got, want)
	}
}

func TestRecoveringAnInPlaceEntryRaisesNoOtherWorkspacesRequestFact(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ctx := context.Background()
	if err := h.db.RequestMerge(ctx, h.repoKey(), theWorkspace, ownBranch, h.clock()); err != nil {
		t.Fatalf("RequestMerge: %v", err)
	}
	if _, err := h.db.QueueMerge(ctx, h.repoKey(), theWorkspace, "ledger-1"); err != nil {
		t.Fatalf("QueueMerge: %v", err)
	}

	// Act.
	if err := h.o.Recover(ctx); err != nil {
		t.Fatalf("Recover: %v", err)
	}

	// Assert.
	if got := h.sidebar.requestedSeen(); len(got) != 0 {
		t.Fatalf("request facts = %v, want none for an in-place merge in line", got)
	}
}
