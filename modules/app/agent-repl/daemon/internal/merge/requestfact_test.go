package merge

import (
	"context"
	"errors"
	"slices"
	"testing"
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
