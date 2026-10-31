package merge

import (
	"context"
	"testing"

	"agent-repld/internal/wsm"
)

// TestEveryEndOfARunningMergeTellsTheEndObserverOnce pins that the end
// observer (Deps.Ends) hears each merge's resolution exactly once, with its
// kind, whichever way the merge left the queue.
func TestEveryEndOfARunningMergeTellsTheEndObserverOnce(t *testing.T) {
	for _, tc := range releaseCases() {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange and Act.
			h := ended(t, tc.phase, tc.res)

			// Assert.
			want := DetachedAbandoned
			if tc.phase == phaseLanded {
				want = DetachedLanded
			}
			got := h.ends.told()
			if len(got) != 1 || got[0].ws != theWorkspace || got[0].out.Kind != want {
				t.Fatalf("ends told = %+v, want one %v end of %s", got, want, theWorkspace)
			}
		})
	}
}

func TestAFailedMergeTellsTheEndObserverItsSummary(t *testing.T) {
	// Arrange: a merge whose before-action fails.
	h := newHarness(t)
	h.emacsRepo()
	h.configurePromptHooks([]string{"before"}, nil)
	h.briefs["before"] = nil
	h.turnCloses = []wsm.TurnClose{wsm.CloseFailed}
	enqueue(t, h)

	// Act.
	_ = h.admit(context.Background())

	// Assert.
	got := h.ends.told()
	if len(got) != 1 || got[0].out.Kind != DetachedFailed || got[0].out.Summary == "" {
		t.Fatalf("ends told = %+v, want one failed end with its summary", got)
	}
}

func TestAMergeStillInLineTellsTheEndObserverNothing(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	enqueue(t, h)

	// Assert.
	if got := h.ends.told(); len(got) != 0 {
		t.Fatalf("ends told = %+v, want none while the merge waits", got)
	}
}
