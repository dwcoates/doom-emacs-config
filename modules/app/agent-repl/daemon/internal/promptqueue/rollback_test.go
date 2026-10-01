package promptqueue

import (
	"context"
	"errors"
	"testing"
	"time"

	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// since is the fixed instant every TestHeldSince case measures against.
var since = instant

// TestHeldSince covers heldSince's three filters: a held session act is
// skipped regardless of when it queued, a prompt queued strictly before the
// cut is skipped, and the survivors come back in their original queue order.
func TestHeldSince(t *testing.T) {
	tests := []struct {
		name     string
		standing []wsm.HeldPrompt
		want     []ids.TurnID
	}{
		{
			name: "a session act held prompt is skipped even though queued after since",
			standing: []wsm.HeldPrompt{
				{Turn: "act1", QueuedAt: since.Add(time.Minute), Act: &wsm.HeldAct{Kind: wsm.ActModel, Value: "model-x"}},
			},
			want: nil,
		},
		{
			name: "a prompt queued strictly before since is skipped",
			standing: []wsm.HeldPrompt{
				{Turn: "t0", QueuedAt: since.Add(-time.Minute)},
			},
			want: nil,
		},
		{
			name: "survivors are kept in their original queue order, skips interleaved",
			standing: []wsm.HeldPrompt{
				{Turn: "t0", QueuedAt: since.Add(-time.Minute)},                                                     // before since: skipped
				{Turn: "act1", QueuedAt: since.Add(time.Minute), Act: &wsm.HeldAct{Kind: wsm.ActModel, Value: "m"}}, // session act: skipped
				{Turn: "t1", QueuedAt: since},                                                                       // at since: kept
				{Turn: "t2", QueuedAt: since.Add(2 * time.Minute)},                                                  // after since: kept
			},
			want: []ids.TurnID{"t1", "t2"},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: tt.standing is the arrangement.
			// Act.
			got := heldSince(tt.standing, since)
			// Assert.
			if !sameTurns(got, tt.want) {
				t.Fatalf("heldSince = %v, want %v", got, tt.want)
			}
		})
	}
}

// seedHeld parks a standing held prompt directly in the fake DB, queued at
// WHEN, bypassing Submit and classification: RollBack only reads and
// tombstones held prompts, so the harness does not need a running turn or a
// judged verdict behind it.
func seedHeld(t *testing.T, h *harness, turn ids.TurnID, when time.Time) {
	t.Helper()
	if err := h.db.PutHeldPrompt(context.Background(), wsm.HeldPrompt{
		Workspace: theWorkspace,
		Turn:      turn,
		Said:      userSaid("rolled-back prompt"),
		Origin:    "PROMPT_ORIGIN_USER_SENT",
		QueuedAt:  when,
	}); err != nil {
		t.Fatalf("seedHeld(%s): %v", turn, err)
	}
}

func TestRollBackSuccess(t *testing.T) {
	// Arrange: two prompts queued at or after since, a perform that succeeds.
	h := newHarness(t)
	seedHeld(t, h, "t1", since)
	seedHeld(t, h, "t2", since.Add(time.Minute))
	before := h.holds.pushCount()
	calls := 0
	perform := func(context.Context) error {
		calls++
		return nil
	}

	// Act.
	err := h.q.RollBack(context.Background(), theWorkspace, since, []ids.TurnID{"t1", "t2"}, perform)

	// Assert.
	if err != nil {
		t.Fatalf("RollBack: %v", err)
	}
	if calls != 1 {
		t.Fatalf("perform was called %d times, want exactly once", calls)
	}
	for _, turn := range []ids.TurnID{"t1", "t2"} {
		tomb := h.db.hold(turn).Tombstone
		if tomb == nil || tomb.Kind != tombstoneRolledBack {
			t.Fatalf("tombstone(%s) = %+v, want a rolled_back tombstone", turn, tomb)
		}
	}
	if h.holds.pushCount() <= before {
		t.Fatal("the tray must be republished after a rollback")
	}
	if !logged(h.log.Records(), "info", opRollBack, "the rollback was performed and the held prompts queued since were dropped") {
		t.Fatalf("records = %+v, want the rollback's success logged at info", h.log.Records())
	}
}

// TestRollBackUnderDrainLock proves RollBack runs PERFORM while it still owns
// the workspace's drain lock: a concurrent Submit, which takes the same lock
// before doing anything else, must not complete until perform returns.
func TestRollBackUnderDrainLock(t *testing.T) {
	// Arrange: one held prompt to roll back, and a perform that signals it
	// started, then blocks on the test's release.
	h := newHarness(t)
	seedHeld(t, h, "t1", since)
	started := make(chan struct{})
	proceed := make(chan struct{})
	perform := func(context.Context) error {
		close(started)
		<-proceed
		return nil
	}

	rollBackDone := make(chan error, 1)
	go func() {
		rollBackDone <- h.q.RollBack(context.Background(), theWorkspace, since, []ids.TurnID{"t1"}, perform)
	}()

	// Act: wait for perform to be running (and so the drain lock to be held),
	// then launch a concurrent Submit that takes the same lock.
	select {
	case <-started:
	case <-time.After(bounceStartBound):
		t.Fatal("RollBack's perform never started")
	}
	submitDone := make(chan error, 1)
	go func() {
		_, err := h.q.Submit(context.Background(), submission("t-other", "a concurrent submission"))
		submitDone <- err
	}()

	// Assert: the concurrent Submit has not completed while perform still
	// holds the drain lock. This is a bounded negative check, not a sleep used
	// for synchronization: the rendezvous above already proved perform is in
	// flight, and nothing legitimate completes a Submit in this window unless
	// the lock was not actually held.
	select {
	case got := <-submitDone:
		t.Fatalf("Submit completed (%v) while RollBack's perform still held the drain lock", got)
	case <-time.After(50 * time.Millisecond):
	}

	// Release perform and require both calls to finish promptly.
	close(proceed)
	select {
	case err := <-rollBackDone:
		if err != nil {
			t.Fatalf("RollBack: %v", err)
		}
	case <-time.After(bounceStartBound):
		t.Fatal("RollBack never returned after perform was released")
	}
	select {
	case err := <-submitDone:
		if err != nil {
			t.Fatalf("Submit: %v", err)
		}
	case <-time.After(bounceStartBound):
		t.Fatal("the concurrent Submit never completed once the drain lock was released")
	}
}

func TestRollBackHoldsChangedRefusesWithoutPerforming(t *testing.T) {
	// Arrange: t1 is planned for the rollback, but by the time it runs another
	// prompt (t2) has since queued behind it, so the live holds no longer
	// match the plan.
	h := newHarness(t)
	seedHeld(t, h, "t1", since)
	seedHeld(t, h, "t2", since.Add(time.Minute))
	performed := false
	perform := func(context.Context) error {
		performed = true
		return nil
	}

	// Act.
	err := h.q.RollBack(context.Background(), theWorkspace, since, []ids.TurnID{"t1"}, perform)

	// Assert.
	if !errors.Is(err, ErrHoldsChanged) {
		t.Fatalf("RollBack = %v, want ErrHoldsChanged", err)
	}
	if performed {
		t.Fatal("perform must never run when the held prompts changed since the rollback was planned")
	}
	if !logged(h.log.Records(), "info", opRollBack, "the rollback was refused: the held prompts changed since it was planned") {
		t.Fatalf("records = %+v, want the refusal logged at info", h.log.Records())
	}
}

func TestRollBackPerformErrorLeavesHoldsUntouched(t *testing.T) {
	// Arrange: perform (the vendor-side rollback) fails.
	h := newHarness(t)
	seedHeld(t, h, "t1", since)
	causeErr := errors.New("the vendor refused the rollback")
	perform := func(context.Context) error { return causeErr }

	// Act.
	err := h.q.RollBack(context.Background(), theWorkspace, since, []ids.TurnID{"t1"}, perform)

	// Assert: the code returns perform's error directly, unwrapped.
	if !errors.Is(err, causeErr) {
		t.Fatalf("RollBack = %v, want the perform error returned directly", err)
	}
	if tomb := h.db.hold("t1").Tombstone; tomb != nil {
		t.Fatalf("tombstone(t1) = %+v, want the hold untouched when perform fails", tomb)
	}
	if !logged(h.log.Records(), "info", opRollBack, "the rollback was not performed; the held prompts stand") {
		t.Fatalf("records = %+v, want the perform failure logged at info", h.log.Records())
	}
}

func TestRollBackTombstoneFailureIsErrorAndSurfaces(t *testing.T) {
	// Arrange: perform succeeds, but retiring the held prompt's tombstone
	// fails durably.
	h := newHarness(t)
	seedHeld(t, h, "t1", since)
	h.db.tombstoneErr = errors.New("the database is read-only")
	perform := func(context.Context) error { return nil }

	// Act.
	err := h.q.RollBack(context.Background(), theWorkspace, since, []ids.TurnID{"t1"}, perform)

	// Assert: a failed tombstone write is surfaced, and the held prompt that
	// could not be retired stays queued: the retirement is all or nothing.
	if err == nil {
		t.Fatal("a failed tombstone write must be surfaced")
	}
	if tomb := h.db.hold("t1").Tombstone; tomb != nil {
		t.Fatalf("tombstone(t1) = %+v, want the hold still standing after the failed retirement", tomb)
	}
	if !logged(h.log.Records(), "error", opRollBack, "the held prompts the rollback dropped could not be retired; they stay queued") {
		t.Fatalf("records = %+v, want the tombstone failure logged at error", h.log.Records())
	}
}
