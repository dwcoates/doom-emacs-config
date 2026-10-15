package merge

import (
	"context"
	"errors"
	"strings"
	"testing"

	"agent-repld/internal/wsm"
)

func TestAPausedQueueAdmitsNothing(t *testing.T) {
	// Arrange.
	c := newChain(t)
	enqueue(t, c.h)
	c.h.db.paused[c.h.repoKey()] = true

	// Act.
	if err := c.h.admit(context.Background()); err != nil {
		t.Fatalf("the pump ended on: %v", err)
	}

	// Assert.
	if got := c.runner.ran(c.dirs[theWorkspace]); got != 0 {
		t.Fatalf("the gate ran %d times on a paused queue, want none", got)
	}
}

func TestAPauseSuspendsTheRunningMergeAndTheQueuesResumeTakesItOnAtItsStep(t *testing.T) {
	// Arrange, Act: pause while the head holds in its gate (the running gate
	// is cut), then resume the queue.
	c := pausedUnderGate(t)
	cut := c.runner.ran(c.dirs[theWorkspace])
	queued, err := c.h.db.MergeQueue(context.Background(), c.h.repoKey())
	if err != nil {
		t.Fatal(err)
	}
	if err := c.h.o.Unpause(context.Background(), nil); err != nil {
		t.Fatalf("Unpause: %v", err)
	}
	c.promote(t)

	// Assert.
	if cut != 1 || len(queued) != 1 || queued[0].State != wsm.MergeAdmitted {
		t.Fatalf("after the pause: gate ran %d, queue %v; want one cut run and the merge still admitted in line", cut, queued)
	}
	if got := c.git.landedOwn(); len(got) != 1 {
		t.Fatalf("landings after the resume %v, want the merge landed once", got)
	}
	if !c.recorded("info", "daemon.merge.pause", "suspended for its queue's pause") {
		t.Fatal("the pause's suspension was not recorded")
	}
}

// pausedUnderGate pauses every queue while the head holds in its test gate,
// and answers once the paused run has let go.
func pausedUnderGate(t *testing.T) *chain {
	t.Helper()
	c, done := headInGate(t)
	if err := c.h.o.Pause(context.Background(), nil); err != nil {
		t.Fatalf("Pause: %v", err)
	}
	<-done
	return c
}

func TestAGateThePauseCutNamesThePause(t *testing.T) {
	// Arrange, Act.
	c := pausedUnderGate(t)

	// Assert.
	got := gateRecord(t, c.h, "test gate was cut before its verdict")
	if got["by"] != "its queue's pause" {
		t.Fatalf("cut by = %v, want the queue's pause", got["by"])
	}
	if gateRecords(c.h, "it runs again from its start once the queue is resumed") != 1 {
		t.Fatal("the cut does not say the gate runs again once the queue is resumed")
	}
}

func TestAPausedMergesSuspendRecordNamesThePause(t *testing.T) {
	// Arrange, Act.
	c := pausedUnderGate(t)

	// Assert.
	rec, found := recordWith(c.h, "info", "daemon.merge.suspend")
	if !found {
		t.Fatal("the paused merge wrote no INFO suspend record")
	}
	if rec.Context["by"] != "its queue's pause" || rec.Context["cause"] != errPaused.Error() {
		t.Fatalf("suspend record context = %v, want the pause as its cause", rec.Context)
	}
	if !strings.Contains(rec.Message, "until the queue is resumed") {
		t.Fatalf("suspend record says %q, want that it stays suspended until the queue is resumed", rec.Message)
	}
}

func TestAPausedMergesSuspendRecordKeepsTheStepsErrorApart(t *testing.T) {
	// Arrange, Act.
	c := pausedUnderGate(t)

	// Assert.
	rec, found := recordWith(c.h, "info", "daemon.merge.suspend")
	if !found {
		t.Fatal("the paused merge wrote no INFO suspend record")
	}
	if stepErr, ok := rec.Context["step_error"].(string); !ok || stepErr == "" || stepErr == rec.Context["cause"] {
		t.Fatalf("suspend record context = %v, want the cut step's own error as step_error", rec.Context)
	}
}

func TestThePausesTakeBackRecordsThatTheMergeStaysSuspended(t *testing.T) {
	// Arrange, Act.
	c := pausedUnderGate(t)

	// Assert.
	rec, found := recordWith(c.h, "info", "daemon.merge.recover")
	if !found {
		t.Fatal("the pause's take-back wrote no INFO recover record")
	}
	if rec.Context["paused"] != true || !strings.Contains(rec.Message, "stays suspended until the queue is resumed") {
		t.Fatalf("recover record = %q %v, want that the merge stays suspended on its paused queue", rec.Message, rec.Context)
	}
}

// otherQueueKey is a second repository's queue key, for the tests that need
// more than the harness's one queue.
const otherQueueKey = wsm.RepoKey("/other/checkout/.git")

// seedQueue gives a repository's queue one waiting entry, which is all an
// unscoped pause needs to count the queue as one to change.
func seedQueue(h *harness, repo wsm.RepoKey, paused bool) {
	h.db.mu.Lock()
	defer h.db.mu.Unlock()
	h.db.queues[repo] = append(h.db.queues[repo], wsm.MergeQueueEntry{State: wsm.MergeQueued})
	h.db.paused[repo] = paused
}

// pauseVerb runs Pause or Unpause, the two verbs every pause test drives.
func pauseVerb(h *harness, pause bool, scope *RepositoryScope) error {
	if pause {
		return h.o.Pause(context.Background(), scope)
	}
	return h.o.Unpause(context.Background(), scope)
}

func TestAPauseChangeThatWouldChangeNothingIsRefusedAndWritesNothing(t *testing.T) {
	tests := []struct {
		name    string
		pause   bool
		wantArm string
		wantOp  string
	}{
		{name: "pausing a paused queue", pause: true, wantArm: ArmAlreadyPaused, wantOp: "daemon.merge.pause"},
		{name: "resuming a running queue", pause: false, wantArm: ArmNotPaused, wantOp: "daemon.merge.unpause"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: the queue already stands where the verb would put it, and
			// any write would answer an error the refusal could not be confused
			// with.
			h := newHarness(t)
			seedQueue(h, h.repoKey(), tt.pause)
			h.db.pauseWriteErr = errors.New("a no-op pause wrote the state database")

			// Act.
			err := pauseVerb(h, tt.pause, nil)

			// Assert.
			var refused *RefusalError
			if !errors.As(err, &refused) || refused.Arm != tt.wantArm {
				t.Fatalf("error = %v, want the %s refusal", err, tt.wantArm)
			}
			if rec, ok := h.recordFor("warn", tt.wantOp); !ok || rec.Context["arm"] != tt.wantArm {
				t.Fatalf("WARN %s record = %v (%v), want one naming the arm", tt.wantOp, rec.Context, ok)
			}
		})
	}
}

func TestAnUnscopedPauseChangeWithNoQueueAnywhereIsRefused(t *testing.T) {
	tests := []struct {
		name    string
		pause   bool
		wantArm string
		wantOp  string
	}{
		{name: "pause", pause: true, wantArm: ArmAlreadyPaused, wantOp: "daemon.merge.pause"},
		{name: "resume", pause: false, wantArm: ArmNotPaused, wantOp: "daemon.merge.unpause"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: the harness holds no queue at all.
			h := newHarness(t)

			// Act.
			err := pauseVerb(h, tt.pause, nil)

			// Assert.
			var refused *RefusalError
			if !errors.As(err, &refused) || refused.Arm != tt.wantArm {
				t.Fatalf("error = %v, want the %s refusal", err, tt.wantArm)
			}
			if rec, ok := h.recordFor("warn", tt.wantOp); !ok || rec.Context["arm"] != tt.wantArm {
				t.Fatalf("WARN %s record = %v (%v), want one naming the arm", tt.wantOp, rec.Context, ok)
			}
		})
	}
}

func TestAnUnscopedPausePausesEveryRepositoryWithAQueue(t *testing.T) {
	// Arrange: two repositories, each with a running queue.
	h := newHarness(t)
	seedQueue(h, h.repoKey(), false)
	seedQueue(h, otherQueueKey, false)

	// Act.
	err := h.o.Pause(context.Background(), nil)

	// Assert.
	if err != nil {
		t.Fatalf("Pause: %v", err)
	}
	h.db.mu.Lock()
	defer h.db.mu.Unlock()
	if !h.db.paused[h.repoKey()] || !h.db.paused[otherQueueKey] {
		t.Fatalf("paused = %v, want both repositories' queues paused", h.db.paused)
	}
}

func TestAnUnscopedResumeResumesOnlyTheQueuesThatWerePaused(t *testing.T) {
	// Arrange: one paused queue and one running queue; the resume changes the
	// paused one and is not refused for the other.
	h := newHarness(t)
	seedQueue(h, h.repoKey(), true)
	seedQueue(h, otherQueueKey, false)

	// Act.
	err := h.o.Unpause(context.Background(), nil)

	// Assert.
	if err != nil {
		t.Fatalf("Unpause: %v", err)
	}
	h.db.mu.Lock()
	defer h.db.mu.Unlock()
	if h.db.paused[h.repoKey()] || h.db.paused[otherQueueKey] {
		t.Fatalf("paused = %v, want neither queue paused", h.db.paused)
	}
}

func TestAScopedPauseOfARepositoryTheRegistryDoesNotHoldIsRefused(t *testing.T) {
	tests := []struct {
		name    string
		scope   *RepositoryScope
		wantMsg string
	}{
		{name: "an unknown id", scope: &RepositoryScope{ID: "no-such-repo"}, wantMsg: "refused a pause change for a repository the registry does not hold"},
		{name: "an unknown dir", scope: &RepositoryScope{Dir: "/no/such/checkout"}, wantMsg: "refused a pause change for a repository the registry does not hold"},
		{name: "an empty ref", scope: &RepositoryScope{}, wantMsg: "refused a pause change scoped by an empty repository ref"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: a running queue the refusal must leave alone.
			h := newHarness(t)
			seedQueue(h, h.repoKey(), false)

			// Act.
			err := h.o.Pause(context.Background(), tt.scope)

			// Assert.
			var refused *RefusalError
			if !errors.As(err, &refused) || refused.Arm != ArmUnknownRepository {
				t.Fatalf("error = %v, want the %s refusal", err, ArmUnknownRepository)
			}
			rec, ok := h.recordFor("warn", "daemon.merge.pause")
			if !ok || rec.Message != tt.wantMsg || rec.Context["arm"] != ArmUnknownRepository {
				t.Fatalf("WARN record = %+v (%v), want %q naming the arm", rec, ok, tt.wantMsg)
			}
			h.db.mu.Lock()
			defer h.db.mu.Unlock()
			if h.db.paused[h.repoKey()] {
				t.Fatal("a refused scoped pause paused the queue")
			}
		})
	}
}
