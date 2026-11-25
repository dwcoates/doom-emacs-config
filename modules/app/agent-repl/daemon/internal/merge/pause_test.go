package merge

import (
	"context"
	"errors"
	"strings"
	"testing"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/wsm"
	"agentrepl/logging/logsite"
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
	// Arrange, Act: pause while the head holds in its gate (the gate's watch
	// is cut, its run goes on on the test service), let the run finish, then
	// resume the queue.
	c, done, release := headHeldInGate(t)
	if err := c.h.o.Pause(context.Background(), nil, userPauser); err != nil {
		t.Fatalf("Pause: %v", err)
	}
	<-done
	queued, err := c.h.db.MergeQueue(context.Background(), c.h.repoKey())
	if err != nil {
		t.Fatal(err)
	}
	close(release)
	if err := c.h.o.Unpause(context.Background(), nil); err != nil {
		t.Fatalf("Unpause: %v", err)
	}
	c.promote(t)

	// Assert: the one run the pause left going is the one the resume read.
	if ran := c.runner.ran(c.dirs[theWorkspace]); ran != 1 || len(queued) != 1 || queued[0].State != wsm.MergeAdmitted {
		t.Fatalf("gate ran %d, queue after the pause %v; want one run and the merge still admitted in line", ran, queued)
	}
	if stops := c.h.tests.stopped(); len(stops) != 0 {
		t.Fatalf("stopped %v, want the run the pause left going never stopped", stops)
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
	if err := c.h.o.Pause(context.Background(), nil, userPauser); err != nil {
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
	if gateRecords(c.h, "its run goes on on the test service, and the merge watches it again once the queue is resumed") != 1 {
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

// seedQueue gives a repository's queue one waiting entry of the harness's
// workspace, which an unscoped pause counts as a queue to change and its
// redraw can name. The entry was enqueued at the harness clock, so an
// unscoped pause can draw its tab.
func seedQueue(h *harness, repo wsm.RepoKey, paused bool) {
	h.db.mu.Lock()
	defer h.db.mu.Unlock()
	h.db.queues[repo] = append(h.db.queues[repo], wsm.MergeQueueEntry{
		Repo: repo, Workspace: theWorkspace, State: wsm.MergeQueued, Source: ownBranch, EnqueuedAt: h.clock(),
	})
	h.db.paused[repo] = paused
}

// pauseVerb runs Pause or Unpause, the two verbs every pause test drives.
func pauseVerb(h *harness, pause bool, scope *RepositoryScope) error {
	if pause {
		return h.o.Pause(context.Background(), scope, userPauser)
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
			if rec, ok := h.recordFor("info", tt.wantOp); !ok || rec.Context["arm"] != tt.wantArm {
				t.Fatalf("INFO %s record = %v (%v), want one naming the arm", tt.wantOp, rec.Context, ok)
			}
			if rec, ok := h.recordFor("warn", tt.wantOp); ok {
				t.Fatalf("WARN %s record = %v, want none: the refusal is the asker's answer, not a fault", tt.wantOp, rec.Context)
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
			if rec, ok := h.recordFor("info", tt.wantOp); !ok || rec.Context["arm"] != tt.wantArm {
				t.Fatalf("INFO %s record = %v (%v), want one naming the arm", tt.wantOp, rec.Context, ok)
			}
			if rec, ok := h.recordFor("warn", tt.wantOp); ok {
				t.Fatalf("WARN %s record = %v, want none: the refusal is the asker's answer, not a fault", tt.wantOp, rec.Context)
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
	err := h.o.Pause(context.Background(), nil, userPauser)

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
			err := h.o.Pause(context.Background(), tt.scope, userPauser)

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

// TestAPauseChangeRedrawsEveryBubbleOnTheQueue covers the bubble's half of a
// pause (frontend.v1.FeedMergeUpdate.paused, frontend.v1.FeedMergeQueue.paused):
// a pause redraws every head in line as paused and every queue tab with the
// queue paused, and a resume redraws them running again.
func TestAPauseChangeRedrawsEveryBubbleOnTheQueue(t *testing.T) {
	tests := []struct {
		name       string
		act        func(h *harness) error
		wantPaused bool
	}{
		{name: "pause", act: func(h *harness) error { return h.o.Pause(context.Background(), nil, userPauser) }, wantPaused: true},
		{name: "resume", act: func(h *harness) error {
			if err := h.o.Pause(context.Background(), nil, userPauser); err != nil {
				return err
			}
			return h.o.Unpause(context.Background(), nil)
		}, wantPaused: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: two merges in line on a queue that runs.
			h := newHarness(t)
			enqueue(t, h)
			queueAnother(t, h, "ws-2", "two")

			// Act.
			if err := tt.act(h); err != nil {
				t.Fatalf("act: %v", err)
			}

			// Assert.
			for _, ws := range []wsm.WorkspaceID{theWorkspace, "ws-2"} {
				if got := lastMergeOf(t, h, ws).GetUpdate().GetPaused() != nil; got != tt.wantPaused {
					t.Fatalf("%s's head says paused %v, want %v", ws, got, tt.wantPaused)
				}
				if got := lastQueueTabOf(t, h, ws).GetQueue().GetPaused() != nil; got != tt.wantPaused {
					t.Fatalf("%s's queue tab says paused %v, want %v", ws, got, tt.wantPaused)
				}
			}
		})
	}
}

// TestAPauseWhoseRedrawFailsIsRecordedAndThePauseStands covers redrawPaused's
// error path: a queue whose bubbles cannot be redrawn is recorded at ERROR
// with its repository and cause, the pause is answered with the cause, and
// the pause itself stands.
func TestAPauseWhoseRedrawFailsIsRecordedAndThePauseStands(t *testing.T) {
	// Arrange: a merge in line whose workspace record is gone by the redraw.
	h := newHarness(t)
	enqueue(t, h)
	h.db.mu.Lock()
	h.db.queues[h.repoKey()][0].Workspace = "ws-gone"
	h.db.mu.Unlock()

	// Act.
	err := h.o.Pause(context.Background(), nil, userPauser)

	// Assert.
	if err == nil {
		t.Fatalf("Pause answered no error, want the failed redraw")
	}
	if !loggedLike(h, logLike{level: "error", op: "daemon.merge.pause", text: "could not redraw the merge queue's bubbles after its pause changed",
		context: map[string]string{"repo": string(h.repoKey())}}) {
		t.Fatalf("no error record of the failed redraw: %+v", h.logs.Records())
	}
	h.db.mu.Lock()
	defer h.db.mu.Unlock()
	if !h.db.paused[h.repoKey()] {
		t.Fatalf("the queue is not paused, want the pause to stand")
	}
}

func TestResumeOwnPauseResumesOnlyThePauseOfTheAskingWorkspace(t *testing.T) {
	own := &wsm.MergeQueuePause{At: epochPause, ToLand: true, By: wsm.MergeQueuePauser{Kind: wsm.MergeQueuePauserWorkspace, Name: "ws-one", Workspace: theWorkspace}}
	ownHard := &wsm.MergeQueuePause{At: epochPause, Hard: true, ToLand: true, By: own.By}
	tests := []struct {
		name        string
		pause       *wsm.MergeQueuePause
		wantResumed bool
	}{
		{name: "its own pause to land a change", pause: own, wantResumed: true},
		{name: "its own hard stop to land a change", pause: ownHard, wantResumed: true},
		// 2026-11-14 20:47:10: the owner asked for this pause, to save power.
		{name: "its own pause not made to land", pause: &wsm.MergeQueuePause{At: epochPause, By: own.By}},
		{name: "the host's pause", pause: &wsm.MergeQueuePause{At: epochPause, By: wsm.MergeQueuePauser{Kind: wsm.MergeQueuePauserHost, Cause: "no runner"}}},
		{name: "the user's pause", pause: &wsm.MergeQueuePause{At: epochPause, By: wsm.MergeQueuePauser{Kind: wsm.MergeQueuePauserUser}}},
		{name: "another workspace's pause", pause: &wsm.MergeQueuePause{At: epochPause, By: wsm.MergeQueuePauser{Kind: wsm.MergeQueuePauserWorkspace, Name: "ws-two", Workspace: otherWorkspace}}},
		{name: "a workspace pause that names no id", pause: &wsm.MergeQueuePause{At: epochPause, By: wsm.MergeQueuePauser{Kind: wsm.MergeQueuePauserWorkspace, Name: "ws-one"}}},
		{name: "a running queue"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			seedQueue(h, scopedKey(h), tt.pause != nil)
			h.db.mu.Lock()
			h.db.pauses[scopedKey(h)] = tt.pause
			h.db.mu.Unlock()

			// Act
			resumed, err := h.o.ResumeOwnPause(context.Background(), RepositoryScope{ID: theRepository}, theWorkspace)

			// Assert
			if err != nil {
				t.Fatalf("ResumeOwnPause: %v", err)
			}
			if resumed != tt.wantResumed {
				t.Fatalf("resumed = %v, want %v", resumed, tt.wantResumed)
			}
			h.db.mu.Lock()
			defer h.db.mu.Unlock()
			if wantPaused := tt.pause != nil && !tt.wantResumed; h.db.paused[scopedKey(h)] != wantPaused {
				t.Fatalf("paused = %v, want %v", h.db.paused[scopedKey(h)], wantPaused)
			}
			if _, ok := h.recordFor("info", "daemon.merge.resume_own_pause"); !ok {
				t.Fatal("no INFO record of the resume's outcome")
			}
		})
	}
}

func TestResumeOwnPauseRefusesARepositoryTheRegistryDoesNotHold(t *testing.T) {
	// Arrange
	h := newHarness(t)
	seedQueue(h, scopedKey(h), true)

	// Act
	resumed, err := h.o.ResumeOwnPause(context.Background(), RepositoryScope{ID: "no-such-repo"}, theWorkspace)

	// Assert
	var refused *RefusalError
	if resumed || !errors.As(err, &refused) || refused.Arm != ArmUnknownRepository {
		t.Fatalf("ResumeOwnPause = (%v, %v), want the %s refusal", resumed, err, ArmUnknownRepository)
	}
	h.db.mu.Lock()
	defer h.db.mu.Unlock()
	if !h.db.paused[scopedKey(h)] {
		t.Fatal("a refused resume resumed the queue")
	}
}

func TestResumeOwnPauseThatCannotReadThePauseLeavesTheQueueAndRecordsWhy(t *testing.T) {
	// Arrange
	h := newHarness(t)
	seedQueue(h, scopedKey(h), true)
	h.db.mu.Lock()
	h.db.pausedReadErr = errors.New("db gone")
	h.db.mu.Unlock()

	// Act
	resumed, err := h.o.ResumeOwnPause(context.Background(), RepositoryScope{ID: theRepository}, theWorkspace)

	// Assert
	if resumed || err == nil {
		t.Fatalf("ResumeOwnPause = (%v, %v), want the read's failure", resumed, err)
	}
	rec, ok := h.recordFor("error", "daemon.merge.resume_own_pause")
	if !ok || rec.Context["workspace"] != string(theWorkspace) || !strings.Contains(rec.Context["error"].(string), "db gone") {
		t.Fatalf("ERROR record = %+v (%v), want one naming the workspace and the cause", rec, ok)
	}
	h.db.mu.Lock()
	defer h.db.mu.Unlock()
	if !h.db.paused[scopedKey(h)] {
		t.Fatal("a resume that could not read the pause resumed the queue")
	}
}

// epochPause is the instant a seeded pause record names.
var epochPause = time.Date(2026, 11, 13, 18, 0, 0, 0, time.UTC)

// scopedKey is the queue key a scope naming the harness's repository resolves
// to (queueKeyOf).
func scopedKey(h *harness) wsm.RepoKey { return wsm.RepoKey(h.repoRoot + "/.git") }

func TestAPauseRecordsWhetherItWasMadeToLand(t *testing.T) {
	tests := []struct {
		name   string
		hard   bool
		toLand bool
	}{
		{name: "a pause made to land a change", toLand: true},
		{name: "a pause the owner asked for"},
		{name: "a hard stop made to land a change", hard: true, toLand: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			seedQueue(h, scopedKey(h), false)
			by := Pauser{By: RequestedByUser, ToLand: tt.toLand}

			// Act
			var err error
			if tt.hard {
				err = h.o.HardStop(context.Background(), &RepositoryScope{ID: theRepository}, by)
			} else {
				err = h.o.Pause(context.Background(), &RepositoryScope{ID: theRepository}, by)
			}

			// Assert
			if err != nil {
				t.Fatalf("pause: %v", err)
			}
			h.db.mu.Lock()
			defer h.db.mu.Unlock()
			if got := h.db.pauses[scopedKey(h)]; got == nil || got.ToLand != tt.toLand || got.Hard != tt.hard {
				t.Fatalf("pause = %+v, want to_land %v, hard %v", got, tt.toLand, tt.hard)
			}
		})
	}
}

func TestRefuseNoopPauseNamesTheArmAndReasonOfTheAskedForState(t *testing.T) {
	tests := []struct {
		name       string
		paused     bool
		wantArm    string
		wantReason string
	}{
		{name: "pause", paused: true, wantArm: ArmAlreadyPaused, wantReason: "already"},
		{name: "resume", paused: false, wantArm: ArmNotPaused, wantReason: "not"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			refused := h.o.refuseNoopPause("daemon.merge.test", tt.paused, logsite.Msg("the message"), "not", "already")

			// Assert: the arm and reason follow the asked-for state, and one INFO
			// record names the arm and no WARN record is written.
			if refused.Arm != tt.wantArm || refused.Reason != tt.wantReason {
				t.Fatalf("refusal = %+v, want arm %s reason %q", refused, tt.wantArm, tt.wantReason)
			}
			if rec, ok := h.recordFor("info", "daemon.merge.test"); !ok || rec.Context["arm"] != tt.wantArm || rec.Message != "the message" {
				t.Fatalf("INFO record = %+v (%v), want the message naming the arm", rec, ok)
			}
			if _, ok := h.recordFor("warn", "daemon.merge.test"); ok {
				t.Fatalf("a WARN record was written, want none")
			}
		})
	}
}

// EVERY PAUSE CHANGE SAYS WHO MADE IT AND WHY: a queue found paused names
// its pauser from the log alone.
func TestAPauseChangeIsRecordedWithItsPauser(t *testing.T) {
	tests := []struct {
		name   string
		act    func(h *harness) error
		want   dlog.Context
		absent []string
	}{
		{name: "the user's pause", act: func(h *harness) error { return h.o.Pause(context.Background(), &theScope, userPauser) },
			want: dlog.Context{"paused": true, "pauser": "user", "hard": false, "to_land": false}, absent: []string{"cause", "pausing_workspace"}},
		{name: "a workspace's pause to land", act: func(h *harness) error {
			return h.o.Pause(context.Background(), &theScope, Pauser{By: RequestedByAgent, Asker: theWorkspace, ToLand: true})
		}, want: dlog.Context{"paused": true, "pauser": "workspace", "to_land": true, "pausing_workspace": string(theWorkspace)}},
		{name: "the host's pause on a fault", act: func(h *harness) error {
			return h.o.holdForHostFault(context.Background(), h.repoKey(), "the test runner could not be started")
		}, want: dlog.Context{"paused": true, "pauser": "host", "cause": "the test runner could not be started"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			seedQueue(h, h.repoKey(), false)

			// Act
			if err := tt.act(h); err != nil {
				t.Fatal(err)
			}

			// Assert
			rec, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "info", Operation: "daemon.merge.pause", Message: "changed the merge queue's pause state"})
			if !ok {
				t.Fatalf("records = %v; want the pause change at INFO", h.logs.Records())
			}
			for k, v := range tt.want {
				if rec.Context[k] != v {
					t.Fatalf("record %v: %s = %v, want %v", rec.Context, k, rec.Context[k], v)
				}
			}
			for _, k := range tt.absent {
				if _, has := rec.Context[k]; has {
					t.Fatalf("record %v carries %s, want none", rec.Context, k)
				}
			}
		})
	}
}
