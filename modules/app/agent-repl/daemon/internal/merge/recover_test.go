package merge

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"reflect"
	"slices"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// TestRecoverReEnqueuesWaitingMergesInOrder covers the durable queue's whole
// point: a bounce brings back exactly what was waiting, in the order it waited.
func TestRecoverReEnqueuesWaitingMergesInOrder(t *testing.T) {
	// Arrange: three merges queued before the restart.
	h := newHarness(t)
	order := []ids.WorkspaceID{"ws-1", "ws-2", "ws-3"}
	h.register("ws-2", "ws-two")
	h.register("ws-3", "ws-three")
	for _, ws := range order {
		enqueueWorkspace(t, h, ws)
	}

	// Act.
	if err := h.o.Recover(context.Background()); err != nil {
		t.Fatalf("Recover failed: %v", err)
	}

	// Assert.
	entries, _ := h.db.MergeQueue(context.Background(), h.repoKey())
	for i, entry := range entries {
		if entry.Workspace != order[i] {
			t.Fatalf("after recovery the queue is %+v, want %v", entries, order)
		}
	}
}

// TestRecoverResumesAnInFlightMergeOverACleanTarget covers the resumable case:
// nothing of the interrupted merge is half applied, so it can run again.
func TestRecoverResumesAnInFlightMergeOverACleanTarget(t *testing.T) {
	// Arrange: a merge that was admitted and whose lease survived the restart.
	h := newHarness(t)
	enqueue(t, h)
	interruptMerge(t, h)
	h.git.clean = true

	// Act.
	if err := h.o.Recover(context.Background()); err != nil {
		t.Fatalf("Recover failed: %v", err)
	}

	// Assert.
	entries, _ := h.db.MergeQueue(context.Background(), h.repoKey())
	if len(entries) != 1 || entries[0].State != wsm.MergeQueued {
		t.Fatalf("the recovered entry is %+v, want it waiting again", entries)
	}
	if facts, _ := h.o.Facts(theWorkspace); facts.State != StateQueued {
		t.Fatalf("the merge is %q, want it queued for another run", facts.State)
	}
}

// TestRecoverReleasesTheStuckLease covers the invariant a stuck lease would
// break: a lease nobody holds and nobody releases refuses that workspace's every
// prompt forever.
func TestRecoverReleasesTheStuckLease(t *testing.T) {
	// Arrange: a merge whose lease survived the restart.
	h := newHarness(t)
	enqueue(t, h)
	interruptMerge(t, h)

	// Act.
	if err := h.o.Recover(context.Background()); err != nil {
		t.Fatalf("Recover failed: %v", err)
	}

	// Assert.
	if _, held, _ := h.db.Lease(context.Background(), theWorkspace); held {
		t.Fatal("a merge lease survived recovery")
	}
}

// TestRecoverRequeuesARecordlessMergeWhateverItsTargetHolds covers the
// owner's ruling: a merge's fate is never decided from what its target's
// working tree holds. An admitted merge with no progress record never took a
// step, so content in the target -- an untracked file, the owner's own edits
// -- is no evidence about it.
func TestRecoverRequeuesARecordlessMergeWhateverItsTargetHolds(t *testing.T) {
	// Arrange: an interrupted merge with no record, over a target whose
	// status would read unclean.
	h := newHarness(t)
	enqueue(t, h)
	interruptMerge(t, h)
	h.git.clean = false

	// Act.
	if err := h.o.Recover(context.Background()); err != nil {
		t.Fatalf("Recover failed: %v", err)
	}

	// Assert.
	if facts, _ := h.o.Facts(theWorkspace); facts.State != StateQueued {
		t.Fatalf("the merge is %q, want it queued for another run", facts.State)
	}
}

// TestRecoverNeverReadsTheTargetsStatus pins the same ruling at its source: the
// recovery asks git nothing about the target's working tree.
func TestRecoverNeverReadsTheTargetsStatus(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	enqueue(t, h)
	interruptMerge(t, h)

	// Act.
	if err := h.o.Recover(context.Background()); err != nil {
		t.Fatalf("Recover failed: %v", err)
	}

	// Assert.
	if slices.Contains(h.git.calls, "is_clean") {
		t.Fatalf("the recovery read the target's status: %v", h.git.calls)
	}
}

// TestRecoverLoudlyFailsWhenTheGeometryIsGone covers a workspace whose creation
// job no longer resolves: there is nothing left to resume into.
func TestRecoverLoudlyFailsWhenTheGeometryIsGone(t *testing.T) {
	// Arrange: an interrupted merge whose geometry was forgotten.
	h := newHarness(t)
	enqueue(t, h)
	interruptMerge(t, h)
	h.db.mu.Lock()
	delete(h.db.jobs, theWorkspace)
	h.db.mu.Unlock()

	// Act.
	if err := h.o.Recover(context.Background()); err != nil {
		t.Fatalf("Recover failed: %v", err)
	}

	// Assert.
	if facts, _ := h.o.Facts(theWorkspace); facts.State != StateFailed {
		t.Fatalf("the merge is %q, want it failed loudly", facts.State)
	}
}

// TestRecoverNeverLeavesAMergeInFlight covers the whole file's invariant: after
// recovery nothing is admitted-but-unowned.
func TestRecoverNeverLeavesAMergeInFlight(t *testing.T) {
	// Arrange: one resumable and one unresumable merge, in two repositories.
	h := newHarness(t)
	h.register("ws-2", "ws-two")
	enqueue(t, h)
	enqueueWorkspace(t, h, "ws-2")
	interruptMerge(t, h)
	h.git.clean = false

	// Act.
	if err := h.o.Recover(context.Background()); err != nil {
		t.Fatalf("Recover failed: %v", err)
	}

	// Assert.
	queues, _ := h.db.AllMergeQueues(context.Background())
	for repo, entries := range queues {
		for _, entry := range entries {
			if entry.State == wsm.MergeAdmitted {
				t.Fatalf("%s in %s is still admitted after recovery", entry.Workspace, repo)
			}
		}
	}
}

// enqueueWorkspace queues one named workspace's merge.
func enqueueWorkspace(t *testing.T, h *harness, ws ids.WorkspaceID) {
	t.Helper()
	if _, err := h.o.Enqueue(context.Background(), Request{Workspace: ws, By: RequestedByUser}); err != nil {
		t.Fatalf("enqueueing %s: %v", ws, err)
	}
}

// interruptMerge leaves the store looking the way a daemon that died mid-merge
// leaves it: the entry admitted and the lease still held.
func interruptMerge(t *testing.T, h *harness) {
	t.Helper()
	if err := h.db.AdmitMerge(context.Background(), h.repoKey(), theWorkspace); err != nil {
		t.Fatalf("admitting: %v", err)
	}
	if _, err := h.db.AcquireLease(context.Background(), theWorkspace, wsm.HolderMerge, wsm.PolicyRefuse); err != nil {
		t.Fatalf("taking the lease: %v", err)
	}
}

// TestRecoverRefusesACorruptCreationJobRatherThanFailingTheMerge covers the
// corrupt-refuses-load ruling: a creation_jobs row that will not decode is
// state corruption, so the recovery FAILS THE BOOT instead of downgrading it
// into the ordinary "the geometry is gone" outcome and serving on.
func TestRecoverRefusesACorruptCreationJobRatherThanFailingTheMerge(t *testing.T) {
	// Arrange: an interrupted merge whose creation_jobs row will not decode.
	h := newHarness(t)
	enqueue(t, h)
	interruptMerge(t, h)
	h.db.mu.Lock()
	h.db.jobDecodeErrs[theWorkspace] = &wsm.DecodeError{
		Table: "creation_jobs", Row: string(theWorkspace), Field: "actions_before",
		Err: errors.New("not valid json"),
	}
	h.db.mu.Unlock()

	// Act.
	err := h.o.Recover(context.Background())

	// Assert.
	if err == nil {
		t.Fatal("Recover over a corrupt creation_jobs row = nil, want a loud refusal")
	}
	var decodeErr *wsm.DecodeError
	if !errors.As(err, &decodeErr) {
		t.Fatalf("Recover error = %v, want it to carry the *wsm.DecodeError", err)
	}
}

// forgetJob removes a workspace's recorded merge geometry, which is what a nuke
// between the shutdown and the restart leaves behind: a durable queue entry for
// a workspace whose merge can no longer be described.
func forgetJob(h *harness, ws ids.WorkspaceID) {
	h.db.mu.Lock()
	defer h.db.mu.Unlock()
	delete(h.db.jobs, ws)
}

// TestRecoverAbandonsAWaitingMergeItCannotRequeueWithTheShutdownAsTheCause is
// the daemon-shutdown producer: the durable queue survives the shutdown, so the
// restart is where a merge that cannot go back gives up, and the cause names
// the shutdown rather than leaving the queue entry to fail at the front.
func TestRecoverAbandonsAWaitingMergeItCannotRequeueWithTheShutdownAsTheCause(t *testing.T) {
	// Arrange: a merge queued before the shutdown, whose geometry is gone.
	h := newHarness(t)
	enqueue(t, h)
	forgetJob(h, theWorkspace)
	fresh, err := newOrchestrator(h.deps())
	if err != nil {
		t.Fatalf("building the successor: %v", err)
	}

	// Act.
	if err := fresh.Recover(context.Background()); err != nil {
		t.Fatalf("Recover failed: %v", err)
	}

	// Assert.
	want := "the daemon shut down while this merge was waiting in the queue, and the restart could not put it back"
	if got := h.feed.lastAbandonedSummary(); got != want {
		t.Fatalf("the unrecoverable merge's summary = %q, want the shutdown's own cause", got)
	}
}

// TestRecoverTakesAnUnrequeueableMergeOffItsQueue covers the durable half: the
// entry is removed, so the queue behind it is not held by a merge that can
// never be admitted.
func TestRecoverTakesAnUnrequeueableMergeOffItsQueue(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	enqueue(t, h)
	forgetJob(h, theWorkspace)
	fresh, err := newOrchestrator(h.deps())
	if err != nil {
		t.Fatalf("building the successor: %v", err)
	}

	// Act.
	if err := fresh.Recover(context.Background()); err != nil {
		t.Fatalf("Recover failed: %v", err)
	}

	// Assert.
	entries, _ := h.db.MergeQueue(context.Background(), h.repoKey())
	if len(entries) != 0 {
		t.Fatalf("the queue after the recovery is %+v, want the unrequeueable merge gone", entries)
	}
}

// TestRecoverRefusesTheBootWhenAWaitingMergesJobRowWillNotDecode keeps the
// corruption arm distinct from the abandon: a half-written row is evidence,
// not a merge that legitimately lost its geometry.
func TestRecoverRefusesTheBootWhenAWaitingMergesJobRowWillNotDecode(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	enqueue(t, h)
	h.db.mu.Lock()
	h.db.jobDecodeErrs[theWorkspace] = &wsm.DecodeError{Table: "creation_jobs", Row: string(theWorkspace), Field: "layout", Err: errors.New("bad json")}
	h.db.mu.Unlock()
	fresh, err := newOrchestrator(h.deps())
	if err != nil {
		t.Fatalf("building the successor: %v", err)
	}

	// Act.
	recoverErr := fresh.Recover(context.Background())

	// Assert.
	var decodeErr *wsm.DecodeError
	if !errors.As(recoverErr, &decodeErr) {
		t.Fatalf("Recover = %v, want the boot refused with the decode error", recoverErr)
	}
	if got := h.feed.lastMergeErrorArm(); got != "" {
		t.Fatalf("the corrupt row drew the %q terminal, want the boot refused instead", got)
	}
}

// TestRecoverResubmitsATurnAMergeDisplacedAndNeverPutBack covers the boot
// recovery's whole purpose: a merge that died holding the user's turn does not
// keep it.
func TestRecoverResubmitsATurnAMergeDisplacedAndNeverPutBack(t *testing.T) {
	// Arrange: a turn left marked displaced by a merge that never released.
	h := newHarness(t)
	h.displaceTurn("carry on with the refactor")

	// Act.
	if err := h.o.Recover(context.Background()); err != nil {
		t.Fatalf("Recover failed: %v", err)
	}

	// Assert.
	if n := h.queue.countOrigin(conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_DISPLACED_TURN_RESUME); n != 1 {
		t.Fatalf("the boot resubmitted the displaced turn %d times, want exactly once", n)
	}
}

// TestRecoverResubmitsTheDisplacedTurnsOwnText covers what comes back: the
// words the user typed, not an empty prompt.
func TestRecoverResubmitsTheDisplacedTurnsOwnText(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	const text = "carry on with the refactor"
	h.displaceTurn(text)

	// Act.
	if err := h.o.Recover(context.Background()); err != nil {
		t.Fatalf("Recover failed: %v", err)
	}

	// Assert.
	h.queue.mu.Lock()
	defer h.queue.mu.Unlock()
	if len(h.queue.submissions) != 1 {
		t.Fatalf("the boot made %d submissions, want exactly the one resubmission", len(h.queue.submissions))
	}
	got := h.queue.submissions[0].Said.GetContent().GetBlocks()[0].GetText().GetText()
	if got != text {
		t.Fatalf("the resubmitted text = %q, want %q", got, text)
	}
}

// TestASecondBootDoesNotResubmitADisplacedTurnAgain covers the exactly-once
// edge the durable claim exists for: two boots, one resubmission.
func TestASecondBootDoesNotResubmitADisplacedTurnAgain(t *testing.T) {
	// Arrange: one displaced turn, already recovered by a first boot.
	h := newHarness(t)
	h.displaceTurn("carry on with the refactor")
	if err := h.o.Recover(context.Background()); err != nil {
		t.Fatalf("the first Recover failed: %v", err)
	}

	// Act: a second boot over the same store.
	second, err := newOrchestrator(h.deps())
	if err != nil {
		t.Fatalf("building the successor: %v", err)
	}
	if err := second.Recover(context.Background()); err != nil {
		t.Fatalf("the second Recover failed: %v", err)
	}

	// Assert.
	if n := h.queue.countOrigin(conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_DISPLACED_TURN_RESUME); n != 1 {
		t.Fatalf("two boots resubmitted the displaced turn %d times, want exactly once", n)
	}
}

// TestRecoverClaimsADisplacedTurnEvenWhenTheResubmissionFails covers the claim
// ordering: a submission that fails must not leave the mark standing for the
// next boot to fire again.
func TestRecoverClaimsADisplacedTurnEvenWhenTheResubmissionFails(t *testing.T) {
	// Arrange: a displaced turn and a queue that refuses.
	h := newHarness(t)
	h.displaceTurn("carry on with the refactor")
	h.queue.err = errors.New("the session is gone")

	// Act.
	if err := h.o.Recover(context.Background()); err != nil {
		t.Fatalf("Recover failed: %v", err)
	}

	// Assert.
	h.db.mu.Lock()
	defer h.db.mu.Unlock()
	if len(h.db.retired) != 1 {
		t.Fatalf("the displaced turns claimed = %v, want the one record claimed", h.db.retired)
	}
}

// TestRecoverWithNoDisplacedTurnsResubmitsNothing covers the ordinary boot,
// where no merge ever took a turn.
func TestRecoverWithNoDisplacedTurnsResubmitsNothing(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	if err := h.o.Recover(context.Background()); err != nil {
		t.Fatalf("Recover failed: %v", err)
	}

	// Assert.
	if n := h.queue.countOrigin(conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_DISPLACED_TURN_RESUME); n != 0 {
		t.Fatalf("a boot with nothing displaced resubmitted %d turns, want none", n)
	}
}

// TestRecoverSweepsTheQueueTreesADeadRunLeft covers the queue's scratch trees
// across a crash: they are the queue's own, and the recovery removes them.
func TestRecoverSweepsTheQueueTreesADeadRunLeft(t *testing.T) {
	// Arrange: an interrupted merge whose run left two trees behind.
	h := newHarness(t)
	enqueue(t, h)
	interruptMerge(t, h)
	lease := h.leaseID(t)
	var want []string
	for attempt := 1; attempt <= 2; attempt++ {
		dir := treeFor(h.stateDir, lease, attempt)
		if err := os.MkdirAll(dir, 0o755); err != nil {
			t.Fatalf("making a leftover tree: %v", err)
		}
		want = append(want, dir)
	}

	// Act.
	if err := h.o.Recover(context.Background()); err != nil {
		t.Fatalf("Recover failed: %v", err)
	}

	// Assert.
	h.git.mu.Lock()
	defer h.git.mu.Unlock()
	if !equal(h.git.removedWorktrees, want) {
		t.Fatalf("removed %v, want the dead run's trees %v", h.git.removedWorktrees, want)
	}
}

func TestRecoverReArmsARequestStillWaitingForItsTurn(t *testing.T) {
	// Arrange: a request recorded before the restart.
	h := newHarness(t)
	if err := h.db.RequestMerge(context.Background(), h.repoKey(), theWorkspace, ownBranch, h.clock()); err != nil {
		t.Fatalf("RequestMerge: %v", err)
	}

	// Act.
	if err := h.o.Recover(context.Background()); err != nil {
		t.Fatalf("Recover: %v", err)
	}

	// Assert: re-armed, not yet in line, reported nowhere.
	if state, _ := h.entryState(); state != wsm.MergeRequested {
		t.Fatalf("entry state = %v, want still requested", state)
	}
	if _, standing := h.o.Facts(theWorkspace); standing {
		t.Fatal("a re-armed request was reported")
	}
	h.runWait(t)
	if state, _ := h.entryState(); state != wsm.MergeQueued {
		t.Fatalf("entry state after the wait = %v, want queued", state)
	}
}

func TestRecoverKeepsAResumedMergesSource(t *testing.T) {
	// Arrange: an admitted merge of the own branch kept open, which is not
	// the default source, interrupted.
	h := newHarness(t)
	if err := h.request(t, wsm.MergeSource{Kind: wsm.MergeSourceOwnBranch, KeepOpen: true}, RequestedByUser); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	before, _ := h.db.MergeQueue(context.Background(), h.repoKey())
	if len(before) != 1 {
		t.Fatalf("queue = %+v, want the one request", before)
	}
	recorded := before[0].Source
	interruptMerge(t, h)

	// Act.
	if err := h.o.Recover(context.Background()); err != nil {
		t.Fatalf("Recover: %v", err)
	}

	// Assert.
	entries, _ := h.db.MergeQueue(context.Background(), h.repoKey())
	if len(entries) != 1 || !reflect.DeepEqual(entries[0].Source, recorded) || entries[0].State != wsm.MergeQueued {
		t.Fatalf("queue = %+v, want the merge back in line with its source", entries)
	}
}

// recordedAt reports whether h logged a record at level whose message says msg.
func recordedAt(h *harness, level, msg string) bool {
	return loggedLike(h, logLike{level: level, text: msg})
}

// logLike is what a test looks for in the harness's log: every field set must
// match -- the level and operation exactly, the message by containing text,
// and each context key by its value.
type logLike struct {
	level, op, text string
	context         map[string]string
}

// loggedLike reports whether h's log holds a record like want. It is the one
// matcher every "was it recorded" helper of this package's tests reads.
func loggedLike(h *harness, want logLike) bool {
	for _, r := range h.logs.Records() {
		if want.level != "" && r.Level != want.level || want.op != "" && r.Operation != want.op || !strings.Contains(r.Message, want.text) {
			continue
		}
		matched := true
		for k, v := range want.context {
			if fmt.Sprint(r.Context[k]) != v {
				matched = false
			}
		}
		if matched {
			return true
		}
	}
	return false
}

func TestRecoverSweepsTheMergeStateNoQueueEntryNames(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	if err := h.o.Recover(context.Background()); err != nil {
		t.Fatalf("Recover: %v", err)
	}

	// Assert
	if h.db.sweeps != 1 {
		t.Fatalf("SweepOrphanedMergeState ran %d times, want once per recovery", h.db.sweeps)
	}
}

func TestRecoverRecordsWhatTheSweepRemoved(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.db.swept = wsm.OrphanedMergeState{ProgressRecords: 1, RoundsClosed: 29}

	// Act
	if err := h.o.Recover(context.Background()); err != nil {
		t.Fatalf("Recover: %v", err)
	}

	// Assert
	if !recordedAt(h, "info", "swept the merge state no queue entry names any more") {
		t.Fatal("the sweep's removals were not recorded")
	}
}

func TestRecoverGoesOnWhenTheSweepFailsAndSaysSoAtError(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.db.sweepErr = errors.New("disk full")

	// Act
	err := h.o.Recover(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("Recover = %v, want recovery to go on past a failed sweep", err)
	}
	if !recordedAt(h, "error", "could not sweep the merge state no queue entry names any more") {
		t.Fatal("the failed sweep was not recorded at ERROR")
	}
}

func TestRecoverRecordsEachMergeLeaseTheSweepReleasedWithItsWorkspaceAndTellsTheQueue(t *testing.T) {
	// Arrange: the lease an eviction left standing over no entry.
	h := newHarness(t)
	orphan := wsm.Lease{ID: "lease-orphan", Workspace: theWorkspace, Holder: wsm.HolderMerge}
	h.db.mu.Lock()
	h.db.leases[theWorkspace] = orphan
	h.db.swept = wsm.OrphanedMergeState{ReleasedLeases: []wsm.Lease{orphan}}
	h.db.mu.Unlock()

	// Act.
	if err := h.o.Recover(context.Background()); err != nil {
		t.Fatalf("Recover: %v", err)
	}

	// Assert.
	rec, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "info", Operation: "daemon.merge.recover", Message: "released a merge lease no queue entry names any more; the workspace takes prompts again"})
	if !ok || rec.Context["workspace"] != string(theWorkspace) || rec.Context["lease"] != "lease-orphan" {
		t.Fatalf("the released lease was recorded as %+v, want INFO naming its workspace and lease", rec)
	}
	h.queue.mu.Lock()
	defer h.queue.mu.Unlock()
	if n := len(h.queue.leaseEvents); n == 0 || h.queue.leaseEvents[n-1] {
		t.Fatalf("lease events %v, want the queue told the merge lease is gone", h.queue.leaseEvents)
	}
}

func TestLoggedLikeMatchesEveryFieldItIsGiven(t *testing.T) {
	// Arrange: one record.
	h := newHarness(t)
	h.logs.Global().Info("daemon.merge.test", "the merge did the thing", dlog.Context{"workspace": "ws-1", "round": 2})
	tests := []struct {
		name string
		want logLike
		hit  bool
	}{
		{name: "nothing asked", want: logLike{}, hit: true},
		{name: "level and op", want: logLike{level: "info", op: "daemon.merge.test"}, hit: true},
		{name: "part of the message", want: logLike{text: "did the"}, hit: true},
		{name: "a context value, any type", want: logLike{context: map[string]string{"workspace": "ws-1", "round": "2"}}, hit: true},
		{name: "another level", want: logLike{level: "error"}, hit: false},
		{name: "another op", want: logLike{op: "daemon.merge.other"}, hit: false},
		{name: "another message", want: logLike{text: "did nothing"}, hit: false},
		{name: "another context value", want: logLike{context: map[string]string{"workspace": "ws-2"}}, hit: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act / Assert.
			if got := loggedLike(h, tt.want); got != tt.hit {
				t.Fatalf("loggedLike(%+v) = %v, want %v", tt.want, got, tt.hit)
			}
		})
	}
}

func TestEveryContainingLogMatchRunsThroughLoggedLike(t *testing.T) {
	// Arrange.
	files, err := filepath.Glob("*_test.go")
	if err != nil {
		t.Fatal(err)
	}
	for _, file := range files {
		body, err := os.ReadFile(file)
		if err != nil {
			t.Fatalf("reading %s: %v", file, err)
		}

		// Act / Assert: a hand loop matching a message by containment is a
		// second matcher; only loggedLike loops so.
		if n := strings.Count(string(body), "strings.Contains(r.Message,"); n > 0 && file != "recover_test.go" {
			t.Fatalf("%s matches log records by hand %d times instead of loggedLike", file, n)
		}
	}
}

func TestRecoverUnderAPausedQueueRecordsThatTheMergeStaysSuspended(t *testing.T) {
	// Arrange: a merge a restart interrupted in its tests, on a paused queue.
	h := newHarness(t)
	interruptedAt(t, h, atStep(TabTests, 1))
	h.db.mu.Lock()
	h.db.paused[h.repoKey()] = true
	h.db.mu.Unlock()

	// Act.
	if err := h.o.Recover(context.Background()); err != nil {
		t.Fatalf("Recover: %v", err)
	}

	// Assert.
	rec, found := dlog.FindRecord(h.logs.Global().(*dlog.TestLogger).Records(), dlog.RecordQuery{
		Level: dlog.LevelInfo, Operation: "daemon.merge.recover", Message: "a suspended merge is taken back at the step it recorded; its queue is paused, so it stays suspended until the queue is resumed"})
	if !found || rec.Context["paused"] != true {
		t.Fatalf("recover record = %v (found %v), want that the merge stays suspended on its paused queue", rec, found)
	}
}

func TestRecoverUnderARunningQueueRecordsThatTheMergeResumes(t *testing.T) {
	// Arrange: a merge a restart interrupted in its tests.
	h := newHarness(t)
	interruptedAt(t, h, atStep(TabTests, 1))

	// Act.
	if err := h.o.Recover(context.Background()); err != nil {
		t.Fatalf("Recover: %v", err)
	}

	// Assert.
	rec, found := dlog.FindRecord(h.logs.Global().(*dlog.TestLogger).Records(), dlog.RecordQuery{
		Level: dlog.LevelInfo, Operation: "daemon.merge.recover", Message: "a merge a restart interrupted resumes at the step it recorded"})
	if !found || rec.Context["paused"] != false {
		t.Fatalf("recover record = %v (found %v), want that the merge resumes", rec, found)
	}
}

func TestRecoverRefusesTheBootWhenTheQueuesPauseCannotBeRead(t *testing.T) {
	// Arrange: a merge a restart interrupted, and a pause that cannot be read.
	h := newHarness(t)
	interruptedAt(t, h, atStep(TabTests, 1))
	h.db.mu.Lock()
	h.db.pausedReadErr = errors.New("database is locked")
	h.db.mu.Unlock()

	// Act.
	err := h.o.Recover(context.Background())

	// Assert: the boot is refused, recorded, and nothing was taken.
	if err == nil || !strings.Contains(err.Error(), "database is locked") {
		t.Fatalf("Recover = %v, want the unreadable pause", err)
	}
	rec, found := dlog.FindRecord(h.logs.Global().(*dlog.TestLogger).Records(), dlog.RecordQuery{
		Level: dlog.LevelError, Operation: "daemon.merge.recover"})
	if !found || rec.Context["repo"] != string(h.repoKey()) || rec.Context["error"] != "database is locked" {
		t.Fatalf("error record = %v (found %v), want the repo and the cause", rec, found)
	}
	if len(h.db.adopted) != 0 {
		t.Fatalf("adopted = %v, want no lease taken", h.db.adopted)
	}
}

// TestRecoverReArmsADetachedPickTheRestartCaughtBeforeItsHandOff is the
// 2026-10-20 boot loop: an async request with an instruction was recorded on
// its requester's row, and the daemon died before the fork handed it to its
// merge workspace. The row still names the requester and its detach, so the
// pick is not the requester's to begin: the request is re-armed, and its
// wait's end makes the hand-off that then begins the pick.
func TestRecoverReArmsADetachedPickTheRestartCaughtBeforeItsHandOff(t *testing.T) {
	// Arrange
	h := pickHarness(t)
	source := asyncSource
	source.Detach, source.DetachName, source.Pick = "req-1", "reconnect-fix", "the reconnect fix"
	if err := h.db.RequestMerge(context.Background(), h.repoKey(), theWorkspace, source, h.clock()); err != nil {
		t.Fatalf("RequestMerge: %v", err)
	}

	// Act
	err := h.o.Recover(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("Recover = %v, want the unhanded request re-armed", err)
	}
	if forks := h.detached.forks; len(forks) != 0 {
		t.Fatalf("forks before the wait = %v, want none", forks)
	}
	h.runWait(t)
	if forks := h.detached.forks; len(forks) != 1 || forks[0] != string(theWorkspace)+":reconnect-fix:"+asyncBranch {
		t.Fatalf("forks after the wait = %v, want the request handed off once", forks)
	}
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("admit: %v", err)
	}
	if entry, ok := h.entryOf(theMergeWorkspace); !ok || entry.Source.PickStartedAt == nil {
		t.Fatalf("merge workspace entry = %+v, %v; want its pick begun", entry, ok)
	}
}

func TestRecoverPutsARecordlessHeadBackAtItsOwnPlace(t *testing.T) {
	// Arrange: the head was interrupted before it took a step, with two
	// merges waiting behind it.
	h := newHarness(t)
	enqueue(t, h)
	queueAnother(t, h, "ws-2", "two")
	queueAnother(t, h, "ws-3", "three")
	interruptMerge(t, h)

	// Act.
	if err := h.o.Recover(context.Background()); err != nil {
		t.Fatalf("Recover failed: %v", err)
	}

	// Assert: it heads the line again, waiting, and was told requeued.
	queue, err := h.db.MergeQueue(context.Background(), h.repoKey())
	if err != nil || len(queue) != 3 || queue[0].Workspace != theWorkspace || queue[0].State != wsm.MergeQueued {
		t.Fatalf("queue = %+v, %v; want the interrupted head first, waiting", queue, err)
	}
	if kinds := h.events.kinds(); len(kinds) == 0 || kinds[len(kinds)-1] != QueueChangeRequeued {
		t.Fatalf("changes = %v, want the head reported requeued", kinds)
	}
}
