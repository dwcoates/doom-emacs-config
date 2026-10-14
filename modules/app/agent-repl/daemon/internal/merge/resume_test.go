package merge

import (
	"context"
	"encoding/json"
	"errors"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"google.golang.org/protobuf/encoding/prototext"

	"agent-repld/internal/feedid"
	"agent-repld/internal/gitclient"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// The resumed merge's fixtures: the lease the dead run held, the tip it
// rebased onto, the branch's head before that rebase, and the gated head.
const (
	deadLease  wsm.LeaseID = "lease-dead-run"
	resumeTip              = "tip0000000000001"
	branchHead             = "head000000000001"
	gatedHead              = "gated00000000001"
	mergeSHA               = "merge0000000001"
	deadTurn   ids.TurnID  = "turn-dead-run"
)

// restart stands a fresh orchestrator over the harness's durable state -- the
// store, the trees and the feed all outlive a daemon -- as the next boot builds
// it.
func (h *harness) restart(t *testing.T) {
	t.Helper()
	o, err := newOrchestrator(h.deps())
	if err != nil {
		t.Fatalf("building the restarted orchestrator: %v", err)
	}
	h.o = o
	t.Cleanup(func() {
		gone := make(chan struct{})
		go func() { o.live.Wait(); close(gone) }()
		select {
		case <-gone:
		case <-time.After(5 * time.Second):
			t.Error("a resumed merge run outlived its test")
		}
	})
}

// interruptedAt leaves the store the way a daemon that died at one step of an
// Emacs-repo merge leaves it -- the entry admitted, the lease held, and the
// progress record naming the step, shaped by mutate -- and restarts.
func interruptedAt(t *testing.T, h *harness, mutate func(*progressDoc)) {
	t.Helper()
	h.emacsRepo()
	// THE LEASE IS THE LEDGER MINTED AT ENQUEUE, as the admission takes it.
	h.o.mu.Lock()
	h.o.ledgerOf[theWorkspace] = deadLease
	h.o.mu.Unlock()
	enqueue(t, h)
	ctx := context.Background()
	if err := h.db.AdmitMerge(ctx, h.repoKey(), theWorkspace); err != nil {
		t.Fatalf("admitting: %v", err)
	}
	if _, err := h.db.AcquireLeaseAs(ctx, theWorkspace, deadLease, wsm.HolderMerge, wsm.PolicyHold); err != nil {
		t.Fatalf("taking the lease: %v", err)
	}
	entries, _ := h.db.MergeQueue(ctx, h.repoKey())
	queued := entries[0].EnqueuedAt.UnixMilli()
	doc := progressDoc{
		Version: progressVersion, Step: TabQueue, Repo: string(h.repoKey()),
		QueuedMS: queued, StartedMS: queued + 1,
		Subject:   subjectDoc{Branch: "feature", Dir: h.sourceD, TargetDir: h.targetD, Closes: string(theWorkspace)},
		EmacsRepo: true,
		Rounds:    map[string]int{TabQueue: 1},
		Active:    roundDoc{Kind: TabQueue, N: 1, StartedMS: queued},
		Open:      []roundDoc{{Kind: TabQueue, N: 1, StartedMS: queued}},
		Facts:     factsDoc{Step: "enqueued"},
	}
	if mutate != nil {
		mutate(&doc)
	}
	encoded, err := json.Marshal(doc)
	if err != nil {
		t.Fatalf("encoding the record: %v", err)
	}
	if err := h.db.PutMergeProgress(ctx, wsm.MergeProgress{
		Workspace: theWorkspace, Lease: deadLease, UpdatedAt: h.clock(), Document: encoded,
	}); err != nil {
		t.Fatalf("recording the progress: %v", err)
	}
	h.restart(t)
}

// atStep stands a record at one step of the attempt, its tab round live.
func atStep(step string, round int) func(*progressDoc) {
	return func(d *progressDoc) {
		d.Step = step
		d.Rounds = map[string]int{TabQueue: 1, TabRebasing: 1, step: round}
		d.Active = roundDoc{Kind: step, N: round, StartedMS: d.StartedMS + 10}
		d.Open = []roundDoc{d.Active}
		d.TargetBranch, d.Tip, d.BranchHead = "master", resumeTip, branchHead
		d.Commits = toCommitDocs(commits(3))
	}
}

// resume recovers the restarted daemon's merges and runs the pump once, as
// the boot and its admission pump do.
func resume(t *testing.T, h *harness) {
	t.Helper()
	if err := h.o.Recover(context.Background()); err != nil {
		t.Fatalf("Recover: %v", err)
	}
	if err := h.admit(context.Background()); err != nil {
		t.Logf("the resumed merge ended on: %v", err)
	}
}

// headIDOf is the row identity of one lease's bubble head, as text.
func headIDOf(lease wsm.LeaseID) string {
	return prototext.Format(feedid.Encode(headRef(theWorkspace, lease)))
}

// deadHeadID is the row identity of the dead run's bubble head.
func deadHeadID() string { return headIDOf(deadLease) }

// headIDs answers the row identity of every bubble head pushed, as text.
func (f *fakeFeed) headIDs() []string {
	f.mu.Lock()
	defer f.mu.Unlock()
	var out []string
	for _, row := range f.rows {
		if row.Row.GetActivity().GetMerge() != nil {
			out = append(out, prototext.Format(row.Row.GetId()))
		}
	}
	return out
}

// headPublications reports every publication of a merge head, in order, with
// whether it was published pinned.
func (f *fakeFeed) headPublications() []synthesized {
	f.mu.Lock()
	defer f.mu.Unlock()
	var out []synthesized
	for _, row := range f.rows {
		if row.Row.GetActivity().GetMerge() != nil {
			out = append(out, row)
		}
	}
	return out
}

// livePublishedUnpinned reports the head publications that carry no result
// yet were published unpinned: a live head that would have settled early.
func livePublishedUnpinned(heads []synthesized) []string {
	var out []string
	for _, head := range heads {
		if _, live := head.Row.GetActivity().GetMerge().GetResult().(*frontendv1.FeedMerge_Update); live && !head.Pinned {
			out = append(out, prototext.Format(head.Row.GetId()))
		}
	}
	return out
}

// landsAfterResume scripts the rest of a merge that lands.
func landsAfterResume(h *harness) {
	h.git.refs["HEAD"] = resumeTip
	h.git.ancestry[resumeTip+">feature"] = true
	h.landsCleanly(mergeSHA)
	h.gatePasses("daemon")
}

func TestRecoverDrawsTheResumedMergesBubbleAtOnce(t *testing.T) {
	// Arrange: a merge a restart interrupted in its tests.
	h := newHarness(t)
	interruptedAt(t, h, atStep(TabTests, 1))

	// Act: the recovery alone, before the pump runs anything.
	if err := h.o.Recover(context.Background()); err != nil {
		t.Fatalf("Recover: %v", err)
	}

	// Assert: the dead run's own bubble is drawn live, and the footer stands
	// on the merge's step rather than on a failure.
	heads := h.feed.headIDs()
	if len(heads) == 0 || heads[len(heads)-1] != deadHeadID() || h.feed.heads()[len(heads)-1] != "update" {
		t.Fatalf("heads = %v / %v, want the dead run's bubble %s drawn live", heads, h.feed.heads(), deadHeadID())
	}
	if facts := h.footer.last(); facts.State != StateMerging {
		t.Fatalf("the footer stands at %q, want %q", facts.State, StateMerging)
	}
}

func TestRecoverDrawsTheResumedMergesHeadClockFromItsAdmission(t *testing.T) {
	// Arrange: a merge a restart interrupted in its tests, admitted one
	// millisecond after it was queued.
	h := newHarness(t)
	interruptedAt(t, h, atStep(TabTests, 1))
	queued := queuedAtMS(t, h, theWorkspace)

	// Act.
	if err := h.o.Recover(context.Background()); err != nil {
		t.Fatalf("Recover: %v", err)
	}

	// Assert.
	if got := lastHeadOf(t, h, theWorkspace).GetRuntime().GetStartedAtMs(); got != queued+1 {
		t.Fatalf("the resumed head's clock starts at %d, want the admission %d", got, queued+1)
	}
}

func TestRecoverCountsTheResumedMergeFromWhenItWasQueued(t *testing.T) {
	// Arrange: a merge a restart interrupted in its tests.
	h := newHarness(t)
	interruptedAt(t, h, atStep(TabTests, 1))
	queued := queuedAtMS(t, h, theWorkspace)

	// Act.
	if err := h.o.Recover(context.Background()); err != nil {
		t.Fatalf("Recover: %v", err)
	}

	// Assert: the restart does not move the instant the roster counts from.
	if got := h.footer.last().QueuedAt.UnixMilli(); got != queued {
		t.Fatalf("the resumed merge's facts are queued at %d, want the recorded %d", got, queued)
	}
}

func TestRecoverAdoptsTheResumedMergesLease(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	interruptedAt(t, h, nil)

	// Act.
	if err := h.o.Recover(context.Background()); err != nil {
		t.Fatalf("Recover: %v", err)
	}

	// Assert.
	if !slices.Contains(h.db.adopted, deadLease) {
		t.Fatalf("adopted = %v, want the dead run's lease", h.db.adopted)
	}
	if _, held, _ := h.db.Lease(context.Background(), theWorkspace); !held {
		t.Fatal("the recovery released the lease the resumed merge runs under")
	}
}

func TestRecoverRefusesTheBootOnAProgressRecordThatWillNotDecode(t *testing.T) {
	// Arrange: a record of a version this build cannot read.
	h := newHarness(t)
	interruptedAt(t, h, func(d *progressDoc) { d.Version = progressVersion + 1 })

	// Act.
	err := h.o.Recover(context.Background())

	// Assert.
	var decodeErr *wsm.DecodeError
	if !errors.As(err, &decodeErr) {
		t.Fatalf("Recover = %v, want the DecodeError refusing the boot", err)
	}
	if record, found := recordWith(h, "error", "daemon.merge.recover"); !found || record.Context["lease"] != string(deadLease) {
		t.Fatalf("the error record is %+v (found %v), want the refusal naming the lease", record, found)
	}
	if !slices.Equal(h.db.adopted, nil) {
		t.Fatalf("adopted = %v, want nothing adopted for a record that will not decode", h.db.adopted)
	}
}

func TestRecoverLeavesAResumedMergesDisplacedTurnToIt(t *testing.T) {
	// Arrange: the dead run displaced a user turn, which its record names.
	h := newHarness(t)
	h.db.mu.Lock()
	h.db.turns[theWorkspace] = append(h.db.turns[theWorkspace], wsm.Turn{ID: "turn-user", Workspace: theWorkspace, Text: "hi", Displaced: true})
	h.db.mu.Unlock()
	interruptedAt(t, h, func(d *progressDoc) { d.Displaced = &displacedDoc{Turn: "turn-user", Text: "hi"} })

	// Act.
	if err := h.o.Recover(context.Background()); err != nil {
		t.Fatalf("Recover: %v", err)
	}

	// Assert: the boot put nothing back; the resumed merge does, at its end.
	if len(h.queue.submissions) != 0 {
		t.Fatalf("submissions = %+v, want the displaced turn left to its merge", h.queue.submissions)
	}
}

func TestAMergeResumedAtTheQueueRunsToItsLanding(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	interruptedAt(t, h, nil)
	landsAfterResume(h)
	h.git.ancestry = map[string]bool{}
	h.git.refs["HEAD"] = resumeTip

	// Act.
	resume(t, h)

	// Assert.
	if !slices.Contains(h.git.fastForwards, h.targetD+"@"+mergeSHA) {
		t.Fatalf("fast-forwards = %v, want the target landed at %s", h.git.fastForwards, mergeSHA)
	}
}

func TestAResumedMergeDrawsIntoTheBubbleItAlreadyHad(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	interruptedAt(t, h, nil)
	landsAfterResume(h)
	h.git.ancestry = map[string]bool{}

	// Act.
	resume(t, h)

	// Assert: every head the resumed merge drew is the dead run's own.
	for _, id := range h.feed.headIDs() {
		if id != deadHeadID() {
			t.Fatalf("a head was drawn as %s, want every head to be the dead run's %s", id, deadHeadID())
		}
	}
}

func TestAMergeResumedMidRebaseAtAConflictHandsItToTheSession(t *testing.T) {
	// Arrange: the rebase stands stopped at a conflict the record never saw.
	h := newHarness(t)
	interruptedAt(t, h, atStep(TabRebasing, 1))
	h.git.standing = true
	h.git.onTip = commits(1)
	h.git.conflicted = [][]string{{"a.go"}}

	// Act.
	resume(t, h)

	// Assert.
	if len(h.queue.submissions) == 0 || h.queue.submissions[0].Origin != conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR {
		t.Fatalf("submissions = %+v, want the conflict handed to the session first", h.queue.submissions)
	}
}

func TestAMergeResumedMidRebaseAtABreakContinuesIt(t *testing.T) {
	// Arrange: the rebase stands stopped at a break.
	h := newHarness(t)
	interruptedAt(t, h, atStep(TabRebasing, 1))
	h.git.standing = true
	h.git.onTip = commits(1)
	h.git.rebaseTotal = 3
	h.git.rebaseReplayed = 1
	landsAfterResume(h)

	// Act.
	resume(t, h)

	// Assert.
	if slices.Contains(h.git.calls, "start_rebase") || !slices.Contains(h.git.calls, "continue_rebase") {
		t.Fatalf("calls = %v, want the standing rebase continued, never begun again", h.git.calls)
	}
}

func TestAMergeResumedAfterItsRebaseFinishedGoesToItsTests(t *testing.T) {
	// Arrange: no rebase stands, and the branch is on the tip.
	h := newHarness(t)
	interruptedAt(t, h, atStep(TabRebasing, 1))
	landsAfterResume(h)

	// Act.
	resume(t, h)

	// Assert.
	if slices.Contains(h.git.calls, "start_rebase") || slices.Contains(h.git.calls, "continue_rebase") {
		t.Fatalf("calls = %v, want no rebase command for a finished rebase", h.git.calls)
	}
	if len(h.runner.argv) != 1 {
		t.Fatalf("the gate ran %d times, want once", len(h.runner.argv))
	}
}

func TestAMergeResumedBeforeItsRebaseBeganBeginsIt(t *testing.T) {
	// Arrange: no rebase stands, and the branch is where it stood before.
	h := newHarness(t)
	interruptedAt(t, h, atStep(TabRebasing, 1))
	h.git.refs["feature"] = branchHead

	// Act.
	resume(t, h)

	// Assert.
	if !slices.Contains(h.git.calls, "start_rebase") {
		t.Fatalf("calls = %v, want the recorded rebase begun", h.git.calls)
	}
}

func TestAMergeResumedOnABranchThatContradictsItsRecordFailsNamingIt(t *testing.T) {
	// Arrange: no rebase stands, the branch is neither on the tip nor at its
	// recorded head.
	h := newHarness(t)
	interruptedAt(t, h, atStep(TabRebasing, 1))
	h.git.refs["feature"] = "elsewhere0000001"

	// Act.
	resume(t, h)

	// Assert.
	facts := h.footer.last()
	if facts.State != StateFailed || !strings.Contains(facts.Detail, "could not resume") || !strings.Contains(facts.Detail, short(branchHead)) {
		t.Fatalf("facts = %+v, want a failure naming what contradicted the record", facts)
	}
	record, found := recordWith(h, "error", "daemon.merge.resume")
	if !found || record.Context["step"] != TabRebasing || !strings.Contains(record.Context["contradiction"].(string), short(branchHead)) {
		t.Fatalf("the error record is %+v (found %v), want the step and the contradiction named", record, found)
	}
}

func TestAMergeResumedAtItsTestsRunsTheGateAgainInTheSameRound(t *testing.T) {
	// Arrange: the dead run was in round 2 of its tests.
	h := newHarness(t)
	interruptedAt(t, h, atStep(TabTests, 2))
	landsAfterResume(h)

	// Act.
	resume(t, h)

	// Assert.
	if runs := h.feed.labelsOfKind(rowTestsRun); !slices.Equal(runs, []string{"run 2"}) {
		t.Fatalf("runs drawn = %v, want the gate run again as run 2 alone", runs)
	}
}

func TestAMergeResumedAtItsTestsFailsWhenARebaseStands(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	interruptedAt(t, h, atStep(TabTests, 1))
	h.git.standing = true

	// Act.
	resume(t, h)

	// Assert.
	if facts := h.footer.last(); facts.State != StateFailed || !strings.Contains(facts.Detail, "a rebase stands") {
		t.Fatalf("facts = %+v, want a failure naming the standing rebase", facts)
	}
}

// atFix stands a record at fixing attempt 1, whose turn is deadTurn.
func atFix(d *progressDoc) {
	atStep(TabFixes, 1)(d)
	d.FixAttempt, d.Turn, d.FailingSuites = 1, string(deadTurn), []string{"daemon"}
}

// recordTurn stands the dead run's turn in the store, closed when how is set.
func recordTurn(h *harness, how *wsm.TurnClose) {
	h.db.mu.Lock()
	defer h.db.mu.Unlock()
	turn := wsm.Turn{ID: deadTurn, Workspace: theWorkspace, Close: how}
	if how != nil {
		at := time.Unix(1700000001, 0)
		turn.ClosedAt = &at
	}
	h.db.turns[theWorkspace] = append(h.db.turns[theWorkspace], turn)
}

func TestAMergeResumedAtAFixReattachesToItsRunningTurn(t *testing.T) {
	// Arrange: the fixing turn still runs in the session.
	h := newHarness(t)
	interruptedAt(t, h, atFix)
	recordTurn(h, nil)
	landsAfterResume(h)

	// Act.
	resume(t, h)

	// Assert.
	if !slices.Contains(h.awaitedTurns, deadTurn) {
		t.Fatalf("awaited = %v, want the dead run's turn waited for", h.awaitedTurns)
	}
	if len(h.queue.submissions) != 0 {
		t.Fatalf("submissions = %+v, want nothing submitted again", h.queue.submissions)
	}
}

func TestAMergeResumedAtAFixWhoseTurnEndedGoesOnWithoutWaiting(t *testing.T) {
	// Arrange: the fixing turn ended while the daemon was down.
	h := newHarness(t)
	interruptedAt(t, h, atFix)
	completed := wsm.CloseCompleted
	recordTurn(h, &completed)
	landsAfterResume(h)

	// Act.
	resume(t, h)

	// Assert.
	if slices.Contains(h.awaitedTurns, deadTurn) {
		t.Fatalf("awaited = %v, want no wait on a turn that already ended", h.awaitedTurns)
	}
}

func TestAMergeResumedAtAFixWhoseTurnNeverReachedTheSessionSubmitsIt(t *testing.T) {
	// Arrange: the record names a turn no store knows.
	h := newHarness(t)
	interruptedAt(t, h, atFix)
	landsAfterResume(h)

	// Act.
	resume(t, h)

	// Assert.
	if len(h.queue.submissions) == 0 || h.queue.submissions[0].Turn != deadTurn {
		t.Fatalf("submissions = %+v, want the fixing brief submitted under the recorded turn", h.queue.submissions)
	}
}

func TestAMergeResumedAtItsConflictReattachesAndContinuesTheRebase(t *testing.T) {
	// Arrange: the conflict resolution's turn still runs.
	h := newHarness(t)
	interruptedAt(t, h, func(d *progressDoc) {
		atStep(TabConflicts, 1)(d)
		d.Turn, d.ConflictFiles = string(deadTurn), []string{"a.go"}
	})
	recordTurn(h, nil)
	h.git.standing = true
	h.git.rebaseTotal = 3

	// Act.
	resume(t, h)

	// Assert.
	if !slices.Contains(h.awaitedTurns, deadTurn) || !slices.Contains(h.git.calls, "continue_rebase") {
		t.Fatalf("awaited %v, calls %v: want the resolution reattached and the rebase continued", h.awaitedTurns, h.git.calls)
	}
}

// atCommit stands a record at committing, the merge commit made.
func atCommit(d *progressDoc) {
	atStep(TabCommitting, 1)(d)
	d.Head, d.MergeCommit = gatedHead, mergeSHA
}

func TestAMergeResumedAfterItsFastForwardHasLanded(t *testing.T) {
	// Arrange: the target already stands at the recorded merge commit.
	h := newHarness(t)
	interruptedAt(t, h, atCommit)
	h.git.refs["HEAD"] = mergeSHA

	// Act.
	resume(t, h)

	// Assert.
	if slices.Contains(h.git.calls, "merge_no_ff") || slices.Contains(h.git.calls, "fast_forward") {
		t.Fatalf("calls = %v, want nothing made again for a landed merge", h.git.calls)
	}
	if facts := h.footer.last(); facts.State != StateMerged {
		t.Fatalf("the merge stands at %q, want %q", facts.State, StateMerged)
	}
}

func TestAMergeResumedBeforeItsFastForwardCommitsAgain(t *testing.T) {
	// Arrange: the target still stands at the tip.
	h := newHarness(t)
	interruptedAt(t, h, atCommit)
	h.git.refs["HEAD"] = resumeTip
	h.git.refs["feature"] = gatedHead
	h.landsCleanly("merge0000000002")

	// Act.
	resume(t, h)

	// Assert.
	if !slices.Contains(h.git.fastForwards, h.targetD+"@merge0000000002") {
		t.Fatalf("fast-forwards = %v, want the commit made again and landed", h.git.fastForwards)
	}
}

func TestAMergeResumedAtItsCommitWithAMovedBranchFailsNamingIt(t *testing.T) {
	// Arrange: the branch is not at the head the gate passed.
	h := newHarness(t)
	interruptedAt(t, h, atCommit)
	h.git.refs["HEAD"] = resumeTip
	h.git.refs["feature"] = "moved00000000001"

	// Act.
	resume(t, h)

	// Assert.
	if facts := h.footer.last(); facts.State != StateFailed || !strings.Contains(facts.Detail, short(gatedHead)) {
		t.Fatalf("facts = %+v, want a failure naming the gated head", facts)
	}
}

func TestAMergeResumedAtAPrePromptReattachesToItsTurn(t *testing.T) {
	// Arrange: the dead run was in its before-merge prompt.
	h := newHarness(t)
	h.configureActions([]string{"prepare"}, nil)
	interruptedAt(t, h, func(d *progressDoc) {
		d.Step, d.PromptIndex, d.Turn = TabPrePrompt, 0, string(deadTurn)
		d.Rounds = map[string]int{TabQueue: 1, TabPrePrompt: 1}
		d.Active = roundDoc{Kind: TabPrePrompt, N: 1, StartedMS: d.StartedMS}
		d.Open = []roundDoc{d.Active}
	})
	recordTurn(h, nil)
	landsAfterResume(h)
	h.git.ancestry = map[string]bool{}

	// Act.
	resume(t, h)

	// Assert.
	if !slices.Contains(h.awaitedTurns, deadTurn) {
		t.Fatalf("awaited = %v, want the prompt's turn waited for", h.awaitedTurns)
	}
	if rounds := h.feed.roundsOfKind(TabPrePrompt); !slices.Equal(rounds, []int{1}) {
		t.Fatalf("pre-prompt rounds = %v, want the one round the dead run drew", rounds)
	}
}

func TestAResumedMergeOnAPausedQueueWaitsForTheQueueToResume(t *testing.T) {
	// Arrange: a paused queue advances nothing (owner ruling, 2026-10-12).
	h := newHarness(t)
	interruptedAt(t, h, atStep(TabTests, 1))
	landsAfterResume(h)
	h.db.paused[h.repoKey()] = true

	// Act.
	resume(t, h)

	// Assert.
	if len(h.runner.argv) != 0 {
		t.Fatalf("the gate ran %d times on a paused queue, want the resumed merge to wait", len(h.runner.argv))
	}
}

func TestAResumedMergeRunsOnceItsPausedQueueIsResumed(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	interruptedAt(t, h, atStep(TabTests, 1))
	landsAfterResume(h)
	h.db.paused[h.repoKey()] = true
	resume(t, h)

	// Act.
	h.db.paused[h.repoKey()] = false
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the resumed merge ended on: %v", err)
	}

	// Assert.
	if len(h.runner.argv) != 1 {
		t.Fatalf("the gate ran %d times once the queue resumed, want the resumed merge run once", len(h.runner.argv))
	}
}

func TestAResumedMergeDropsItsRecordAtItsEnd(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	interruptedAt(t, h, atStep(TabTests, 1))
	landsAfterResume(h)

	// Act.
	resume(t, h)

	// Assert.
	if _, found, _ := h.db.MergeProgressOf(context.Background(), theWorkspace); found {
		t.Fatal("the progress record outlived the merge")
	}
}

func TestAMergeSuspendedMidGateResumesAfterARestartAndLands(t *testing.T) {
	// Arrange: a merge cut in its test gate by the daemon's exit.
	h := newHarness(t)
	h.emacsRepo()
	h.landsCleanly(mergeSHA)
	inGate, release := make(chan struct{}), make(chan struct{})
	t.Cleanup(func() { close(release) })
	h.runner.before = func() {
		close(inGate)
		<-release
	}
	enqueue(t, h)
	done := admitAsync(h, context.Background())
	<-inGate
	h.o.Drain(context.Background())
	<-done
	lease, _, _ := h.db.Lease(context.Background(), theWorkspace)
	h.runner.mu.Lock()
	h.runner.before = nil
	h.runner.mu.Unlock()
	h.gatePasses("daemon")
	// The tree as the dead run's finished rebase left it: the branch on the
	// target's tip.
	h.git.ancestry["0000000000000000000000000000000000000000>feature"] = true
	h.restart(t)

	// Act.
	resume(t, h)

	// Assert: the same bubble ends landed.
	if facts := h.footer.last(); facts.State != StateMerged {
		t.Fatalf("the merge stands at %q (%s), want it landed", facts.State, facts.Detail)
	}
	heads := h.feed.headIDs()
	if heads[len(heads)-1] != headIDOf(lease.ID) {
		t.Fatalf("the last head is %s, want the suspended run's bubble", heads[len(heads)-1])
	}
}

func TestACheckpointThatCannotBeWrittenStopsTheMerge(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.emacsRepo()
	h.db.progressErr = errors.New("disk full")

	// Act.
	admitted(t, h)

	// Assert.
	if facts := h.footer.last(); facts.State != StateFailed || !strings.Contains(facts.Detail, "progress") {
		t.Fatalf("facts = %+v, want the merge failed on its unwritten record", facts)
	}
	record, found := recordWith(h, "error", "daemon.merge.progress")
	if !found || record.Context["step"] != TabQueue || record.Context["error"] != "disk full" {
		t.Fatalf("the error record is %+v (found %v), want the step and the cause named", record, found)
	}
	if slices.Contains(h.git.calls, "start_rebase") {
		t.Fatalf("calls = %v, want no step taken without its record", h.git.calls)
	}
}

func TestDecodeProgressRefusesARecordWithNoStep(t *testing.T) {
	// Arrange.
	stored := wsm.MergeProgress{Workspace: theWorkspace, Lease: deadLease, Document: []byte(`{"version":1}`)}

	// Act.
	_, err := decodeProgress(stored)

	// Assert.
	var decodeErr *wsm.DecodeError
	if !errors.As(err, &decodeErr) {
		t.Fatalf("decodeProgress = %v, want a DecodeError", err)
	}
}

func TestTheProgressRecordCarriesACommitsAuthorAndTime(t *testing.T) {
	// Arrange.
	at := time.Unix(1700000000, 0).UTC()
	in := []gitclient.Commit{{SHA: "c1", Subject: "s", Author: "a", At: at}}

	// Act.
	out := fromCommitDocs(toCommitDocs(in))

	// Assert.
	if !slices.Equal(out, in) {
		t.Fatalf("round trip = %+v, want %+v", out, in)
	}
}

// fixingAttemptsDrawn answers the fixing attempts the footer drew, each once
// per run of the same number.
func fixingAttemptsDrawn(h *harness) []int {
	var attempts []int
	for _, f := range h.footer.all() {
		if f.Step == "fixing" && (len(attempts) == 0 || attempts[len(attempts)-1] != f.Attempt) {
			attempts = append(attempts, f.Attempt)
		}
	}
	return attempts
}

// atFixArchived is atFix with the failing run's archive on disk, which a
// fixing attempt prompted again reads its failing tests from.
func atFixArchived(t *testing.T) func(*progressDoc) {
	archive := filepath.Join(t.TempDir(), "tests-1.log")
	if err := os.WriteFile(archive, []byte("[agent-repl-tests] ERROR: daemon failed after 4s with exit code 1\n"), 0o644); err != nil {
		t.Fatalf("writing the archive: %v", err)
	}
	return func(d *progressDoc) {
		atFix(d)
		d.FailingArchive = archive
	}
}

func TestAResumedFixTheRestartOrphanedIsPromptedAgain(t *testing.T) {
	// Arrange: the fixing turn was reconciled with no terminal.
	h := newHarness(t)
	interruptedAt(t, h, atFixArchived(t))
	orphaned := wsm.CloseOrphaned
	recordTurn(h, &orphaned)
	landsAfterResume(h)

	// Act.
	resume(t, h)

	// Assert.
	if got := h.queue.countOrigin(conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_TEST_REPAIR); got != 1 {
		t.Fatalf("fixing briefs = %d, want the cut attempt prompted again once", got)
	}
}

func TestAResumedFixTheRestartCutSpendsNoAttempt(t *testing.T) {
	// Arrange: the fixing turn was reconciled with no terminal.
	h := newHarness(t)
	interruptedAt(t, h, atFixArchived(t))
	orphaned := wsm.CloseOrphaned
	recordTurn(h, &orphaned)
	landsAfterResume(h)

	// Act.
	resume(t, h)

	// Assert.
	if got := fixingAttemptsDrawn(h); !slices.Equal(got, []int{1}) {
		t.Fatalf("fixing attempts drawn = %v, want attempt 1 only", got)
	}
}

func TestAResumedFixWhoseAgentDiedWhileSuspendedIsPromptedAgain(t *testing.T) {
	// Arrange: the fixing turn's agent died while the daemon was down.
	h := newHarness(t)
	interruptedAt(t, h, atFixArchived(t))
	died := wsm.CloseAgentDied
	recordTurn(h, &died)
	landsAfterResume(h)

	// Act.
	resume(t, h)

	// Assert.
	if got := h.queue.countOrigin(conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_TEST_REPAIR); got != 1 {
		t.Fatalf("fixing briefs = %d, want the cut attempt prompted again once", got)
	}
}

// atConflict stands a record at a conflict resolution whose turn is deadTurn,
// attempts of the bound already spent on the conflicting commit.
func atConflict(attempts int) func(*progressDoc) {
	return func(d *progressDoc) {
		atStep(TabConflicts, 1)(d)
		d.Turn, d.ConflictFiles = string(deadTurn), []string{"a.go"}
		d.ConflictCommit, d.ConflictAttempts = commits(3)[0].SHA, attempts
	}
}

func TestAResumedConflictTheRestartOrphanedIsPromptedAgainWithoutSpendingAnAttempt(t *testing.T) {
	// Arrange: the bound's last attempt was in flight when the restart cut it.
	h := newHarness(t)
	interruptedAt(t, h, atConflict(MaxConflictAttempts))
	orphaned := wsm.CloseOrphaned
	recordTurn(h, &orphaned)
	h.git.standing = true
	h.git.rebaseTotal = 3
	h.git.conflicted = [][]string{{"a.go"}, nil}
	landsAfterResume(h)

	// Act.
	resume(t, h)

	// Assert.
	if got := h.queue.countOrigin(conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR); got != 1 {
		t.Fatalf("conflict briefs = %d, want the cut resolution prompted again", got)
	}
	if got := h.footer.last(); got.State == StateFailed {
		t.Fatalf("facts = %+v, want no failure: the cut attempt never counted", got)
	}
}

func TestAResumedConflictKeepsTheAttemptsItSpent(t *testing.T) {
	// Arrange: the bound's last attempt ended, unresolved, while the daemon
	// was down.
	h := newHarness(t)
	interruptedAt(t, h, atConflict(MaxConflictAttempts))
	completed := wsm.CloseCompleted
	recordTurn(h, &completed)
	h.git.standing = true
	h.git.rebaseTotal = 3
	h.git.conflicted = [][]string{{"a.go"}}

	// Act.
	resume(t, h)

	// Assert.
	if got := h.footer.last(); got.State != StateFailed || got.FailedArea != "conflicts" {
		t.Fatalf("facts = %+v, want the merge failed in conflicts: the restart forgot no attempt", got)
	}
}

// movedTip is the target's tip after it moved while the daemon was down.
const movedTip = "moved0000000tip"

// targetMovedWhileDown stands the target on movedTip, with the branch
// rebased onto it once the resumed merge rebases.
func targetMovedWhileDown(h *harness) {
	h.git.refs["HEAD"] = movedTip
	h.git.ancestry[movedTip+">feature"] = true
}

func TestAMergeResumedAtItsTestsRebasesOntoATargetThatMovedFirst(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	interruptedAt(t, h, atStep(TabTests, 1))
	landsAfterResume(h)
	targetMovedWhileDown(h)

	// Act.
	resume(t, h)

	// Assert: the one gate runs on the moved tip, none on the tip it left.
	if len(h.runner.argv) != 1 {
		t.Fatalf("the gate ran %d times, want once, after the rebase onto the moved tip", len(h.runner.argv))
	}
	if len(h.git.queueBases) != 1 || h.git.queueBases[0] != movedTip {
		t.Fatalf("merge commits made on %v, want one on the moved tip", h.git.queueBases)
	}
}

func TestAMergeResumedAtItsTestsSettlesTheCutRoundWhenTheTargetMoved(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	interruptedAt(t, h, atStep(TabTests, 1))
	if err := h.db.OpenMergeLedger(context.Background(), theWorkspace, deadLease); err != nil {
		t.Fatalf("OpenMergeLedger: %v", err)
	}
	landsAfterResume(h)
	targetMovedWhileDown(h)

	// Act.
	resume(t, h)

	// Assert.
	if got := testsRoundOutcome(h, 1); got != "restarted" {
		t.Fatalf("the cut tests round closed %q, want restarted", got)
	}
}

// testsRoundOutcome is how the ledger closed the given tests round.
func testsRoundOutcome(h *harness, round int) string {
	h.db.mu.Lock()
	defer h.db.mu.Unlock()
	for _, entries := range h.db.ledger {
		for _, entry := range entries {
			for _, interval := range entry.Intervals {
				if interval.Kind == TabTests && interval.Round == round && interval.EndedAt != nil {
					return interval.Outcome
				}
			}
		}
	}
	return ""
}

func TestAMergeResumedAtAFixRebasesOntoATargetThatMovedBeforeItsGate(t *testing.T) {
	// Arrange: the fixing turn ended while the daemon was down.
	h := newHarness(t)
	interruptedAt(t, h, atFix)
	closed := wsm.CloseCompleted
	recordTurn(h, &closed)
	landsAfterResume(h)
	targetMovedWhileDown(h)

	// Act.
	resume(t, h)

	// Assert: the fix's gate runs on the moved tip, none on the tip it left.
	if len(h.runner.argv) != 1 {
		t.Fatalf("the gate ran %d times, want once, after the rebase onto the moved tip", len(h.runner.argv))
	}
}
