package merge

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/gitclient"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// TestSameDirExcludesASiblingWorktree covers the self-reload's extra condition:
// two checkouts of one repository are not one directory.
func TestSameDirExcludesASiblingWorktree(t *testing.T) {
	tests := []struct {
		name string
		a, b string
		want bool
	}{
		{name: "the same directory", a: "/checkout", b: "/checkout", want: true},
		{name: "an unclean spelling of it", a: "/checkout", b: "/checkout/x/..", want: true},
		{name: "a sibling worktree", a: "/checkout", b: "/checkout-2", want: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: two directory spellings.
			a, b := tc.a, tc.b

			// Act.
			got := sameDir(a, b)

			// Assert.
			if got != tc.want {
				t.Fatalf("sameDir(%q, %q) = %v, want %v", a, b, got, tc.want)
			}
		})
	}
}

// TestPromptTabCannotPark covers the tab family's legality: the prompt tabs have
// no parked arm, because a pre-prompt failure fails the run and a post-prompt
// failure rides the terminal.
func TestPromptTabCannotPark(t *testing.T) {
	tests := []struct {
		name string
		kind string
	}{
		{name: "the pre-prompt tab", kind: TabPrePrompt},
		{name: "the post-prompt tab", kind: TabPostPrompt},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: a settled prompt tab.
			tab := promptTab(tc.kind, tabRound{kind: tc.kind, n: 1, started: time.UnixMilli(500)}.settled(1000, ""))

			// Act.
			kind := tabKindOf(tab)

			// Assert.
			if kind != tc.kind {
				t.Fatalf("the tab is %q, want %q", kind, tc.kind)
			}
			if tc.kind == TabPrePrompt {
				if _, ok := tab.GetPrePrompt().GetState().(*frontendv1.FeedMergeTabPrePrompt_Settled); !ok {
					t.Fatalf("the pre-prompt tab is %T, want settled", tab.GetPrePrompt().GetState())
				}
			} else {
				if _, ok := tab.GetPostPrompt().GetState().(*frontendv1.FeedMergeTabPostPrompt_Settled); !ok {
					t.Fatalf("the post-prompt tab is %T, want settled", tab.GetPostPrompt().GetState())
				}
			}
		})
	}
}

// TestPromptTabCarriesItsFailureSummary covers what a failed prompt tab draws:
// the daemon's one-line account, with the detail in the tab's own content.
func TestPromptTabCarriesItsFailureSummary(t *testing.T) {
	// Arrange: a failed pre-prompt tab.
	tab := promptTab(TabPrePrompt, tabRound{kind: TabPrePrompt, n: 1, started: time.UnixMilli(500)}.settled(1000, "the before-merge prompt did not complete"))

	// Act.
	settled := tab.GetPrePrompt().GetSettled()

	// Assert.
	if settled.GetFailed().GetSummary() != "the before-merge prompt did not complete" {
		t.Fatalf("the tab reads %q, want the composed summary", settled.GetFailed().GetSummary())
	}
}

// TestReadEscalationIgnoresAMissingRecord covers the ordinary case: no record
// means the agent is still working the problem.
func TestReadEscalationIgnoresAMissingRecord(t *testing.T) {
	// Arrange: a target with no record.
	dir := t.TempDir()

	// Act.
	_, escalated := readEscalation(dir)

	// Assert.
	if escalated {
		t.Fatal("a target with no record read as an escalation")
	}
}

// TestReadEscalationReturnsTheAgentsReason covers what the parked line carries:
// the agent's own words, which is what a human reads.
func TestReadEscalationReturnsTheAgentsReason(t *testing.T) {
	// Arrange: a record with a reason under the marker.
	dir := t.TempDir()
	body := EscalationMarker + "\nthe storage layer needs redesigning\n"
	if err := os.WriteFile(filepath.Join(dir, EscalationFile), []byte(body), 0o644); err != nil {
		t.Fatalf("writing the record: %v", err)
	}

	// Act.
	why, escalated := readEscalation(dir)

	// Assert.
	if !escalated || why != "the storage layer needs redesigning" {
		t.Fatalf("readEscalation answered (%q, %v), want the agent's reason", why, escalated)
	}
}

// TestTheAdmissionPauseRunsAfterTheDisplacedCapture covers the test seam's one
// guarantee: it is reached with the capture already durable, which is the
// window a crash has to land in.
func TestTheAdmissionPauseRunsAfterTheDisplacedCapture(t *testing.T) {
	// Arrange: a merge that displaces a turn, with the seam wired.
	h := newHarness(t)
	captured := false
	h.pauseAfterCapture = func(_ context.Context, ws ids.WorkspaceID) {
		// THE RUN IS NOT REGISTERED YET at this point, by design: the seam
		// sits between the capture and everything after it. What it can see is
		// the capture's own durable effect, which is the mark on the turn.
		turns, err := h.db.AllDisplacedTurns(context.Background())
		if err != nil {
			t.Errorf("AllDisplacedTurns inside the pause: %v", err)
			return
		}
		captured = ws == theWorkspace && len(turns) == 1
	}
	h.emacsRepo()
	h.displaceTurn("carry on with the refactor")
	h.landsCleanly("abc123def4567")
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	h.gatePasses("daemon")
	o, err := newOrchestrator(h.deps())
	if err != nil {
		t.Fatalf("building the orchestrator: %v", err)
	}
	h.o = o
	enqueue(t, h)

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}

	// Assert.
	if !captured {
		t.Fatal("the admission pause was never called, want it called once the displaced turn was captured")
	}
}

// TestNoAdmissionPauseIsCalledWhenTheSeamIsUnwired covers production, where the
// seam is nil and a run passes straight through the window.
func TestNoAdmissionPauseIsCalledWhenTheSeamIsUnwired(t *testing.T) {
	// Arrange: the harness's default deps leave PauseAfterCapture nil.
	h := newHarness(t)
	h.emacsRepo()
	h.displaceTurn("carry on with the refactor")
	h.landsCleanly("abc123def4567")
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	h.gatePasses("daemon")
	enqueue(t, h)

	// Act / Assert: the run completes with no pause to call.
	if h.o.deps.PauseAfterCapture != nil {
		t.Fatal("the production deps carry an admission pause, want nil")
	}
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}
}

// --- a git the daemon stopped is not a merge failure --------------------

// TestACancelledGitAtShutdownRecordsNoAbort is the caller half of the
// classification: at an orderly exit every in-flight git is cancelled, and
// aborting the run there wrote a merge FAILURE into the log and the bubble for
// something that never failed.
func TestACancelledGitAtShutdownRecordsNoAbort(t *testing.T) {
	// Arrange: the repository identification is the first git a run performs,
	// and here it reports the cancellation a shutdown causes.
	h := newHarness(t)
	h.git.sameRepoErr = &gitclient.Cancelled{
		Args: []string{"rev-parse", "--git-common-dir"}, Dir: h.targetD, Cause: context.Canceled,
	}
	enqueue(t, h)

	// Act. The pump's own policy decides whether one run's error rides up out
	// of admit; what this test is about is what the run RECORDED.
	_ = h.admit(context.Background())

	// Assert: nothing claims a failure.
	if _, found := recordAt(h, "info", "daemon.merge.abort"); found {
		t.Fatalf("a cancelled git was recorded as a merge abort; records = %v", h.logs.Records())
	}
	for _, record := range h.logs.Records() {
		if record.Level == "error" {
			t.Fatalf("a cancelled git produced an ERROR record: %+v", record)
		}
	}
}

// TestACancelledGitAtShutdownIsRecordedOnce keeps the cancellation diagnosable
// rather than silent: it is not an abort, but it IS in the log.
func TestACancelledGitAtShutdownIsRecordedOnce(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.git.sameRepoErr = &gitclient.Cancelled{
		Args: []string{"rev-parse", "--git-common-dir"}, Dir: h.targetD, Cause: context.Canceled,
	}
	enqueue(t, h)

	// Act.
	_ = h.admit(context.Background())

	// Assert.
	if _, found := recordAt(h, "info", "daemon.merge.stop"); !found {
		t.Fatalf("the cancellation was not recorded at all; records = %v", h.logs.Records())
	}
}

// TestARealGitFailureStillAborts is the guard on the other side: the
// classification must not have weakened what a genuine failure does.
func TestARealGitFailureStillAborts(t *testing.T) {
	// Arrange: a git that really failed, with an exit status of its own.
	h := newHarness(t)
	h.git.sameRepoErr = &gitclient.Error{
		Args: []string{"rev-parse", "--git-common-dir"}, Dir: h.targetD, ExitCode: 128,
		Stderr: "fatal: not a git repository\n",
	}
	enqueue(t, h)

	// Act.
	_ = h.admit(context.Background())

	// Assert.
	if _, found := recordAt(h, "error", "daemon.merge.fault"); !found {
		t.Fatalf("a real git failure was not recorded as a fault; records = %v", h.logs.Records())
	}
	if _, found := recordAt(h, "info", "daemon.merge.abort"); !found {
		t.Fatalf("a real git failure did not abort the run; records = %v", h.logs.Records())
	}
	if facts, _ := h.o.Facts(theWorkspace); facts.State != StateFailed {
		t.Fatalf("the merge is %q, want %q", facts.State, StateFailed)
	}
}

// recordAt finds the first captured record with that level and operation.
func recordAt(h *harness, level, operation string) (dlog.Record, bool) {
	for _, record := range h.logs.Records() {
		if record.Level == level && record.Operation == operation {
			return record, true
		}
	}
	return dlog.Record{}, false
}

// --- an admitted merge waits for the workspace to fall free --------------

// TestAnAdmittedMergeWaitsForTheWorkspaceToFallFree covers the merge's half of
// "an interrupt ends only the turn": the displaced turn was ended unforced, so
// its detached work runs on, and the merge WAITS for it on the fleet's
// freeness before it drives the session. It never stops that work, and a wait
// that ends without the workspace falling free aborts the merge loudly.
func TestAnAdmittedMergeWaitsForTheWorkspaceToFallFree(t *testing.T) {
	tests := []struct {
		name       string
		busy       bool
		awaitErr   error
		wantAwaits int
		wantLanded bool
		wantLevel  string
		wantOp     string
		wantText   string
	}{
		{
			name: "a free workspace is not waited on", wantLanded: true,
			wantLevel: "debug", wantOp: "daemon.merge.await_free", wantText: "the workspace is free; the merge proceeds",
		},
		{
			name: "a busy workspace holds the merge until it falls free", busy: true,
			wantAwaits: 1, wantLanded: true,
			wantLevel: "info", wantOp: "daemon.merge.await_free", wantText: "the workspace fell free; the merge proceeds",
		},
		{
			name: "a wait that fails aborts the merge", busy: true, awaitErr: errors.New("the watcher closed"),
			wantAwaits: 1,
			wantLevel:  "error", wantOp: "daemon.merge.fault", wantText: "the watcher closed",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.emacsRepo()
			h.displaceTurn("carry on with the refactor")
			h.landsCleanly("abc123def4567")
			h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
			h.gatePasses("daemon")
			h.freeness.busy = tt.busy
			h.freeness.awaitErr = tt.awaitErr
			if tt.busy {
				h.freeness.waiting = make(chan struct{})
				h.freeness.release = make(chan struct{})
			}
			enqueue(t, h)

			// Act
			done := admitAsync(h, context.Background())
			if tt.busy {
				select {
				case <-h.freeness.waiting:
				case <-done:
					t.Fatal("the merge ran to its end without waiting for the busy workspace to fall free")
				}
				if h.git.seen("merge_no_ff") {
					t.Fatal("the merge ran its git while the workspace was still busy")
				}
				h.mu.Lock()
				stopped := len(h.stoppedSessions)
				h.mu.Unlock()
				if stopped != 0 {
					t.Fatal("the merge stopped the session while waiting, want its detached work left running")
				}
				close(h.freeness.release)
			}
			// The pump's own policy decides whether one run's error rides up
			// out of it; what the run RECORDED is asserted below.
			<-done

			// Assert
			if got := h.freeness.awaits(); got != tt.wantAwaits {
				t.Fatalf("freeness waits = %d, want %d", got, tt.wantAwaits)
			}
			if got := h.git.seen("merge_no_ff"); got != tt.wantLanded {
				t.Fatalf("the merge ran merge_no_ff = %v, want %v", got, tt.wantLanded)
			}
			for _, record := range h.logs.Records() {
				if record.Level == tt.wantLevel && record.Operation == tt.wantOp &&
					(record.Message == tt.wantText || strings.Contains(fmt.Sprint(record.Context["summary"]), tt.wantText)) {
					return
				}
			}
			t.Fatalf("records = %+v, want %s %s %q", h.logs.Records(), tt.wantLevel, tt.wantOp, tt.wantText)
		})
	}
}

// --- a one-shot merge never kills the turn that asked for it --------------
//
// 2026-09-28, prompt-bubble-height: the agent asked for its merge from inside
// its own turn, and the admission's displacement ended that very turn ("the
// turn was interrupted") and marked it for resubmission.

// ledgeredRun is a run of the harness workspace whose lease has an open
// ledger, so its openTab and closeTab record intervals as a real run's do.
func ledgeredRun(t *testing.T, h *harness) *run {
	t.Helper()
	lease := wsm.Lease{ID: "lease-ledgered"}
	if err := h.db.OpenMergeLedger(context.Background(), theWorkspace, lease.ID); err != nil {
		t.Fatalf("opening the ledger: %v", err)
	}
	return &run{
		o: h.o, ws: theWorkspace, repo: h.repoKey(), lease: lease,
		startedMS:  h.clock().UnixMilli(),
		rounds:     map[string]int{},
		openRounds: map[string]tabRound{},
	}
}

// ledgerIntervals answers every interval the harness workspace's ledger holds.
func ledgerIntervals(h *harness) []wsm.TabInterval {
	entries, _ := h.db.MergeLedger(context.Background(), theWorkspace)
	var out []wsm.TabInterval
	for _, entry := range entries {
		out = append(out, entry.Intervals...)
	}
	return out
}

// TestATabRoundsLiveBadgeCarriesItsStart covers the live badge a round mints:
// it ticks from the instant the round opened.
func TestATabRoundsLiveBadgeCarriesItsStart(t *testing.T) {
	// Arrange.
	round := tabRound{kind: TabTests, n: 1, started: time.UnixMilli(1500)}

	// Act.
	state := round.live()

	// Assert.
	if state.settled || state.startedMS != 1500 {
		t.Fatalf("the live badge is %+v, want live from 1500", state)
	}
}

// TestATabRoundsSettledBadgeCarriesItsStartAndEnd covers the settled badge a
// round mints: its fixed run time is its end less the instant it opened.
func TestATabRoundsSettledBadgeCarriesItsStartAndEnd(t *testing.T) {
	// Arrange.
	round := tabRound{kind: TabTests, n: 1, started: time.UnixMilli(1500)}

	// Act.
	state := round.settled(9000, "it failed")

	// Assert.
	if !state.settled || state.startedMS != 1500 || state.endedMS != 9000 || state.failure != "it failed" {
		t.Fatalf("the settled badge is %+v, want 1500..9000 failed", state)
	}
}

// TestOpenTabStampsTheRoundWithItsLedgerStart covers the source of every tab's
// start: the round openTab answers carries exactly the instant its ledger
// interval recorded.
func TestOpenTabStampsTheRoundWithItsLedgerStart(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	r := ledgeredRun(t, h)

	// Act.
	round := r.openTab(context.Background(), TabTests)

	// Assert.
	intervals := ledgerIntervals(h)
	if len(intervals) != 1 || !intervals[0].StartedAt.Equal(round.started) || round.started.IsZero() {
		t.Fatalf("the round began at %v and the ledger holds %+v, want one interval starting at the round's start", round.started, intervals)
	}
}

// TestOpenTabMakesTheRoundTheActiveOne covers what the queue's front reads:
// the round opened last, with its own start.
func TestOpenTabMakesTheRoundTheActiveOne(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	r := ledgeredRun(t, h)
	r.openTab(context.Background(), TabRebasing)

	// Act.
	tests := r.openTab(context.Background(), TabTests)

	// Assert.
	if got := r.activeRound(); got != tests {
		t.Fatalf("the active round is %+v, want the tests round %+v", got, tests)
	}
}

// TestCloseTabRecordsTheRoundsOwnStart covers the interval's close: it keeps
// the start the round opened with, so the ledger's span and the tab's drawn
// run time agree.
func TestCloseTabRecordsTheRoundsOwnStart(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	r := ledgeredRun(t, h)
	round := r.openTab(context.Background(), TabCommitting)

	// Act.
	r.closeTab(context.Background(), round, "succeeded")

	// Assert.
	intervals := ledgerIntervals(h)
	closed := intervals[len(intervals)-1]
	if closed.EndedAt == nil || !closed.StartedAt.Equal(round.started) {
		t.Fatalf("the closing interval is %+v, want it ended and starting at %v", closed, round.started)
	}
}

// TestEveryPublishedTabCarriesItsRoundsLedgerStart covers every call site at
// once: across a run that rebases, fails its gate, fixes, tests again and
// commits, each tab push -- live and settled alike -- ships the start its
// round's ledger interval recorded.
func TestEveryPublishedTabCarriesItsRoundsLedgerStart(t *testing.T) {
	// Arrange: a gate that fails once and then passes.
	h := newHarness(t)
	h.emacsRepo()
	h.landsCleanly("abc123def4567")
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	h.gateFails("daemon")
	h.gatePasses("daemon")
	enqueue(t, h)

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}

	// Assert.
	starts := map[string]int64{}
	for _, interval := range ledgerIntervals(h) {
		starts[roundKey(interval.Kind, interval.Round)] = interval.StartedAt.UnixMilli()
	}
	checked := 0
	for _, row := range h.feed.rows {
		tab := row.Row.GetMergeTab()
		if tab == nil || tabKindOf(tab) == TabQueue {
			continue
		}
		kind := tabKindOf(tab)
		live, settled := tabBadgeOf(t, tab)
		got := live.GetStartedAtMs() + settled.GetStartedAtMs()
		want, ok := starts[roundKey(kind, int(tab.GetLabel().GetRound()))]
		if !ok || got != want {
			t.Fatalf("the %s round %d tab began at %d, want its ledger start %d", kind, tab.GetLabel().GetRound(), got, want)
		}
		checked++
	}
	if checked == 0 {
		t.Fatal("the run published no tab to check")
	}
}
