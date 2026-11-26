package merge

import (
	"context"
	"errors"
	"reflect"
	"strings"
	"testing"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/wsm"
)

// evicted queues the harness's own merge and evicts it, so its request
// stands off the queue, and makes its branch exist.
func evicted(t *testing.T, h *harness) wsm.OffQueueMerge {
	t.Helper()
	enqueue(t, h)
	if err := h.o.Evict(context.Background(), theWorkspace); err != nil {
		t.Fatalf("Evict: %v", err)
	}
	rec, kept, err := h.db.OffQueueMergeOf(context.Background(), theWorkspace)
	if err != nil || !kept {
		t.Fatalf("OffQueueMergeOf = %+v, %v, %v; want the evicted request", rec, kept, err)
	}
	branchExists(h, requestBranch(h.db.workspaces[theWorkspace], rec.Source))
	return rec
}

// branchExists makes BRANCH exist in the harness's repository.
func branchExists(h *harness, branch string) {
	h.git.mu.Lock()
	defer h.git.mu.Unlock()
	h.git.refs["refs/heads/"+branch] = "b4anch"
}

// refusedArm answers ERR's refusal arm, "" for an error that is none.
func refusedArm(err error) string {
	if refusal, ok := Refused(err); ok {
		return refusal.Arm
	}
	return ""
}

func TestRequeuePutsAnEvictedMergeBackInLineAsItself(t *testing.T) {
	// Arrange
	h := newHarness(t)
	rec := evicted(t, h)

	// Act
	requeued, err := h.o.Requeue(context.Background(), Requeue{Workspace: theWorkspace, By: RequestedByUser})

	// Assert: in line, as itself, the record gone, and the requeue recorded.
	if err != nil || !requeued.InLine || requeued.Position != 1 {
		t.Fatalf("Requeue = %+v, %v; want in line at 1", requeued, err)
	}
	entry, ok := h.entryOf(theWorkspace)
	if !ok || entry.State != wsm.MergeQueued || !reflect.DeepEqual(entry.Source, rec.Source) {
		t.Fatalf("queue entry = %+v (%v), want the same request queued", entry, ok)
	}
	if _, kept, _ := h.db.OffQueueMergeOf(context.Background(), theWorkspace); kept {
		t.Fatal("the requeue left the record standing")
	}
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: "daemon.merge.requeue",
		Message: "put a merge that left its queue back on it, as itself"}); !ok {
		t.Fatalf("the requeue was not recorded: %v", h.logs.Records())
	}
}

// A WORKSPACE HOLDS ONE MERGE, IN LINE OR OFF IT: a request from a
// workspace whose merge stands off its queue is refused with the cause, and
// changes nothing, whether the orchestrator's own read or the store's
// transaction finds the record.
func TestARequestBesideAMergeOffItsQueueIsRefused(t *testing.T) {
	tests := []struct {
		name      string
		arrange   func(t *testing.T, h *harness)
		wantCause string
	}{
		{name: "found before anything is recorded", arrange: func(t *testing.T, h *harness) { evicted(t, h) }, wantCause: "evicted"},
		{name: "found by the store's transaction", arrange: func(t *testing.T, h *harness) {
			h.db.enqueueErr = &wsm.MergeOffQueueError{Workspace: theWorkspace, Cause: causeFailed}
		}, wantCause: "failed"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			tt.arrange(t, h)
			recBefore, keptBefore, _ := h.db.OffQueueMergeOf(context.Background(), theWorkspace)

			// Act
			err := h.request(t, ownBranch, RequestedByAgent)

			// Assert: refused with why it left, nothing in line, the record
			// as it stood, and the refusal recorded as an answer.
			refusal, refused := Refused(err)
			if !refused || refusal.Arm != ArmMergeOffQueue {
				t.Fatalf("Enqueue = %v, want merge_off_queue", err)
			}
			if got := refusal.Fields["cause"]; !reflect.DeepEqual(got, OffQueueCause(tt.wantCause)) {
				t.Fatalf("cause = %v, want the %s arm", got, tt.wantCause)
			}
			if !strings.Contains(err.Error(), "agent-repld merge-queue requeue") {
				t.Fatalf("refusal %q does not name the requeue", err)
			}
			if _, ok := h.entryOf(theWorkspace); ok {
				t.Fatal("the refused request stands in the queue")
			}
			if recAfter, keptAfter, _ := h.db.OffQueueMergeOf(context.Background(), theWorkspace); keptAfter != keptBefore || !reflect.DeepEqual(recAfter, recBefore) {
				t.Fatalf("the refusal changed the record: %+v -> %+v", recBefore, recAfter)
			}
			if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: "daemon.merge.enqueue",
				Message: "refused a merge request beside the request standing off its queue"}); !ok {
				t.Fatalf("the refusal was not recorded: %v", h.logs.Records())
			}
		})
	}
}

func TestARequestWhoseOffQueueReadFailsIsARecordedError(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.db.offQueueErr = errors.New("the store is gone")

	// Act
	err := h.request(t, ownBranch, RequestedByAgent)

	// Assert: an error, not a refusal, recorded once, and nothing in line.
	if _, refused := Refused(err); err == nil || refused {
		t.Fatalf("Enqueue = %v, want the read's error", err)
	}
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: "daemon.merge.enqueue",
		Message: "could not read whether the workspace's merge stands off its queue"}); !ok {
		t.Fatalf("the failed read was not recorded: %v", h.logs.Records())
	}
	if _, ok := h.entryOf(theWorkspace); ok {
		t.Fatal("a request stands in the queue")
	}
}

func TestRequeueRecordsTheModelTheSessionResumesUnder(t *testing.T) {
	// Arrange
	h := newHarness(t)
	evicted(t, h)

	// Act
	_, err := h.o.Requeue(context.Background(), Requeue{Workspace: theWorkspace, By: RequestedByUser, Model: "claude-opus-5-5"})

	// Assert
	if err != nil {
		t.Fatalf("Requeue: %v", err)
	}
	if got := h.db.sessions[theWorkspace].Model; got != "claude-opus-5-5" {
		t.Fatalf("the session's recorded model = %q, want the requeue's", got)
	}
}

func TestRequeueRefusals(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, h *harness)
		req     Requeue
		want    string
	}{
		{name: "a workspace whose merge holds no request", arrange: func(*testing.T, *harness) {}, want: ArmUnknownMerge},
		{name: "a merge standing in line", arrange: func(t *testing.T, h *harness) { enqueue(t, h) }, want: ArmStillQueued},
		{name: "a branch that is gone", arrange: func(t *testing.T, h *harness) {
			rec := evicted(t, h)
			h.git.mu.Lock()
			delete(h.git.refs, "refs/heads/"+requestBranch(h.db.workspaces[theWorkspace], rec.Source))
			h.git.mu.Unlock()
		}, want: ArmBranchGone},
		{name: "a merge whose landing closed its workspace", arrange: func(t *testing.T, h *harness) {
			closeWith(h, wsm.CloseByMerge)
		}, want: ArmAlreadyLanded},
		{name: "a closed workspace", arrange: func(t *testing.T, h *harness) {
			closeWith(h, wsm.CloseByUser)
		}, want: ArmWorkspaceClosed},
		{name: "a placement behind a merge not in line", arrange: func(t *testing.T, h *harness) { evicted(t, h) },
			req: Requeue{PlaceBehind: "nobody"}, want: ArmPlacementBehindNotInLine},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			tt.arrange(t, h)
			before, beforeKept, _ := h.db.OffQueueMergeOf(context.Background(), theWorkspace)
			req := tt.req
			req.Workspace, req.By = theWorkspace, RequestedByUser

			// Act
			_, err := h.o.Requeue(context.Background(), req)

			// Assert: refused under its arm, the record unchanged, recorded.
			if refusedArm(err) != tt.want {
				t.Fatalf("Requeue = %v, want the %s refusal", err, tt.want)
			}
			after, afterKept, _ := h.db.OffQueueMergeOf(context.Background(), theWorkspace)
			if beforeKept != afterKept || !reflect.DeepEqual(before, after) {
				t.Fatalf("the refusal changed the record: %+v -> %+v", before, after)
			}
			if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: "daemon.merge.requeue"}); !ok {
				t.Fatalf("the refusal was not recorded: %v", h.logs.Records())
			}
		})
	}
}

// TestRequeueOfAWorkspaceParkedAtItsColdGate pins the owner request of
// 2026-11-20: a requeue at a cold gate goes in line skipped and answers the
// gate by a compaction, where it was refused cold_gated; and the ruling of
// 2026-11-21: that answer is made once, and its failure takes the merge off
// its queue as failed.
func TestRequeueOfAWorkspaceParkedAtItsColdGate(t *testing.T) {
	tests := []struct {
		name       string
		gated      bool
		compactErr error
		wantInLine bool
		wantMarked bool
		wantAsked  int
		wantRecord string
		wantOp     string
		wantLevel  string
	}{
		{name: "a parked workspace's merge goes in line skipped and its gate is compacted", gated: true, wantInLine: true, wantMarked: true, wantAsked: 1,
			wantLevel: dlog.LevelInfo, wantOp: "daemon.merge.requeue",
			wantRecord: "the requeued merge's workspace was parked at its cold gate; the gate is answered once by a compaction, and the merge resumes at its place once the session is up"},
		{name: "a gate that cannot be compacted takes the merge off its queue as failed", gated: true, compactErr: errors.New("no session"), wantAsked: 1,
			wantLevel: dlog.LevelError, wantOp: opColdGate,
			wantRecord: "the cold gate of a queued merge's workspace could not be answered by a compaction; the merge leaves its queue as failed, and the gate stands for the user"},
		{name: "a workspace at no gate is requeued unmarked and nothing is compacted", gated: false, wantInLine: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: the merge was evicted, and its session is parked or not.
			h := newHarness(t)
			evicted(t, h)
			h.mu.Lock()
			h.coldGated = map[ids.WorkspaceID]bool{theWorkspace: tt.gated}
			h.compactGateErr = tt.compactErr
			h.mu.Unlock()

			// Act
			_, err := h.o.Requeue(context.Background(), Requeue{Workspace: theWorkspace, By: RequestedByUser})

			// Assert
			if err != nil {
				t.Fatalf("Requeue = %v, want it accepted", err)
			}
			entry, ok := h.entryOf(theWorkspace)
			if ok != tt.wantInLine || (ok && (entry.ColdGated != tt.wantMarked || (tt.wantMarked && entry.State != wsm.MergeQueued))) {
				t.Fatalf("entry %+v (in line %v), want in line %v with cold gate mark %v, waiting when marked", entry, ok, tt.wantInLine, tt.wantMarked)
			}
			h.mu.Lock()
			asked := len(h.gatesCompacted)
			h.mu.Unlock()
			if asked != tt.wantAsked {
				t.Fatalf("asked to compact %d gates, want %d", asked, tt.wantAsked)
			}
			if tt.wantRecord == "" {
				return
			}
			record, found := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: tt.wantLevel, Operation: tt.wantOp, Message: tt.wantRecord})
			if !found || record.Context["workspace"] != string(theWorkspace) {
				t.Fatalf("records = %v, want %q with the workspace", h.logs.Records(), tt.wantRecord)
			}
			if tt.compactErr == nil && record.Context["cold_gated"] != true {
				t.Fatalf("record context %v, want the gate", record.Context)
			}
			if tt.compactErr != nil && record.Context["error"] != tt.compactErr.Error() {
				t.Fatalf("record context %v, want the cause %q", record.Context, tt.compactErr)
			}
		})
	}
}

// closeWith closes the harness workspace as BY closes it.
func closeWith(h *harness, by wsm.CloseCause) {
	h.db.mu.Lock()
	defer h.db.mu.Unlock()
	w := h.db.workspaces[theWorkspace]
	w.Closed, w.Closure = true, &wsm.Closure{By: by}
	h.db.workspaces[theWorkspace] = w
}

func TestRequeueWhoseRecordCannotBeReadIsAnErrorAtError(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.db.offQueueErr = errors.New("the state database is unreadable")

	// Act
	_, err := h.o.Requeue(context.Background(), Requeue{Workspace: theWorkspace, By: RequestedByUser})

	// Assert
	if err == nil || refusedArm(err) != "" {
		t.Fatalf("Requeue = %v, want the read's error", err)
	}
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: "daemon.merge.requeue",
		Message: "could not read the merge's request off its queue"}); !ok {
		t.Fatalf("no ERROR record: %v", h.logs.Records())
	}
}

func TestDiscardOfAWorkspacesOwnMergeDropsOnlyTheRequest(t *testing.T) {
	// Arrange
	h := newHarness(t)
	evicted(t, h)

	// Act
	err := h.o.Discard(context.Background(), theWorkspace)

	// Assert
	if err != nil {
		t.Fatalf("Discard = %v; want a workspace's own merge discarded", err)
	}
	if h.db.closed[theWorkspace] {
		t.Fatal("discarding a workspace's own merge closed the workspace")
	}
	if _, kept, _ := h.db.OffQueueMergeOf(context.Background(), theWorkspace); kept {
		t.Fatal("the discard left the request standing")
	}
}

func TestDiscardRefusals(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, h *harness)
		want    string
	}{
		{name: "a workspace whose merge holds no request", arrange: func(*testing.T, *harness) {}, want: ArmUnknownMerge},
		{name: "a merge standing in line", arrange: func(t *testing.T, h *harness) { enqueue(t, h) }, want: ArmStillQueued},
		{name: "a closed workspace", arrange: func(t *testing.T, h *harness) { closeWith(h, wsm.CloseByUser) }, want: ArmUnknownMerge},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			tt.arrange(t, h)

			// Act
			err := h.o.Discard(context.Background(), theWorkspace)

			// Assert
			if refusedArm(err) != tt.want {
				t.Fatalf("Discard = %v, want the %s refusal", err, tt.want)
			}
		})
	}
}

func TestStatusListsTheMergesOffTheQueue(t *testing.T) {
	// Arrange
	h := newHarness(t)
	evicted(t, h)

	// Act
	statuses, err := h.o.Status(context.Background(), nil)

	// Assert
	if err != nil || len(statuses) != 1 || len(statuses[0].OffQueue) != 1 {
		t.Fatalf("Status = %+v, %v; want the one merge off the queue", statuses, err)
	}
	off := statuses[0].OffQueue[0]
	if off.Workspace != theWorkspace || off.Cause != string(CauseUserDrop) {
		t.Fatalf("off-queue member = %+v, want the evicted own merge", off)
	}
}

func TestTheSidebarIsToldTheMergesOffTheQueue(t *testing.T) {
	// Arrange
	h := newHarness(t)
	enqueue(t, h)

	// Act
	if err := h.o.Evict(context.Background(), theWorkspace); err != nil {
		t.Fatalf("Evict: %v", err)
	}

	// Assert: the merge under its own workspace, in the sidebar's words.
	want := []sidebar.OffQueueEntry{{Workspace: theWorkspace, Cause: "evicted"}}
	if got := h.sidebar.offQueueTold(); !reflect.DeepEqual(got, want) {
		t.Fatalf("sidebar told %+v, want %+v", got, want)
	}
}

// TestEveryOffQueueCauseHasItsArmAndReadsEvicted pins each kept cause to its
// typed arm on the wire, and to the sidebar's one word, "evicted", whatever
// the cause.
func TestEveryOffQueueCauseHasItsArmAndReadsEvicted(t *testing.T) {
	for cause := range offQueueCauses {
		t.Run(cause, func(t *testing.T) {
			// Act and Assert.
			if OffQueueCause(cause).GetCause() == nil {
				t.Errorf("cause %q maps to no arm", cause)
			}
			if got := OffQueueWords(cause); got != "evicted" {
				t.Errorf("cause %q reads %q in the sidebar, want evicted", cause, got)
			}
		})
	}
}

func TestAnUndeclaredOffQueueCausePanics(t *testing.T) {
	for _, tt := range []struct {
		name string
		call func()
	}{
		{name: "its arm", call: func() { OffQueueCause("no_such_cause") }},
		{name: "its words", call: func() { OffQueueWords("no_such_cause") }},
	} {
		t.Run(tt.name, func(t *testing.T) {
			defer func() {
				if recover() == nil {
					t.Fatal("an undeclared cause did not panic")
				}
			}()
			tt.call()
		})
	}
}

// exitCauses names every cause constant this package declares, by its Go
// name, with its value: the table TestEveryExitCauseKeepsOrEndsItsRequest
// classifies. That it names every declared cause is a rule over the sources,
// held in reposcans (daemonrules.TestTheExitCauseTableNamesEveryDeclaredCause).
var exitCauses = map[string]string{
	"CauseUserDrop":             string(CauseUserDrop),
	"CauseUserDequeue":          string(CauseUserDequeue),
	"CauseWorkspaceClosed":      string(CauseWorkspaceClosed),
	"CauseDaemonShutdown":       string(CauseDaemonShutdown),
	"CauseAheadDidNotLand":      string(CauseAheadDidNotLand),
	"CauseAgentGaveUp":          string(CauseAgentGaveUp),
	"causeMerged":               causeMerged,
	"causeFailed":               causeFailed,
	"causeRestartFailed":        causeRestartFailed,
	"causeRestartResume":        causeRestartResume,
	"causeFinishFailed":         causeFinishFailed,
	"CauseColdGate":             string(CauseColdGate),
	"CauseBackloggedAtColdGate": string(CauseBackloggedAtColdGate),
	"CauseColdGateAnswerFailed": string(CauseColdGateAnswerFailed),
}

// notQueueExits are the causes that never take a queue row off: a run taken
// off at its workspace's cold gate leaves its run only, its entry staying at
// its place (wsm.RequeueColdGatedMerge).
var notQueueExits = map[string]bool{string(CauseColdGate): true}

// TestEveryExitCauseKeepsOrEndsItsRequest holds every cause a merge leaves its
// queue for to exactly one fate: it ends the request (wsm.EndsRequest), or it
// keeps it off the queue under a declared arm and words (OffQueueCause,
// OffQueueWords), or it is no queue exit at all. A cause with no fate would
// reach OffQueueWords undeclared and panic the daemon.
func TestEveryExitCauseKeepsOrEndsItsRequest(t *testing.T) {
	for name, cause := range exitCauses {
		t.Run(name, func(t *testing.T) {
			// Act
			_, kept := offQueueCauses[cause]
			fates := 0
			for _, fate := range []bool{wsm.EndsRequest(cause), kept, notQueueExits[cause]} {
				if fate {
					fates++
				}
			}

			// Assert
			if fates != 1 {
				t.Errorf("%s (%q): ends %v, kept %v, no exit %v; want exactly one fate", name, cause, wsm.EndsRequest(cause), kept, notQueueExits[cause])
			}
		})
	}
}

func TestARunningMergeWhoseWorkspaceClosedKeepsNoRequestAndPanicsNothing(t *testing.T) {
	// Arrange and Act: the merge, held inside its gate, abandoned because its
	// workspace closed, with its workspace not yet marked closed.
	h := ended(t, phaseRunning, resolutions[2])

	// Assert
	if _, ok := h.entryOf(theWorkspace); ok {
		t.Fatal("the abandoned merge is still queued")
	}
	if rec, kept, _ := h.db.OffQueueMergeOf(context.Background(), theWorkspace); kept {
		t.Fatalf("a merge whose workspace closed kept a request: %+v", rec)
	}
}

// landedScript scripts the harness's master and the evicted merge's branch at
// the two heads, and answers whether the merge's row is drawn once judged.
type landedScript struct {
	branchGone bool
	ancestor   bool
	equivalent bool
}

// offQueueLandedHarness evicts the harness's own merge and scripts git as
// SCRIPT says: master at "m1", the branch at "b1".
func offQueueLandedHarness(t *testing.T, script landedScript) (*harness, string) {
	t.Helper()
	h := newHarness(t)
	rec := evicted(t, h)
	branch := requestBranch(h.db.workspaces[theWorkspace], rec.Source)
	h.git.mu.Lock()
	defer h.git.mu.Unlock()
	h.git.defaultBranch = "master"
	h.git.refs["refs/heads/master"] = "m1"
	h.git.refs["refs/heads/"+branch] = "b1"
	if script.branchGone {
		delete(h.git.refs, "refs/heads/"+branch)
	}
	if h.git.ancestry == nil {
		h.git.ancestry = map[string]bool{}
	}
	h.git.ancestry["b1>m1"] = script.ancestor
	h.git.patchEquivalent = map[string]bool{"m1>b1": script.equivalent}
	return h, branch
}

// gitCalls counts the git calls named CALL the fake recorded.
func gitCalls(h *harness, call string) int {
	h.git.mu.Lock()
	defer h.git.mu.Unlock()
	n := 0
	for _, c := range h.git.calls {
		if c == call {
			n++
		}
	}
	return n
}

// drawnOffQueue answers whether the sidebar's last off-queue set draws the
// harness's merge.
func drawnOffQueue(h *harness) bool {
	for _, entry := range h.sidebar.offQueueTold() {
		if entry.Workspace == theWorkspace {
			return true
		}
	}
	return false
}

func TestAMergeOffItsQueueWithWorkOnMasterIsNotDrawn(t *testing.T) {
	tests := []struct {
		name      string
		script    landedScript
		wantShown bool
	}{
		{name: "a branch master holds by ancestry is left out", script: landedScript{ancestor: true}},
		{name: "a branch with one patch-equivalent commit among unlanded ones is left out", script: landedScript{equivalent: true}},
		{name: "a branch with nothing landed is drawn", wantShown: true},
		{name: "a branch that is gone is left out", script: landedScript{branchGone: true}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h, _ := offQueueLandedHarness(t, tt.script)

			// Act.
			h.o.refreshOffQueueLanded(context.Background())
			statuses, err := h.o.Status(context.Background(), nil)

			// Assert: the roster and the status agree, and the record stands.
			if err != nil {
				t.Fatalf("Status: %v", err)
			}
			if drawnOffQueue(h) != tt.wantShown || (len(statuses[0].OffQueue) == 1) != tt.wantShown {
				t.Fatalf("drawn %v, status lists %d; want shown %v", drawnOffQueue(h), len(statuses[0].OffQueue), tt.wantShown)
			}
			if _, kept, _ := h.db.OffQueueMergeOf(context.Background(), theWorkspace); !kept {
				t.Fatal("hiding the merge lost its record")
			}
		})
	}
}

func TestAHiddenMergeOffItsQueueIsStillRequeuedByName(t *testing.T) {
	// Arrange.
	h, _ := offQueueLandedHarness(t, landedScript{equivalent: true})
	h.o.refreshOffQueueLanded(context.Background())

	// Act.
	_, err := h.o.Requeue(context.Background(), Requeue{Workspace: theWorkspace, By: RequestedByUser})

	// Assert.
	if err != nil {
		t.Fatalf("Requeue of a hidden merge: %v", err)
	}
}

func TestTheLandedJudgementIsMadeAgainWhenMasterMoves(t *testing.T) {
	// Arrange: judged shown at master m1.
	h, _ := offQueueLandedHarness(t, landedScript{})
	h.o.refreshOffQueueLanded(context.Background())
	if !drawnOffQueue(h) {
		t.Fatal("the merge with nothing landed is not drawn")
	}
	asked := gitCalls(h, "any_patch_equivalent")

	// Act: a judgement at unchanged heads asks git nothing more; then master
	// moves to m2, which holds a patch-equivalent of the branch's work.
	h.o.refreshOffQueueLanded(context.Background())
	reused := gitCalls(h, "any_patch_equivalent") == asked
	h.git.mu.Lock()
	h.git.refs["refs/heads/master"] = "m2"
	h.git.patchEquivalent["m2>b1"] = true
	h.git.mu.Unlock()
	h.o.refreshOffQueueLanded(context.Background())

	// Assert.
	if !reused {
		t.Fatal("a judgement at unchanged heads asked git again")
	}
	if drawnOffQueue(h) {
		t.Fatal("the merge is still drawn after master took its work")
	}
}

func TestThePublishOfMergesOffTheQueueNeverAsksGit(t *testing.T) {
	// Arrange.
	h, _ := offQueueLandedHarness(t, landedScript{})
	before := gitCalls(h, "any_patch_equivalent") + gitCalls(h, "is_ancestor")

	// Act.
	h.o.publishOffQueue(context.Background())

	// Assert.
	if after := gitCalls(h, "any_patch_equivalent") + gitCalls(h, "is_ancestor"); after != before {
		t.Fatalf("the publish asked git %d times, want none", after-before)
	}
}

func TestTheDrainWaitsForTheOffQueueJudgementInFlightAndStartsNoOther(t *testing.T) {
	// Arrange: an asynchronous orchestrator whose judgement's read is held.
	h := newHarness(t)
	h.o.async = true
	h.db.offQueueHold, h.db.offQueueEntered = make(chan struct{}), make(chan struct{}, 1)
	h.o.kickOffQueueLanded()
	select {
	case <-h.db.offQueueEntered:
	case <-time.After(waitBound):
		t.Fatal("the judgement never read the merges off their queues")
	}
	drained := make(chan struct{})

	// Act
	go func() { h.o.Drain(context.Background()); close(drained) }()

	// Assert: the drain is held behind the read, then returns once it ends.
	select {
	case <-drained:
		t.Fatal("the drain returned with a judgement still reading the state client")
	case <-h.db.offQueueEntered:
		t.Fatal("a second judgement started during the drain")
	default:
	}
	close(h.db.offQueueHold)
	select {
	case <-drained:
	case <-time.After(waitBound):
		t.Fatal("the drain never returned once the judgement ended")
	}
	h.o.kickOffQueueLanded()
	h.o.live.Wait()
	h.db.mu.Lock()
	reads := h.db.offQueueReads
	h.db.mu.Unlock()
	if reads != 1 {
		t.Fatalf("off-queue reads = %d, want the one in flight and none after the drain", reads)
	}
}

func TestADrainWhoseOffQueueJudgementOutlivesItsBoundIsAnError(t *testing.T) {
	// Arrange: a judgement that never ends, and a drain bound already spent.
	h := newHarness(t)
	h.o.async = true
	h.o.drainBound = time.Nanosecond
	release := make(chan struct{})
	h.db.offQueueHold, h.db.offQueueEntered = release, make(chan struct{}, 1)
	h.o.kickOffQueueLanded()
	<-h.db.offQueueEntered

	// Act
	h.o.Drain(context.Background())

	// Assert
	close(release)
	h.o.live.Wait()
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: "daemon.merge.drain",
		Message: "the judgement of the merges off their queues outlived the drain's bound; the state client closes under it"}); !ok {
		t.Fatalf("records = %+v, want the overrun recorded at error", h.logs.Records())
	}
}
