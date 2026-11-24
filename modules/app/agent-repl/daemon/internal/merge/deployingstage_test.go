package merge

import (
	"context"
	"errors"
	"slices"
	"testing"
	"time"

	repoconfigv1 "agentrepl/proto/repoconfig/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/wsm"
)

// deployingSince is when the harness's seeded landing began deploying.
var deployingSince = deployStarted.Add(-30 * time.Second)

// seedDeploying records theWorkspace's landing as queue REPO's merge still
// deploying since deployingSince, TIMED as its run saw the step begin.
func seedDeploying(t *testing.T, h *harness, repo wsm.RepoKey, timed bool) wsm.DeployingMerge {
	t.Helper()
	m := wsm.DeployingMerge{
		Repo: repo, Workspace: theWorkspace, Lease: "lease-deploying", Credited: theWorkspace, Name: "ws-one", Branch: "agent/one",
		TargetDir: h.targetD, ActiveSince: deployingSince.Add(-time.Minute), DeployingSince: deployingSince, Timed: timed,
	}
	if _, err := h.db.RecordDeployingMerge(context.Background(), m); err != nil {
		t.Fatalf("RecordDeployingMerge: %v", err)
	}
	return m
}

// deployingOf reads queue REPO's merge still deploying.
func deployingOf(t *testing.T, h *harness, repo wsm.RepoKey) *wsm.DeployingMerge {
	t.Helper()
	m, err := h.db.DeployingMergeOf(context.Background(), repo)
	if err != nil {
		t.Fatalf("DeployingMergeOf: %v", err)
	}
	return m
}

// deployingSteps counts the deploying steps the harness measured.
func deployingSteps(h *harness) int {
	n := 0
	for _, g := range h.meter.all() {
		if g == "step:"+string(footer.StepDeploying) {
			n++
		}
	}
	return n
}

// endedWith reports whether the harness recorded a deploying stage ended as
// its check ended OUTCOME.
func endedWith(h *harness, outcome string) bool {
	_, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "info", Operation: "daemon.merge.deploying",
		Message: "the landed merge's deploying stage ended with its deploy's check",
		Match:   func(r dlog.Record) bool { return r.Context["outcome"] == outcome }})
	return ok
}

func TestAChecksEndEndsTheLandedMergesDeployingStage(t *testing.T) {
	tests := []struct {
		name    string
		end     func(t *testing.T, h *harness)
		outcome string
	}{
		{name: "the mediator approves the deploy", outcome: MediationApproved, end: func(t *testing.T, h *harness) {
			if err := decideTold(h, Decision{Kind: MediationApprove, Summary: "fine"}); err != nil {
				t.Fatalf("approve: %v", err)
			}
		}},
		{name: "the mediator escalates the deploy", outcome: MediationEscalated, end: func(t *testing.T, h *harness) {
			if err := decideTold(h, Decision{Kind: MediationEscalate, Summary: "yours"}); err != nil {
				t.Fatalf("escalate: %v", err)
			}
		}},
		{name: "the mediator dispatches what the deploy broke", outcome: MediationDispatched, end: func(t *testing.T, h *harness) {
			if err := dispatched(h, Decision{}); err != nil {
				t.Fatalf("dispatch: %v", err)
			}
		}},
		{name: "the deploy changed nothing", outcome: MediationUnchanged, end: func(t *testing.T, h *harness) {
			if err := h.o.ReleaseUnchangedDeploy(context.Background(), deployStarted); err != nil {
				t.Fatalf("ReleaseUnchangedDeploy: %v", err)
			}
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			owe(t, h)
			withMediator(t, h)
			seedDeploying(t, h, mediationKey(h), true)

			// Act
			tt.end(t, h)

			// Assert
			if m := deployingOf(t, h, mediationKey(h)); m != nil {
				t.Fatalf("deploying merge %+v, want it ended with the check", m)
			}
			if n := deployingSteps(h); n != 1 {
				t.Fatalf("measured %d deploying steps (%v), want exactly one", n, h.meter.all())
			}
			if !endedWith(h, tt.outcome) {
				t.Fatalf("records = %v, want the stage ended %s", h.logs.Records(), tt.outcome)
			}
		})
	}
}

func TestANewerDeployReplacingTheCheckKeepsTheMergeDeploying(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	want := seedDeploying(t, h, mediationKey(h), true)

	// Act
	oweAt(t, h, deployStarted.Add(time.Minute))

	// Assert
	if m := deployingOf(t, h, mediationKey(h)); m == nil || m.Workspace != want.Workspace {
		t.Fatalf("deploying merge %+v, want it still deploying under the newer deploy's check", m)
	}
	if n := deployingSteps(h); n != 0 {
		t.Fatalf("measured %d deploying steps, want none before the check ends", n)
	}
}

func TestAnUntimedDeployingMergeEndsUnmeasured(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	withMediator(t, h)
	seedDeploying(t, h, mediationKey(h), false)

	// Act
	if err := decideTold(h, Decision{Kind: MediationApprove, Summary: "fine"}); err != nil {
		t.Fatalf("approve: %v", err)
	}

	// Assert
	if m := deployingOf(t, h, mediationKey(h)); m != nil {
		t.Fatalf("deploying merge %+v, want it ended", m)
	}
	if n := deployingSteps(h); n != 0 {
		t.Fatalf("measured %d deploying steps, want none for a step whose start no run saw", n)
	}
}

func TestADeployingStageThatCannotBeEndedIsRecordedAndStays(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	withMediator(t, h)
	seedDeploying(t, h, mediationKey(h), true)
	h.db.mu.Lock()
	h.db.deployingEndErr = errors.New("database is locked")
	h.db.mu.Unlock()

	// Act
	if err := decideTold(h, Decision{Kind: MediationApprove, Summary: "fine"}); err != nil {
		t.Fatalf("approve: %v", err)
	}

	// Assert
	if owedCheck(t, h) != nil {
		t.Fatal("the check was not released: ending the deploying stage must never hold the queue")
	}
	if m := deployingOf(t, h, mediationKey(h)); m == nil {
		t.Fatal("the deploying merge was dropped, want it kept for the next check's end")
	}
	if n := deployingSteps(h); n != 0 {
		t.Fatalf("measured %d deploying steps, want none", n)
	}
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: "daemon.merge.deploying",
		Message: "could not end the landed merge's deploying stage as its check ended; it is drawn deploying until the next check ends",
		Match: func(r dlog.Record) bool {
			return r.Context["outcome"] == MediationApproved && r.Context["error"] == "database is locked"
		}}); !ok {
		t.Fatalf("records = %v, want the failure at ERROR with its outcome and cause", h.logs.Records())
	}
}

// landsOwnCheckout scripts a landing into the daemon's own checkout, whose
// deploy hook asks for the rollout, its deploys CHECKED or not. Its admission
// stands it on the head slot, starting its active clock, as the durable
// queue's admission does.
func landsOwnCheckout(t *testing.T, h *harness, checks bool) {
	t.Helper()
	h.ownCheckout()
	h.db.mu.Lock()
	h.db.onAdmit = func(ws ids.WorkspaceID) {
		for i := range h.db.queues[h.repoKey()] {
			if entry := &h.db.queues[h.repoKey()][i]; entry.Workspace == ws {
				clock, _, err := wsm.StampClock(entry.Clock, wsm.MergeAdmitted, wsm.MergeSlotHead, deployingSince.Add(-time.Minute))
				if err != nil {
					t.Errorf("stamp the admitted head: %v", err)
				}
				entry.Slot, entry.Clock = wsm.MergeSlotHead, clock
			}
		}
	}
	h.db.mu.Unlock()
	if checks {
		h.mu.Lock()
		h.config.DeployChecks = &repoconfigv1.DeployChecks{RemediationsBeforeEscalation: theBound}
		h.mu.Unlock()
	}
	h.landsCleanly("abc123def4567")
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	h.gatePasses("daemon")
}

func TestALandingsTeardownSettlesItsDeployingStep(t *testing.T) {
	tests := []struct {
		name          string
		checks        bool
		recordErr     error
		seedReplaced  bool
		wantRecorded  bool
		wantMeasured  int
		wantErrorLine string
	}{
		{name: "a checked deploy is drawn deploying and measured at its check's end", checks: true, wantRecorded: true},
		{name: "an unchecked deploy's step is measured as the run leaves it", wantMeasured: 1},
		{name: "a record that fails measures the step as the run leaves it", checks: true, recordErr: errors.New("database is locked"), wantMeasured: 1,
			wantErrorLine: "could not record the landed merge as deploying until its check ends; its deploying step is measured to its run's end"},
		{name: "a landing replacing an earlier one still deploying measures the earlier's", checks: true, seedReplaced: true, wantRecorded: true, wantMeasured: 1},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			landsOwnCheckout(t, h, tt.checks)
			if tt.seedReplaced {
				seedDeploying(t, h, h.repoKey(), true)
			}
			h.db.mu.Lock()
			h.db.deployingRecordErr = tt.recordErr
			h.db.mu.Unlock()
			enqueue(t, h)

			// Act
			if err := h.admit(context.Background()); err != nil {
				t.Fatalf("the merge failed: %v", err)
			}

			// Assert
			h.db.mu.Lock()
			h.db.deployingRecordErr = nil
			h.db.mu.Unlock()
			m := deployingOf(t, h, h.repoKey())
			if tt.wantRecorded != (m != nil) {
				t.Fatalf("deploying merge %+v, want recorded %v", m, tt.wantRecorded)
			}
			if m != nil && (m.Workspace != theWorkspace || m.Name == "" || !m.Timed || m.DeployingSince.IsZero() || m.ActiveSince.IsZero() || m.TargetDir == "") {
				t.Fatalf("deploying merge %+v, want the landing recorded timed from its deploying step", m)
			}
			if n := deployingSteps(h); n != tt.wantMeasured {
				t.Fatalf("measured %d deploying steps (%v), want %d", n, h.meter.all(), tt.wantMeasured)
			}
			if tt.seedReplaced && !endedWith(h, MediationSuperseded) {
				t.Fatalf("records = %v, want the earlier landing's stage ended superseded", h.logs.Records())
			}
			if tt.wantErrorLine != "" {
				if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: "daemon.merge.deploying", Message: tt.wantErrorLine}); !ok {
					t.Fatalf("records = %v, want %q at ERROR", h.logs.Records(), tt.wantErrorLine)
				}
			}
		})
	}
}

func TestTheStatusListsTheMergeStillDeployingAtTheFront(t *testing.T) {
	// Arrange
	h := statusHarness(t)
	seedDeploying(t, h, h.repoKey(), true)

	// Act
	got, err := h.o.Status(context.Background(), nil)

	// Assert
	if err != nil || len(got) != 1 {
		t.Fatalf("Status = (%+v, %v), want one queue", got, err)
	}
	want := QueueMember{Workspace: theWorkspace, Name: "ws-one", Branch: "agent/one", Started: true, Step: footer.StepDeploying}
	if d := got[0].Deploying; d == nil || *d != want {
		t.Fatalf("deploying = %+v, want %+v", d, want)
	}
}

func TestAnUnreadableDeployingMergeFailsTheStatus(t *testing.T) {
	// Arrange
	h := statusHarness(t)
	h.db.mu.Lock()
	h.db.deployingReadErr = errors.New("database is locked")
	h.db.mu.Unlock()

	// Act
	_, err := h.o.Status(context.Background(), nil)

	// Assert
	if err == nil {
		t.Fatal("Status answered no error for an unreadable deploying merge")
	}
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: "daemon.merge.status",
		Message: "could not read a repository's landed merge still deploying"}); !ok {
		t.Fatalf("records = %v, want the failure at ERROR", h.logs.Records())
	}
}

func TestDeployingMemberCreditsAnAsyncMergeToItsRequester(t *testing.T) {
	tests := []struct {
		name          string
		async         bool
		wantRequester string
	}{
		{name: "an own-branch landing names no requester"},
		{name: "an async merge names its requester", async: true, wantRequester: "ws-asker"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			m := wsm.DeployingMerge{Workspace: "mws", Credited: "mws", Name: "land-x", Branch: "b", Async: tt.async}
			if tt.async {
				m.Credited = "ws-asker"
			}

			// Act
			got := deployingMember(m)

			// Assert
			if string(got.Requester) != tt.wantRequester || !got.Started || got.Step != footer.StepDeploying || got.Name != "land-x" {
				t.Fatalf("member %+v, want it running its deploying step, requester %q", got, tt.wantRequester)
			}
		})
	}
}

func TestTheRosterDrawsTheMergeStillDeployingWithItsDeployStageLive(t *testing.T) {
	// Arrange
	h := newHarness(t)
	m := seedDeploying(t, h, h.repoKey(), true)
	started := deployingSince.Add(-time.Minute)
	ended := deployingSince.Add(time.Second)
	h.db.mu.Lock()
	h.db.ledger[theWorkspace] = []wsm.MergeLedgerEntry{{Workspace: theWorkspace, Lease: m.Lease, Intervals: []wsm.TabInterval{
		{Round: 1, Kind: TabQueue, StartedAt: started, EndedAt: &deployingSince, Outcome: "succeeded"},
		{Round: 1, Kind: TabDeploy, StartedAt: deployingSince, EndedAt: &ended, Outcome: "succeeded"},
	}}}
	h.db.mu.Unlock()

	// Act
	h.o.publishRosterQueue(context.Background(), h.repoKey())

	// Assert
	d := h.sidebar.queueOf(h.repoKey()).Deploying
	if d == nil || d.Workspace != theWorkspace || !d.Admitted || d.Clock.Phase != wsm.MergeClockActive || !d.Clock.Since.Equal(m.ActiveSince) || d.Reveal == nil {
		t.Fatalf("deploying = %+v, want the landing drawn admitted on its active clock, revealing its bubble", d)
	}
	stages := d.Stages
	if len(stages) != 2 || stages[0].GetSucceeded() == nil || stages[1].GetLive().GetStartedAtMs() != deployingSince.UnixMilli() {
		t.Fatalf("stages = %v, want the queue settled and the deploy live since it began", stages)
	}
}

func TestAnUnreadableDeployingMergeTellsTheRosterNothing(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.db.mu.Lock()
	h.db.deployingReadErr = errors.New("database is locked")
	h.db.mu.Unlock()

	// Act
	h.o.publishRosterQueue(context.Background(), h.repoKey())

	// Assert
	if told := h.sidebar.toldCount(); told != 0 {
		t.Fatalf("the roster was told %d queues, want none", told)
	}
	if _, ok := h.recordFor(dlog.LevelError, "daemon.merge.roster_queue"); !ok {
		t.Fatal("the unreadable deploying merge was not recorded at ERROR")
	}
}

func TestDeployingViewKeepsOnlyTheLastDeployStageLive(t *testing.T) {
	// Arrange
	view := mergeView{stages: []viewStage{
		{kind: TabTests, state: stageSucceeded, startedMS: 1, endedMS: 2},
		{kind: TabDeploy, round: 1, state: stageFailed, startedMS: 3, endedMS: 4},
		{kind: TabDeploy, round: 2, state: stageSucceeded, startedMS: 5, endedMS: 6},
		{kind: TabPostDeploy, state: stageSucceeded, startedMS: 7, endedMS: 8},
	}}

	// Act
	got := deployingView(view)

	// Assert
	states := []stageState{got.stages[0].state, got.stages[1].state, got.stages[2].state, got.stages[3].state}
	if !slices.Equal(states, []stageState{stageSucceeded, stageFailed, stageLive, stageSucceeded}) || got.stages[2].endedMS != 0 {
		t.Fatalf("stages = %+v, want only the last deploy stage live", got.stages)
	}
	if view.stages[2].state != stageSucceeded {
		t.Fatal("deployingView changed the view it was handed")
	}
}

func TestEveryQueueTabDrawsTheMergeStillDeployingAhead(t *testing.T) {
	// Arrange
	h := statusHarness(t)
	h.registerOther("ws-asker", "ws-asker", "agent/asker")
	m := seedDeploying(t, h, h.repoKey(), true)
	entries, err := h.db.MergeQueue(context.Background(), h.repoKey())
	if err != nil {
		t.Fatal(err)
	}
	for i := range entries {
		entries[i].EnqueuedAt = deployStarted
	}

	// Act
	view, err := h.o.viewQueue(context.Background(), h.repoKey(), entries)

	// Assert
	if err != nil {
		t.Fatalf("viewQueue: %v", err)
	}
	snap := view.snapshot(entries, otherWorkspace)
	if len(snap.Ahead) != 2 {
		t.Fatalf("ahead = %v, want the deploying merge and the merge running", snap.Ahead)
	}
	front := snap.Ahead[0]
	merging := front.GetMerging()
	if front.GetWorkspace().GetRef().GetId() != string(theWorkspace) || front.GetLabel().GetText() != m.Name || merging == nil ||
		merging.GetActiveTab().GetText() != tabWord(TabDeploy) || merging.GetStageEnteredAtMs() != deployingSince.UnixMilli() || front.GetTarget().GetDir() != m.TargetDir {
		t.Fatalf("front = %v, want the deploying merge on its deploy tab since its step began", front)
	}
}
