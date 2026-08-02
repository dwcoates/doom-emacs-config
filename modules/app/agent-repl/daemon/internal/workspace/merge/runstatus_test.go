package merge

import (
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// pinnedClock returns a clock stuck at one instant, which is how a test drives
// the "two ticks in the same millisecond" edge deterministically.
func pinnedClock(at int64) func() int64 { return func() int64 { return at } }

func newTestRun(t *testing.T, sink StatusSink, now func() int64) *RunStatus {
	t.Helper()
	run, err := NewRunStatus(sink, t.Logf, "/ws/a", now)
	if err != nil {
		t.Fatalf("NewRunStatus: %v", err)
	}
	return run
}

// --- construction -------------------------------------------------------

func TestNewRunStatusRequiresEveryDependency(t *testing.T) {
	// Arrange.
	tests := []struct {
		name    string
		sink    StatusSink
		logf    func(string, ...any)
		ws      string
		now     func() int64
		wantErr bool
	}{
		{name: "complete", sink: &recordingSink{}, logf: t.Logf, ws: "/ws/a", now: testClock()},
		{name: "no sink", sink: nil, logf: t.Logf, ws: "/ws/a", now: testClock(), wantErr: true},
		{name: "no logger", sink: &recordingSink{}, logf: nil, ws: "/ws/a", now: testClock(), wantErr: true},
		{name: "no workspace", sink: &recordingSink{}, logf: t.Logf, ws: "", now: testClock(), wantErr: true},
		{name: "no clock", sink: &recordingSink{}, logf: t.Logf, ws: "/ws/a", now: nil, wantErr: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			run, err := NewRunStatus(tc.sink, tc.logf, tc.ws, tc.now)

			// Assert.
			if tc.wantErr {
				if err == nil {
					t.Fatalf("NewRunStatus() error = nil, want error")
				}
				return
			}
			if err != nil {
				t.Fatalf("NewRunStatus() error = %v", err)
			}
			if run.RunID() == "" {
				t.Fatal("NewRunStatus() minted an empty run id")
			}
		})
	}
}

// Two runs must never share an id: a frontend correlates progress on it, and a
// collision would blend two attempts' commit counts.
func TestTwoRunsGetDifferentIDs(t *testing.T) {
	// Arrange.
	sink := &recordingSink{}

	// Act.
	first, second := newTestRun(t, sink, testClock()), newTestRun(t, sink, testClock())

	// Assert.
	if first.RunID() == second.RunID() {
		t.Fatalf("two runs share the id %q", first.RunID())
	}
}

// --- the two timestamps -------------------------------------------------

// THE GUARANTEE: updated_at_ms is strictly increasing within a run, even when
// the clock does not move. Two statuses that shared an instant would be
// unorderable, and within one phase there is nothing else to order them by.
func TestUpdatedAtIsStrictlyIncreasingUnderAStoppedClock(t *testing.T) {
	// Arrange — a clock pinned to one instant.
	sink := &recordingSink{}
	run := newTestRun(t, sink, pinnedClock(5000))

	// Act — two ticks of the same phase.
	if err := run.PickingCurrent("first"); err != nil {
		t.Fatalf("PickingCurrent: %v", err)
	}
	if err := run.PickingCurrent("second"); err != nil {
		t.Fatalf("PickingCurrent: %v", err)
	}

	// Assert.
	if len(sink.statuses) != 2 {
		t.Fatalf("published %d statuses, want 2", len(sink.statuses))
	}
	if sink.statuses[1].GetUpdatedAtMs() <= sink.statuses[0].GetUpdatedAtMs() {
		t.Fatalf("updated_at_ms went %d then %d, want strictly increasing",
			sink.statuses[0].GetUpdatedAtMs(), sink.statuses[1].GetUpdatedAtMs())
	}
}

// The VIOLATION this guards: phase_started_at_ms must NOT move on a
// within-phase tick, or a frontend rendering "cherry-picking for 40s" resets
// the figure every time a commit lands.
func TestPhaseStartedAtSurvivesAWithinPhaseTick(t *testing.T) {
	// Arrange.
	sink := &recordingSink{}
	run := newTestRun(t, sink, testClock())
	if err := run.PickingCurrent("first"); err != nil {
		t.Fatalf("PickingCurrent: %v", err)
	}

	// Act.
	if err := run.PickingCurrent("second"); err != nil {
		t.Fatalf("PickingCurrent: %v", err)
	}

	// Assert.
	if got, want := sink.statuses[1].GetPhaseStartedAtMs(), sink.statuses[0].GetPhaseStartedAtMs(); got != want {
		t.Fatalf("phase_started_at_ms = %d after a within-phase tick, want the unchanged %d", got, want)
	}
}

// A phase CHANGE does move it: that is the instant the new phase was entered.
func TestPhaseStartedAtMovesOnAPhaseChange(t *testing.T) {
	// Arrange.
	sink := &recordingSink{}
	run := newTestRun(t, sink, testClock())
	if err := run.PickingCurrent("picking"); err != nil {
		t.Fatalf("PickingCurrent: %v", err)
	}

	// Act.
	if err := run.Conflict("abc123", "conflicted"); err != nil {
		t.Fatalf("Conflict: %v", err)
	}

	// Assert.
	if got, want := sink.statuses[1].GetPhaseStartedAtMs(), sink.statuses[0].GetPhaseStartedAtMs(); got == want {
		t.Fatalf("phase_started_at_ms stayed at %d across a phase change, want the new phase's own instant", got)
	}
}

// --- the admission arm --------------------------------------------------

// THE GUARANTEE: the `enqueued` ARM and the merge-axis TOKEN are separate. A
// head admission is `enqueued` to a frontend and merge_enqueuing on the axis,
// because the coarse phase must not tell every reader that a merge starting
// immediately is waiting on something.
func TestAHeadAdmissionPublishesTheEnqueuedArmOnTheEnqueuingToken(t *testing.T) {
	// Arrange.
	sink := &recordingSink{}
	run := newTestRun(t, sink, testClock())

	// Act.
	if err := run.Enqueued(PhaseMergeEnqueuing, 1, 1, "admitted at the head"); err != nil {
		t.Fatalf("Enqueued: %v", err)
	}

	// Assert.
	if got := sink.statuses[0].GetEnqueued(); got == nil {
		t.Fatalf("phase = %T, want the enqueued arm", sink.statuses[0].GetPhase())
	}
	if got := sink.got[0].phase; got != PhaseMergeEnqueuing {
		t.Fatalf("axis token = %q, want %q", got, PhaseMergeEnqueuing)
	}
}

// The VIOLATION EDGE: an `enqueued` arm on a phase that is not an admission
// would report a queue place for a run that is already cherry-picking.
func TestEnqueuedRefusesANonAdmissionPhase(t *testing.T) {
	// Arrange.
	sink := &recordingSink{}
	run := newTestRun(t, sink, testClock())

	// Act.
	err := run.Enqueued(PhaseMerging, 1, 1, "not an admission")

	// Assert.
	if err == nil {
		t.Fatal("Enqueued(merging) error = nil, want the non-admission phase refused")
	}
	if len(sink.statuses) != 0 {
		t.Fatalf("published %d statuses for a refused admission, want none", len(sink.statuses))
	}
}

// --- per-phase publish content ------------------------------------------

func TestEachPhasePublishesItsOwnPayload(t *testing.T) {
	// Arrange.
	plan := CommitPlan{Commits: []PlannedCommit{
		{SHA: "a" + "0000000000000000000000000000000000000", Short: "a00000000000", Subject: "first"},
		{SHA: "b" + "0000000000000000000000000000000000000", Short: "b00000000000", Subject: "second"},
	}}
	tests := []struct {
		name    string
		publish func(*RunStatus) error
		check   func(*testing.T, *frontendv1.MergeStatus)
	}{
		{
			name:    "enqueued carries its queue facts",
			publish: func(r *RunStatus) error { return r.Enqueued(PhaseMergeQueued, 2, 5, "queued") },
			check: func(t *testing.T, s *frontendv1.MergeStatus) {
				got := s.GetEnqueued()
				if got == nil {
					t.Fatalf("phase = %T, want enqueued", s.GetPhase())
				}
				if got.GetPosition() != 2 || got.GetDepth() != 5 {
					t.Fatalf("position/depth = %d/%d, want 2/5", got.GetPosition(), got.GetDepth())
				}
			},
		},
		{
			name:    "before_action carries the recorded prompt",
			publish: func(r *RunStatus) error { return r.BeforeAction("bump the version", "running") },
			check: func(t *testing.T, s *frontendv1.MergeStatus) {
				got := s.GetBeforeAction()
				if got == nil {
					t.Fatalf("phase = %T, want before_action", s.GetPhase())
				}
				if got.GetPrompt() != "bump the version" {
					t.Fatalf("prompt = %q, want the recorded action text", got.GetPrompt())
				}
			},
		},
		{
			name: "cherry_picking carries the plan and the current commit",
			publish: func(r *RunStatus) error {
				r.SetPlan(plan)
				return r.CherryPicking(plan.Commits[0], "picking")
			},
			check: func(t *testing.T, s *frontendv1.MergeStatus) {
				got := s.GetCherryPicking()
				if got == nil {
					t.Fatalf("phase = %T, want cherry_picking", s.GetPhase())
				}
				if got.GetCommitsTotal() != 2 || got.GetCommitsLanded() != 0 {
					t.Fatalf("total/landed = %d/%d, want 2/0", got.GetCommitsTotal(), got.GetCommitsLanded())
				}
				if got.GetCurrentSha() != "a00000000000" || got.GetCurrentSubject() != "first" {
					t.Fatalf("current = %q %q, want the plan's first commit", got.GetCurrentSha(), got.GetCurrentSubject())
				}
			},
		},
		{
			name: "testing carries the same commit context cherry_picking did",
			publish: func(r *RunStatus) error {
				r.SetPlan(plan)
				if err := r.CherryPicking(plan.Commits[0], "picking"); err != nil {
					return err
				}
				r.CommitLanded()
				return r.Testing("testing 1/2")
			},
			check: func(t *testing.T, s *frontendv1.MergeStatus) {
				got := s.GetTesting()
				if got == nil {
					t.Fatalf("phase = %T, want testing", s.GetPhase())
				}
				if got.GetCommitsLanded() != 1 || got.GetCurrentSha() != "a00000000000" {
					t.Fatalf("landed/current = %d/%q, want 1/a00000000000", got.GetCommitsLanded(), got.GetCurrentSha())
				}
			},
		},
		{
			name: "conflict names the commit it parked on",
			publish: func(r *RunStatus) error {
				r.SetPlan(plan)
				if err := r.CherryPicking(plan.Commits[1], "picking"); err != nil {
					return err
				}
				return r.Conflict("b00000000000", "conflicted")
			},
			check: func(t *testing.T, s *frontendv1.MergeStatus) {
				got := s.GetConflict()
				if got == nil {
					t.Fatalf("phase = %T, want conflict", s.GetPhase())
				}
				if got.GetConflictedSha() != "b00000000000" || got.GetConflictedSubject() != "second" {
					t.Fatalf("conflicted = %q %q, want the second plan commit", got.GetConflictedSha(), got.GetConflictedSubject())
				}
			},
		},
		{
			name:    "after_action carries the recorded prompt",
			publish: func(r *RunStatus) error { return r.AfterAction("write the changelog", "running") },
			check: func(t *testing.T, s *frontendv1.MergeStatus) {
				got := s.GetAfterAction()
				if got == nil {
					t.Fatalf("phase = %T, want after_action", s.GetPhase())
				}
				if got.GetPrompt() != "write the changelog" {
					t.Fatalf("prompt = %q, want the recorded action text", got.GetPrompt())
				}
			},
		},
		{
			name: "merged reports the plan size and no after-action error",
			publish: func(r *RunStatus) error {
				r.SetPlan(plan)
				return r.Merged("", "landed")
			},
			check: func(t *testing.T, s *frontendv1.MergeStatus) {
				got := s.GetMerged()
				if got == nil {
					t.Fatalf("phase = %T, want merged", s.GetPhase())
				}
				if got.GetCommitsTotal() != 2 {
					t.Fatalf("commits_total = %d, want 2", got.GetCommitsTotal())
				}
				if got.GetAfterActionError() != "" {
					t.Fatalf("after_action_error = %q, want empty", got.GetAfterActionError())
				}
			},
		},
		{
			name: "failed carries how far the run got",
			publish: func(r *RunStatus) error {
				r.SetPlan(plan)
				if err := r.CherryPicking(plan.Commits[0], "picking"); err != nil {
					return err
				}
				r.CommitLanded()
				return r.Failed("the suite broke")
			},
			check: func(t *testing.T, s *frontendv1.MergeStatus) {
				got := s.GetFailed()
				if got == nil {
					t.Fatalf("phase = %T, want failed", s.GetPhase())
				}
				if got.GetCause() != "the suite broke" {
					t.Fatalf("cause = %q, want the reason it failed", got.GetCause())
				}
				if got.GetCommitsLanded() != 1 || got.GetFailingSha() != "a00000000000" {
					t.Fatalf("landed/failing = %d/%q, want 1/a00000000000", got.GetCommitsLanded(), got.GetFailingSha())
				}
			},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			sink := &recordingSink{}
			run := newTestRun(t, sink, testClock())

			// Act.
			if err := tc.publish(run); err != nil {
				t.Fatalf("publish: %v", err)
			}

			// Assert.
			if len(sink.statuses) == 0 {
				t.Fatal("nothing was published")
			}
			last := sink.statuses[len(sink.statuses)-1]
			if last.GetRunId() != run.RunID() {
				t.Fatalf("run id = %q, want %q", last.GetRunId(), run.RunID())
			}
			tc.check(t, last)
		})
	}
}

// A merged run that could not deliver its after-action still reports MERGED —
// the commits are on the target — with the failure carried alongside.
func TestMergedCarriesTheAfterActionErrorWithoutFailingTheRun(t *testing.T) {
	// Arrange.
	sink := &recordingSink{}
	run := newTestRun(t, sink, testClock())

	// Act.
	if err := run.Merged("the parent refused the prompt", "landed"); err != nil {
		t.Fatalf("Merged: %v", err)
	}

	// Assert.
	last := sink.statuses[0]
	if last.GetMerged() == nil {
		t.Fatalf("phase = %T, want merged even though the after-action failed", last.GetPhase())
	}
	if got := last.GetMerged().GetAfterActionError(); got != "the parent refused the prompt" {
		t.Fatalf("after_action_error = %q, want the after-action's failure", got)
	}
}

// --- the sink contract ---------------------------------------------------

// A sink failure is surfaced, never swallowed: the transition it was meant to
// record is lost, and the caller has to decide what that means.
func TestAPublishSurfacesASinkFailure(t *testing.T) {
	// Arrange.
	sink := &recordingSink{failOn: PhaseMerged}
	run := newTestRun(t, sink, testClock())

	// Act.
	err := run.Merged("", "landed")

	// Assert.
	if err == nil {
		t.Fatal("Merged() error = nil, want the sink's failure surfaced")
	}
}

// A status with no phase set describes nothing, so it is refused rather than
// put on the wire for every frontend to special-case.
func TestAStatusWithNoPhaseIsRefused(t *testing.T) {
	// Arrange.
	sink := &recordingSink{}
	emitter := &statusEmitter{sink: sink, logf: t.Logf}

	// Act.
	err := emitter.emit("/ws/a", PhaseMerged, "landed", &frontendv1.MergeStatus{RunId: "run-1"})

	// Assert.
	if err == nil {
		t.Fatal("emit() error = nil, want an unset phase oneof refused")
	}
	if len(sink.statuses) != 0 {
		t.Fatalf("a phaseless status reached the sink: %+v", sink.statuses)
	}
}

// An unknown phase is a programming error at the call site, never a garbage
// state to record.
func TestAStatusForAnUnknownPhaseIsRefused(t *testing.T) {
	// Arrange.
	sink := &recordingSink{}
	emitter := &statusEmitter{sink: sink, logf: t.Logf}
	status := &frontendv1.MergeStatus{
		RunId: "run-1",
		Phase: &frontendv1.MergeStatus_Merged{Merged: &frontendv1.MergeStatusMerged{}},
	}

	// Act.
	err := emitter.emit("/ws/a", Phase("merge_sideways"), "landed", status)

	// Assert.
	if err == nil {
		t.Fatal("emit() error = nil, want the unknown phase refused")
	}
}
