package merge

import (
	"errors"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// errWatermarkStore stands in for a durable store that cannot record the run's
// updated_at_ms floor.
var errWatermarkStore = errors.New("watermark store unavailable")

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

// A resumed run keeps the name it was admitted under: a boot replay continues
// the merge the user submitted, and a fresh id would make it look abandoned.
func TestResumeRunStatusKeepsTheAdmittedID(t *testing.T) {
	// Arrange.
	sink := &recordingSink{}

	// Act.
	run, err := ResumeRunStatus(sink, t.Logf, "/ws/a", testClock(), "run-admitted", 0)

	// Assert.
	if err != nil {
		t.Fatalf("ResumeRunStatus: %v", err)
	}
	if got := run.RunID(); got != "run-admitted" {
		t.Fatalf("RunID = %q, want the admitted run-admitted", got)
	}
}

// The VIOLATION EDGE: resuming with no id is a caller that lost the very thing
// the resume exists to carry, and minting one silently would rename the run.
func TestResumeRunStatusRefusesAnEmptyID(t *testing.T) {
	// Arrange.
	sink := &recordingSink{}

	// Act.
	_, err := ResumeRunStatus(sink, t.Logf, "/ws/a", testClock(), "", 0)

	// Assert.
	if err == nil {
		t.Fatal("ResumeRunStatus(\"\") error = nil, want the empty id refused")
	}
}

// --- the cross-bounce watermark -----------------------------------------

// recordingWatermark is a StatusWatermarkSink that remembers every value it was
// handed, and can be made to fail.
type recordingWatermark struct {
	runIDs []string
	values []int64
	err    error
}

func (w *recordingWatermark) RecordStatusWatermark(runID string, updatedAtMs int64) error {
	if w.err != nil {
		return w.err
	}
	w.runIDs = append(w.runIDs, runID)
	w.values = append(w.values, updatedAtMs)
	return nil
}

// THE GUARANTEE: a resumed run publishes STRICTLY ABOVE everything the
// pre-bounce process published, whatever the wall clock did across the bounce.
//
// The stopped-clock case stands in for the backwards step: a clock reading at or
// below the watermark is exactly what an ntp correction or a settled container
// clock produces, and it is the reading that used to publish underneath the
// statuses already on the wire.
func TestResumedUpdatedAtClearsTheWatermarkUnderAStoppedClock(t *testing.T) {
	// Arrange — the bounce lost 3000ms of wall clock; the run had reached 9000.
	tests := []struct {
		name      string
		nowMs     int64
		watermark int64
		wantMin   int64
	}{
		{name: "clock stepped backwards", nowMs: 6000, watermark: 9000, wantMin: 9001},
		{name: "clock stopped on the watermark", nowMs: 9000, watermark: 9000, wantMin: 9001},
		{name: "clock advanced normally", nowMs: 12000, watermark: 9000, wantMin: 12000},
		{name: "no watermark recorded", nowMs: 12000, watermark: 0, wantMin: 12000},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			sink := &recordingSink{}
			run, err := ResumeRunStatus(sink, t.Logf, "/ws/a", pinnedClock(tc.nowMs), "run-admitted", tc.watermark)
			if err != nil {
				t.Fatalf("ResumeRunStatus: %v", err)
			}

			// Act.
			if err := run.PickingCurrent("resumed"); err != nil {
				t.Fatalf("PickingCurrent: %v", err)
			}

			// Assert.
			if got := sink.statuses[0].GetUpdatedAtMs(); got != tc.wantMin {
				t.Fatalf("resumed updated_at_ms = %d, want %d", got, tc.wantMin)
			}
		})
	}
}

// The watermark seeds the clock and nothing else: a resume that inherited the
// phase clock would report the dead process's phase age as the live one's.
func TestAResumedRunStartsItsPhaseClockAtItsFirstPublication(t *testing.T) {
	// Arrange.
	sink := &recordingSink{}
	run, err := ResumeRunStatus(sink, t.Logf, "/ws/a", pinnedClock(6000), "run-admitted", 9000)
	if err != nil {
		t.Fatalf("ResumeRunStatus: %v", err)
	}

	// Act.
	if err := run.PickingCurrent("resumed"); err != nil {
		t.Fatalf("PickingCurrent: %v", err)
	}

	// Assert.
	if got := sink.statuses[0].GetPhaseStartedAtMs(); got != 9001 {
		t.Fatalf("resumed phase_started_at_ms = %d, want the first publication's 9001", got)
	}
}

// A negative watermark cannot have come off a run's clock, so it is refused
// rather than used as a floor that lowers the resumed run's first status.
func TestResumeRunStatusRefusesANegativeWatermark(t *testing.T) {
	// Arrange.
	sink := &recordingSink{}

	// Act.
	_, err := ResumeRunStatus(sink, t.Logf, "/ws/a", testClock(), "run-admitted", -1)

	// Assert.
	if err == nil {
		t.Fatal("ResumeRunStatus(-1) error = nil, want the negative watermark refused")
	}
}

// Every published updated_at_ms reaches the durable floor, or the next resume
// seeds beneath the status it missed.
func TestEveryPublicationRecordsItsWatermark(t *testing.T) {
	// Arrange.
	sink := &recordingSink{}
	watermark := &recordingWatermark{}
	run := newTestRun(t, sink, pinnedClock(5000))
	if err := run.BindWatermark(watermark); err != nil {
		t.Fatalf("BindWatermark: %v", err)
	}

	// Act.
	if err := run.PickingCurrent("first"); err != nil {
		t.Fatalf("PickingCurrent: %v", err)
	}
	if err := run.PickingCurrent("second"); err != nil {
		t.Fatalf("PickingCurrent: %v", err)
	}

	// Assert.
	if got, want := watermark.values, []int64{5000, 5001}; len(got) != len(want) || got[0] != want[0] || got[1] != want[1] {
		t.Fatalf("recorded watermarks = %v, want %v", got, want)
	}
}

// The floor is keyed on the run, so the sink must be told which run it is
// raising: a value filed under the wrong name protects the wrong entry.
func TestTheRecordedWatermarkCarriesTheRunID(t *testing.T) {
	// Arrange.
	sink := &recordingSink{}
	watermark := &recordingWatermark{}
	run, err := ResumeRunStatus(sink, t.Logf, "/ws/a", pinnedClock(5000), "run-admitted", 0)
	if err != nil {
		t.Fatalf("ResumeRunStatus: %v", err)
	}
	if err := run.BindWatermark(watermark); err != nil {
		t.Fatalf("BindWatermark: %v", err)
	}

	// Act.
	if err := run.PickingCurrent("resumed"); err != nil {
		t.Fatalf("PickingCurrent: %v", err)
	}

	// Assert.
	if got := watermark.runIDs; len(got) != 1 || got[0] != "run-admitted" {
		t.Fatalf("recorded run ids = %v, want [run-admitted]", got)
	}
}

// THE VIOLATION EDGE: a status must not reach the sink above a floor that was
// never written down, because a bounce right after would resume beneath it.
func TestAFailedWatermarkRefusesThePublication(t *testing.T) {
	// Arrange.
	sink := &recordingSink{}
	run := newTestRun(t, sink, pinnedClock(5000))
	if err := run.BindWatermark(&recordingWatermark{err: errWatermarkStore}); err != nil {
		t.Fatalf("BindWatermark: %v", err)
	}

	// Act.
	err := run.PickingCurrent("first")

	// Assert.
	if err == nil {
		t.Fatal("PickingCurrent() error = nil, want the unrecordable watermark refused")
	}
	if len(sink.statuses) != 0 {
		t.Fatalf("published %d statuses past a failed watermark, want none", len(sink.statuses))
	}
}

// A refused publication must not burn an updated_at_ms either, or the run's
// stream develops a gap no receiver can account for.
func TestAFailedWatermarkLeavesTheClockUntouched(t *testing.T) {
	// Arrange.
	sink := &recordingSink{}
	watermark := &recordingWatermark{err: errWatermarkStore}
	run := newTestRun(t, sink, pinnedClock(5000))
	if err := run.BindWatermark(watermark); err != nil {
		t.Fatalf("BindWatermark: %v", err)
	}
	if err := run.PickingCurrent("refused"); err == nil {
		t.Fatal("PickingCurrent() error = nil, want the failure")
	}

	// Act — the store recovers.
	watermark.err = nil
	if err := run.PickingCurrent("retried"); err != nil {
		t.Fatalf("PickingCurrent: %v", err)
	}

	// Assert.
	if got := sink.statuses[0].GetUpdatedAtMs(); got != 5000 {
		t.Fatalf("updated_at_ms = %d after a refused publication, want the unburned 5000", got)
	}
}

// Two sinks would mean two durable floors for one run, and the resume would seed
// from whichever the queue happened to read.
func TestBindWatermarkRefusesASecondSink(t *testing.T) {
	// Arrange.
	run := newTestRun(t, &recordingSink{}, testClock())
	if err := run.BindWatermark(&recordingWatermark{}); err != nil {
		t.Fatalf("BindWatermark: %v", err)
	}

	// Act.
	err := run.BindWatermark(&recordingWatermark{})

	// Assert.
	if err == nil {
		t.Fatal("BindWatermark() twice error = nil, want the rebind refused")
	}
}

// A nil sink binds nothing while looking bound, which would silently leave the
// run publishing above a floor nobody records.
func TestBindWatermarkRefusesANilSink(t *testing.T) {
	// Arrange.
	run := newTestRun(t, &recordingSink{}, testClock())

	// Act.
	err := run.BindWatermark(nil)

	// Assert.
	if err == nil {
		t.Fatal("BindWatermark(nil) error = nil, want the nil sink refused")
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

// THE REGRESSION THIS PINS: cherry_picking and testing both ride PhaseMerging,
// so a clock keyed on the merge AXIS left the testing arm claiming to have begun
// when the pick before it did. A frontend rendering "testing for 40s" was
// reading the age of the cherry-pick.
func TestPhaseStartedAtMovesWhenTheArmChangesWithinOneAxisPhase(t *testing.T) {
	// Arrange — a pick published on the merging axis.
	sink := &recordingSink{}
	run := newTestRun(t, sink, testClock())
	if err := run.PickingCurrent("picking"); err != nil {
		t.Fatalf("PickingCurrent: %v", err)
	}

	// Act — the testing arm, on that SAME axis phase.
	if err := run.Testing("testing"); err != nil {
		t.Fatalf("Testing: %v", err)
	}

	// Assert.
	if sink.got[0].phase != sink.got[1].phase {
		t.Fatalf("the two statuses rode axis phases %s then %s; this edge needs them to share one",
			sink.got[0].phase, sink.got[1].phase)
	}
	if got, want := sink.statuses[1].GetPhaseStartedAtMs(), sink.statuses[0].GetPhaseStartedAtMs(); got <= want {
		t.Fatalf("phase_started_at_ms = %d for the testing arm, want later than the cherry_picking arm's %d", got, want)
	}
}

// The VIOLATION EDGE for the same clock: an arm change under a STOPPED clock
// must still land strictly later, or the new arm is indistinguishable from the
// one it replaced.
func TestPhaseStartedAtAdvancesOnAnArmChangeUnderAStoppedClock(t *testing.T) {
	// Arrange — a clock pinned to one instant.
	sink := &recordingSink{}
	run := newTestRun(t, sink, pinnedClock(5000))
	if err := run.PickingCurrent("picking"); err != nil {
		t.Fatalf("PickingCurrent: %v", err)
	}

	// Act.
	if err := run.Testing("testing"); err != nil {
		t.Fatalf("Testing: %v", err)
	}

	// Assert.
	if got, want := sink.statuses[1].GetPhaseStartedAtMs(), sink.statuses[0].GetPhaseStartedAtMs(); got <= want {
		t.Fatalf("phase_started_at_ms = %d under a stopped clock, want strictly later than %d", got, want)
	}
}

// The other half of the keying rule: a REPEATED arm leaves the phase clock
// alone even when the axis phase is the same, so "cherry-picking for 40s" does
// not reset every time a commit lands.
func TestPhaseStartedAtHoldsAcrossARepeatedArmOnAnotherCommit(t *testing.T) {
	// Arrange.
	plan := CommitPlan{Commits: []PlannedCommit{
		{SHA: "a", Short: "a00000000000", Subject: "first"},
		{SHA: "b", Short: "b00000000000", Subject: "second"},
	}}
	sink := &recordingSink{}
	run := newTestRun(t, sink, testClock())
	run.SetPlan(plan)
	if err := run.CherryPicking(plan.Commits[0], "picking 1/2"); err != nil {
		t.Fatalf("CherryPicking: %v", err)
	}

	// Act — the same arm for the NEXT commit.
	if err := run.CherryPicking(plan.Commits[1], "picking 2/2"); err != nil {
		t.Fatalf("CherryPicking: %v", err)
	}

	// Assert.
	if got, want := sink.statuses[1].GetPhaseStartedAtMs(), sink.statuses[0].GetPhaseStartedAtMs(); got != want {
		t.Fatalf("phase_started_at_ms = %d on a repeated cherry_picking arm, want the unchanged %d", got, want)
	}
}

// --- the resumed cursor -------------------------------------------------

func TestResumePlanRebuildsTheCursor(t *testing.T) {
	// Arrange.
	commits := func(n int) CommitPlan {
		var plan CommitPlan
		for i := 0; i < n; i++ {
			plan.Commits = append(plan.Commits, PlannedCommit{SHA: string(rune('a' + i))})
		}
		return plan
	}
	tests := []struct {
		name       string
		setPlan    int // the plan the run already established, 0 for a rebuilt publisher
		full       int
		remaining  int
		wantTotal  int32
		wantLanded int32
	}{
		{name: "a publisher that lost its cursor counts the whole range", setPlan: 0, full: 3, remaining: 3, wantTotal: 3, wantLanded: 0},
		{name: "a publisher that lost its cursor mid-run counts what is gone as landed", setPlan: 0, full: 3, remaining: 1, wantTotal: 3, wantLanded: 2},
		{name: "a run that still knows its total keeps it", setPlan: 2, full: 5, remaining: 1, wantTotal: 2, wantLanded: 1},
		{name: "a run with nothing left reports its total fully landed", setPlan: 1, full: 4, remaining: 0, wantTotal: 1, wantLanded: 1},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			sink := &recordingSink{}
			run := newTestRun(t, sink, testClock())
			if tc.setPlan > 0 {
				run.SetPlan(commits(tc.setPlan))
			}

			// Act.
			if err := run.ResumePlan(commits(tc.full), commits(tc.remaining)); err != nil {
				t.Fatalf("ResumePlan: %v", err)
			}
			if err := run.Merged("", "landed"); err != nil {
				t.Fatalf("Merged: %v", err)
			}

			// Assert.
			if got := sink.statuses[0].GetMerged().GetCommitsTotal(); got != tc.wantTotal {
				t.Fatalf("commits_total = %d, want %d", got, tc.wantTotal)
			}
			if got := run.commitsLanded; got != tc.wantLanded {
				t.Fatalf("commits_landed = %d, want %d", got, tc.wantLanded)
			}
		})
	}
}

// The VIOLATION EDGE: a remainder larger than the run's total describes no run
// at all, and clamping it would put a fabricated progress figure on the wire.
func TestResumePlanRefusesARemainderLargerThanTheTotal(t *testing.T) {
	// Arrange — a run that established a one-commit plan.
	sink := &recordingSink{}
	run := newTestRun(t, sink, testClock())
	run.SetPlan(CommitPlan{Commits: []PlannedCommit{{SHA: "a"}}})

	// Act.
	err := run.ResumePlan(
		CommitPlan{Commits: []PlannedCommit{{SHA: "a"}}},
		CommitPlan{Commits: []PlannedCommit{{SHA: "a"}, {SHA: "b"}}})

	// Assert.
	if err == nil {
		t.Fatal("ResumePlan() error = nil, want a remainder larger than the total refused")
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
