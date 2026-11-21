package wsm

import (
	"context"
	"fmt"
	"strings"
	"testing"
	"time"

	"agent-repld/internal/dlog"
)

func TestStampClock(t *testing.T) {
	t0 := time.UnixMilli(1_700_000_000_000).UTC()
	at := t0.Add(40 * time.Minute)
	waiting := MergeClock{Phase: MergeClockWaiting, Since: t0, Waited: 5 * time.Minute}
	active := MergeClock{Phase: MergeClockActive, Since: t0, Waited: 5 * time.Minute}
	held := MergeClock{Phase: MergeClockActive, Since: t0, Waited: 5 * time.Minute, Held: true}
	cases := []struct {
		name      string
		prev      MergeClock
		state     MergeQueueState
		slot      MergeSlot
		want      MergeClock
		wantMoved bool
	}{
		{name: "a merge put in line starts waiting with nothing waited", prev: MergeClock{}, state: MergeQueued, slot: MergeSlotWaiting,
			want: MergeClock{Phase: MergeClockWaiting, Since: at}, wantMoved: true},
		{name: "a merge put in line at the head starts waiting", prev: MergeClock{}, state: MergeQueued, slot: MergeSlotHead,
			want: MergeClock{Phase: MergeClockWaiting, Since: at}, wantMoved: true},
		{name: "a waiting merge taking the head keeps waiting on", prev: waiting, state: MergeQueued, slot: MergeSlotHead,
			want: waiting},
		{name: "a head admitted becomes active from 0 and banks its wait", prev: waiting, state: MergeAdmitted, slot: MergeSlotHead,
			want: MergeClock{Phase: MergeClockActive, Since: at, Waited: 45 * time.Minute}, wantMoved: true},
		{name: "an active head stepping aside to fix its tests stays on its stretch", prev: active, state: MergeRemediating, slot: MergeSlotFixingTests,
			want: active},
		{name: "an active head stepping aside to resolve conflicts stays on its stretch", prev: active, state: MergeRemediating, slot: MergeSlotResolvingConflicts,
			want: active},
		{name: "an active merge awaiting the fixing-tests slot waits again with its wait given back", prev: active, state: MergeRemediating, slot: MergeSlotAwaitingFixingTests,
			want: MergeClock{Phase: MergeClockWaiting, Since: at, Waited: 5 * time.Minute}, wantMoved: true},
		{name: "an active merge back in line waits again with its wait given back", prev: active, state: MergeAdmitted, slot: MergeSlotWaiting,
			want: MergeClock{Phase: MergeClockWaiting, Since: at, Waited: 5 * time.Minute}, wantMoved: true},
		{name: "a waiting merge whose background rebase conflicted becomes active", prev: waiting, state: MergeRemediating, slot: MergeSlotResolvingConflicts,
			want: MergeClock{Phase: MergeClockActive, Since: at, Waited: 45 * time.Minute}, wantMoved: true},
		{name: "a head a restart returned stays active while held", prev: held, state: MergeQueued, slot: MergeSlotHead,
			want: held},
		{name: "a held head admitted again continues its stretch and drops the hold", prev: held, state: MergeAdmitted, slot: MergeSlotHead,
			want: active, wantMoved: true},
		{name: "a held head sent behind another merge waits and drops the hold", prev: held, state: MergeQueued, slot: MergeSlotWaiting,
			want: MergeClock{Phase: MergeClockWaiting, Since: at, Waited: 5 * time.Minute}, wantMoved: true},
		{name: "an unheld head returned to waiting waits", prev: active, state: MergeQueued, slot: MergeSlotHead,
			want: MergeClock{Phase: MergeClockWaiting, Since: at, Waited: 5 * time.Minute}, wantMoved: true},
		{name: "a merge leaving every line forgets its clock", prev: waiting, state: MergeRequested, slot: MergeSlotNone,
			want: MergeClock{Phase: MergeClockNone}, wantMoved: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, moved, err := StampClock(tc.prev, tc.state, tc.slot, at)

			// Assert
			if err != nil || got != tc.want || moved != tc.wantMoved {
				t.Fatalf("StampClock = %+v, moved %v, %v; want %+v, moved %v", got, moved, err, tc.want, tc.wantMoved)
			}
		})
	}
}

func TestStampClockRefusesAnUndeclaredSlot(t *testing.T) {
	// Act
	_, _, err := StampClock(MergeClock{}, MergeQueued, MergeSlot(9), time.UnixMilli(1))

	// Assert
	if err == nil {
		t.Fatal("StampClock over an undeclared slot = nil, want a refusal")
	}
}

// clockStep is one write of a clock scenario on a slotQueue.
type clockStep struct {
	name  string
	write func(*testing.T, *slotQueue, context.Context) error
}

func admitStep(ws func(*slotQueue) WorkspaceID) func(*testing.T, *slotQueue, context.Context) error {
	return func(_ *testing.T, q *slotQueue, ctx context.Context) error { return q.s.AdmitMerge(ctx, q.repo, ws(q)) }
}

func asideStep(ws func(*slotQueue) WorkspaceID, aside MergeAside) func(*testing.T, *slotQueue, context.Context) error {
	return func(_ *testing.T, q *slotQueue, ctx context.Context) error {
		if aside != MergeAsideWaitingConflicts {
			if _, err := holdAsideLeaseErr(ctx, q.s, ws(q), aside); err != nil {
				return err
			}
		}
		_, err := q.s.StepAsideMerge(ctx, q.repo, ws(q), aside)
		return err
	}
}

func returnStep(ws func(*slotQueue) WorkspaceID) func(*testing.T, *slotQueue, context.Context) error {
	return func(_ *testing.T, q *slotQueue, ctx context.Context) error {
		_, err := returnRemediated(ctx, q.s, q.repo, ws(q), nil)
		return err
	}
}

func landStep(ws func(*slotQueue) WorkspaceID) func(*testing.T, *slotQueue, context.Context) error {
	return func(_ *testing.T, q *slotQueue, ctx context.Context) error {
		return q.s.RemoveMergeQueueEntry(ctx, q.repo, ws(q), "landed")
	}
}

func restartStep(ws func(*slotQueue) WorkspaceID) func(*testing.T, *slotQueue, context.Context) error {
	return func(_ *testing.T, q *slotQueue, ctx context.Context) error {
		return q.s.ReturnAdmittedMerge(ctx, q.repo, ws(q), NewLeaseID())
	}
}

func handoverStep(ws func(*slotQueue) WorkspaceID) func(*testing.T, *slotQueue, context.Context) error {
	return func(t *testing.T, q *slotQueue, ctx context.Context) error {
		lease, err := q.s.AcquireLease(ctx, ws(q), HolderMerge, PolicyHold)
		if err != nil {
			return err
		}
		putProgress(t, q.s, ws(q), lease.ID)
		return q.s.RequeuePausedMerge(ctx, q.repo, ws(q), lease.ID, NewLeaseID())
	}
}

func TestTheStoreStampsEachMergesClockWithEveryChange(t *testing.T) {
	a := func(q *slotQueue) WorkspaceID { return q.a }
	b := func(q *slotQueue) WorkspaceID { return q.b }
	// put is when each merge was put in line (newSlotQueue): a, b, c a
	// minute apart.
	put := func(n int) time.Time { return instant.Add(time.Duration(n) * time.Minute) }
	cases := []struct {
		name  string
		steps []clockStep
		ws    func(*slotQueue) WorkspaceID
		want  func(m map[string]time.Time) MergeClock
	}{
		{name: "a head not yet admitted waits from when it was put in line", ws: a,
			want: func(map[string]time.Time) MergeClock { return MergeClock{Phase: MergeClockWaiting, Since: put(1)} }},
		{name: "waiting then active, the clock starts from 0 as the head is admitted", ws: a,
			steps: []clockStep{{"admit a", admitStep(a)}},
			want: func(m map[string]time.Time) MergeClock {
				return MergeClock{Phase: MergeClockActive, Since: m["admit a"], Waited: m["admit a"].Sub(put(1))}
			}},
		{name: "a head stepping aside to fix its tests stays on its active stretch", ws: a,
			steps: []clockStep{{"admit a", admitStep(a)}, {"a aside", asideStep(a, MergeAsideTests)}},
			want: func(m map[string]time.Time) MergeClock {
				return MergeClock{Phase: MergeClockActive, Since: m["admit a"], Waited: m["admit a"].Sub(put(1))}
			}},
		{name: "active then waiting, the merge back in line takes back its waiting", ws: a,
			steps: []clockStep{{"admit a", admitStep(a)}, {"a aside", asideStep(a, MergeAsideTests)}, {"admit b", admitStep(b)}, {"a returns", returnStep(a)}},
			want: func(m map[string]time.Time) MergeClock {
				return MergeClock{Phase: MergeClockWaiting, Since: m["a returns"], Waited: m["admit a"].Sub(put(1))}
			}},
		{name: "active then awaiting the fixing-tests slot, the merge takes back its waiting", ws: b,
			steps: []clockStep{{"admit a", admitStep(a)}, {"a aside", asideStep(a, MergeAsideTests)}, {"admit b", admitStep(b)}, {"b aside", asideStep(b, MergeAsideTests)}},
			want: func(m map[string]time.Time) MergeClock {
				return MergeClock{Phase: MergeClockWaiting, Since: m["b aside"], Waited: m["admit b"].Sub(put(2))}
			}},
		// A merge back from its repair waits in line like any merge (owner
		// ruling, 2026-11-20), so it is active again only once it is admitted
		// at the head.
		{name: "several cycles sum every waiting stretch and no active one", ws: a,
			steps: []clockStep{{"admit a", admitStep(a)}, {"a aside", asideStep(a, MergeAsideTests)}, {"admit b", admitStep(b)}, {"a returns", returnStep(a)},
				{"b lands", landStep(b)}, {"readmit a", admitStep(a)}},
			want: func(m map[string]time.Time) MergeClock {
				waited := m["admit a"].Sub(put(1)) + m["readmit a"].Sub(m["a returns"])
				return MergeClock{Phase: MergeClockActive, Since: m["readmit a"], Waited: waited}
			}},
		{name: "a third waiting stretch after two active ones is summed too", ws: a,
			steps: []clockStep{{"admit a", admitStep(a)}, {"a aside", asideStep(a, MergeAsideTests)}, {"admit b", admitStep(b)}, {"a returns", returnStep(a)},
				{"b lands", landStep(b)}, {"readmit a", admitStep(a)}, {"a aside again", asideStep(a, MergeAsideTests)}, {"admit c", admitStep(func(q *slotQueue) WorkspaceID { return q.c })}, {"a returns again", returnStep(a)}},
			want: func(m map[string]time.Time) MergeClock {
				waited := m["admit a"].Sub(put(1)) + m["readmit a"].Sub(m["a returns"])
				return MergeClock{Phase: MergeClockWaiting, Since: m["a returns again"], Waited: waited}
			}},
		{name: "a restart returning the admitted head holds its active stretch", ws: a,
			steps: []clockStep{{"admit a", admitStep(a)}, {"restart", restartStep(a)}},
			want: func(m map[string]time.Time) MergeClock {
				return MergeClock{Phase: MergeClockActive, Since: m["admit a"], Waited: m["admit a"].Sub(put(1)), Held: true}
			}},
		{name: "the head admitted again after a restart continues its active stretch", ws: a,
			steps: []clockStep{{"admit a", admitStep(a)}, {"restart", restartStep(a)}, {"readmit a", admitStep(a)}},
			want: func(m map[string]time.Time) MergeClock {
				return MergeClock{Phase: MergeClockActive, Since: m["admit a"], Waited: m["admit a"].Sub(put(1))}
			}},
		{name: "a handover requeueing the paused head holds its active stretch", ws: a,
			steps: []clockStep{{"admit a", admitStep(a)}, {"handover", handoverStep(a)}},
			want: func(m map[string]time.Time) MergeClock {
				return MergeClock{Phase: MergeClockActive, Since: m["admit a"], Waited: m["admit a"].Sub(put(1)), Held: true}
			}},
		{name: "a merge waiting behind a head that changes keeps waiting on", ws: b,
			steps: []clockStep{{"admit a", admitStep(a)}, {"a lands", landStep(a)}},
			want:  func(map[string]time.Time) MergeClock { return MergeClock{Phase: MergeClockWaiting, Since: put(2)} }},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			q := newSlotQueue(t, s)

			// Act
			moments := runClockSteps(t, q, tc.steps)

			// Assert
			if got, want := q.entry(t, tc.ws(q)).Clock, tc.want(moments); got != want {
				t.Fatalf("clock = %+v, want %+v", got, want)
			}
		})
	}
}

// runClockSteps runs STEPS on Q a minute apart, answering the moment each ran
// at by its name.
func runClockSteps(t *testing.T, q *slotQueue, steps []clockStep) map[string]time.Time {
	t.Helper()
	moments := map[string]time.Time{}
	for _, st := range steps {
		moments[st.name] = q.step(t, st.name, func(ctx context.Context) error { return st.write(t, q, ctx) })
	}
	return moments
}

func TestADaemonRestartInTheMiddleOfAStretchNeitherResetsNorInflatesTheClock(t *testing.T) {
	a := func(q *slotQueue) WorkspaceID { return q.a }
	b := func(q *slotQueue) WorkspaceID { return q.b }
	put := instant.Add(time.Minute)
	cases := []struct {
		name   string
		before []clockStep
		after  []clockStep
		want   func(m map[string]time.Time) MergeClock
	}{
		{name: "a restart while the merge waits", before: nil,
			after: []clockStep{{"admit a", admitStep(a)}},
			want: func(m map[string]time.Time) MergeClock {
				return MergeClock{Phase: MergeClockActive, Since: m["admit a"], Waited: m["admit a"].Sub(put)}
			}},
		{name: "a restart while the merge is active", before: []clockStep{{"admit a", admitStep(a)}},
			after: []clockStep{{"restart", restartStep(a)}, {"readmit a", admitStep(a)}},
			want: func(m map[string]time.Time) MergeClock {
				return MergeClock{Phase: MergeClockActive, Since: m["admit a"], Waited: m["admit a"].Sub(put)}
			}},
		{name: "a restart while the merge waits again after an active stretch",
			before: []clockStep{{"admit a", admitStep(a)}, {"a aside", asideStep(a, MergeAsideTests)}, {"admit b", admitStep(b)}, {"a returns", returnStep(a)}},
			after:  []clockStep{{"b lands", landStep(b)}, {"readmit a", admitStep(a)}},
			want: func(m map[string]time.Time) MergeClock {
				return MergeClock{Phase: MergeClockActive, Since: m["readmit a"], Waited: m["admit a"].Sub(put) + m["readmit a"].Sub(m["a returns"])}
			}},
		{name: "a restart while the merge fixes its tests out of line",
			before: []clockStep{{"admit a", admitStep(a)}, {"a aside", asideStep(a, MergeAsideTests)}, {"admit b", admitStep(b)}},
			after:  []clockStep{{"a returns", returnStep(a)}},
			want: func(m map[string]time.Time) MergeClock {
				return MergeClock{Phase: MergeClockWaiting, Since: m["a returns"], Waited: m["admit a"].Sub(put)}
			}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the stretch begins on one daemon.
			s, _ := fileStore(t)
			q := newSlotQueue(t, s)
			moments := runClockSteps(t, q, tc.before)
			stood := q.entry(t, q.a).Clock

			// Act: a new daemon opens the same file and carries the queue on.
			handle, err := Open(context.Background(), s.path, WithUnsyncedWrites())
			if err != nil {
				t.Fatalf("Open: %v", err)
			}
			defer handle.Close()
			reopened := handle.(*store)
			reopened.now = q.clock.now
			read := (&slotQueue{s: reopened, clock: q.clock, repo: q.repo, a: q.a, b: q.b, c: q.c}).entry(t, q.a).Clock
			q.s = reopened
			for name, at := range runClockSteps(t, q, tc.after) {
				moments[name] = at
			}

			// Assert
			if read != stood {
				t.Fatalf("clock read by the new daemon = %+v, want the one the old daemon left, %+v", read, stood)
			}
			if got, want := q.entry(t, q.a).Clock, tc.want(moments); got != want {
				t.Fatalf("clock after the restart = %+v, want %+v", got, want)
			}
		})
	}
}

func TestAnUndeclaredClockPhaseRefusesTheQueuesRead(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	q := newSlotQueue(t, s)
	corrupt(t, s, `UPDATE merge_queue SET clock_phase = 9 WHERE workspace_id = ?`, q.a)

	// Act
	_, err := s.MergeQueue(context.Background(), q.repo)

	// Assert
	if err == nil {
		t.Fatal("MergeQueue over an undeclared clock phase = nil, want a decode refusal")
	}
}

func TestAQueueWriteOverAnUndeclaredSlotFailsWithoutMovingAnyClock(t *testing.T) {
	// Arrange: b's row carries an aside naming no repair, so the slot
	// derivation the write's restamp runs refuses it.
	s, logs := testStore(t)
	q := newSlotQueue(t, s)
	corrupt(t, s, `UPDATE merge_queue SET state = ?, aside = 0 WHERE workspace_id = ?`, int(MergeRemediating), q.b)
	before := q.clockColumns(t)

	// Act
	err := q.s.AdmitMerge(context.Background(), q.repo, q.a)

	// Assert
	if err == nil {
		t.Fatal("AdmitMerge over a queue its derivation refuses = nil, want the refusal")
	}
	if after := q.clockColumns(t); after != before {
		t.Fatalf("clock columns = %v after the refused write, want unchanged %v", after, before)
	}
	rec, ok := dlog.FindRecord(logs.Records(), dlog.RecordQuery{Level: "error", Operation: "daemon.wsm.admit_merge", Message: "refused the write"})
	if !ok || rec.Context["workspace"] != string(q.a) || !strings.Contains(fmt.Sprint(rec.Context["error"]), "names no repair slot") {
		t.Fatalf("ERROR record = %+v (found %v), want the refused admission naming a and the slot refusal", rec, ok)
	}
}

// clockColumns reads a's stored clock columns raw, bypassing the decode.
func (q *slotQueue) clockColumns(t *testing.T) [3]int64 {
	t.Helper()
	return [3]int64{
		scalar[int64](t, q.s, `SELECT clock_phase FROM merge_queue WHERE workspace_id = ?`, q.a),
		scalar[int64](t, q.s, `SELECT clock_since FROM merge_queue WHERE workspace_id = ?`, q.a),
		scalar[int64](t, q.s, `SELECT clock_waited FROM merge_queue WHERE workspace_id = ?`, q.a),
	}
}

func TestTheClockMigrationGivesEveryQueuedMergeTheClockItsStandingGives(t *testing.T) {
	// Arrange: a layout-179 file holding a merge in each standing.
	path := fixtureAt(t, 179)
	execRaw(t, path, `INSERT INTO merge_queue (repo_key, workspace_id, seq, state, enqueued_at, source_kind, slot, slot_since, aside) VALUES
	 ('/repo', 'head', 1, 1, 10, 0, 2, 100, 0), ('/repo', 'waits', 2, 0, 20, 0, 1, 200, 0),
	 ('/repo', 'fixes', 3, 3, 30, 0, 4, 300, 1), ('/repo', 'awaits', 4, 3, 40, 0, 6, 400, 1),
	 ('/repo', 'resolves', 5, 3, 50, 0, 5, 500, 2), ('/repo', 'asked', 6, 2, 60, 0, 0, 600, 0),
	 ('/other', 'unadmitted', 1, 0, 70, 0, 2, 700, 0)`)

	// Act
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open on a layout-179 database: %v", err)
	}
	defer handle.Close()
	s := handle.(*store)

	// Assert
	want := []struct {
		ws    string
		phase MergeClockPhase
		since int64
	}{
		{"head", MergeClockActive, 100},
		{"waits", MergeClockWaiting, 20},
		{"fixes", MergeClockActive, 300},
		{"awaits", MergeClockWaiting, 40},
		{"resolves", MergeClockActive, 500},
		{"asked", MergeClockNone, 0},
		{"unadmitted", MergeClockWaiting, 70},
	}
	for _, w := range want {
		phase := scalar[int](t, s, `SELECT clock_phase FROM merge_queue WHERE workspace_id = ?`, w.ws)
		since := scalar[int64](t, s, `SELECT clock_since FROM merge_queue WHERE workspace_id = ?`, w.ws)
		waited := scalar[int64](t, s, `SELECT clock_waited FROM merge_queue WHERE workspace_id = ?`, w.ws)
		if MergeClockPhase(phase) != w.phase || since != w.since || waited != 0 {
			t.Errorf("%s = %s since %d waited %d, want %s since %d waited 0", w.ws, MergeClockPhase(phase), since, waited, w.phase, w.since)
		}
	}
}
