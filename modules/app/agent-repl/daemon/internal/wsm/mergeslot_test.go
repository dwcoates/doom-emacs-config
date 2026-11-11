package wsm

import (
	"context"
	"strings"
	"testing"
	"time"
)

func TestMergeSlotsOf(t *testing.T) {
	type row struct {
		state MergeQueueState
		aside MergeAside
	}
	queued, admitted, requested := row{state: MergeQueued}, row{state: MergeAdmitted}, row{state: MergeRequested}
	tests := row{state: MergeRemediating, aside: MergeAsideTests}
	conflicts := row{state: MergeRemediating, aside: MergeAsideConflicts}
	waitingConflicts := row{state: MergeRemediating, aside: MergeAsideWaitingConflicts}
	cases := []struct {
		name string
		rows []row
		want []MergeSlot
	}{
		{name: "an empty queue holds no slot", rows: nil, want: []MergeSlot{}},
		{name: "the first merge in line heads it while it waits", rows: []row{queued, queued}, want: []MergeSlot{MergeSlotHead, MergeSlotWaiting}},
		{name: "an admitted front heads its line", rows: []row{admitted}, want: []MergeSlot{MergeSlotHead}},
		{name: "an admitted merge behind the head waits", rows: []row{admitted, admitted, queued}, want: []MergeSlot{MergeSlotHead, MergeSlotWaiting, MergeSlotWaiting}},
		{name: "a requested merge holds no slot and never heads", rows: []row{requested, queued}, want: []MergeSlot{MergeSlotNone, MergeSlotHead}},
		{name: "a merge fixing its tests holds the fixing slot and never heads", rows: []row{tests, admitted}, want: []MergeSlot{MergeSlotFixingTests, MergeSlotHead}},
		{name: "a merge resolving conflicts holds the resolving slot", rows: []row{admitted, conflicts}, want: []MergeSlot{MergeSlotHead, MergeSlotResolvingConflicts}},
		{name: "a waiting merge's conflict holds the resolving slot", rows: []row{admitted, waitingConflicts}, want: []MergeSlot{MergeSlotHead, MergeSlotResolvingConflicts}},
		{name: "a second merge fixing tests awaits the slot the first holds", rows: []row{admitted, tests, tests}, want: []MergeSlot{MergeSlotHead, MergeSlotFixingTests, MergeSlotAwaitingFixingTests}},
		{name: "a conflict of either origin awaits the resolving slot another holds", rows: []row{conflicts, waitingConflicts, conflicts}, want: []MergeSlot{MergeSlotResolvingConflicts, MergeSlotAwaitingResolvingConflicts, MergeSlotAwaitingResolvingConflicts}},
		{name: "the two repair slots are held at once", rows: []row{queued, tests, conflicts, queued}, want: []MergeSlot{MergeSlotHead, MergeSlotFixingTests, MergeSlotResolvingConflicts, MergeSlotWaiting}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			rows := make([]slotRow, len(tc.rows))
			for i, r := range tc.rows {
				rows[i] = slotRow{state: r.state, aside: r.aside}
			}

			// Act
			got, err := mergeSlotsOf(rows)

			// Assert
			if err != nil {
				t.Fatalf("mergeSlotsOf: %v", err)
			}
			if len(got) != len(tc.want) {
				t.Fatalf("mergeSlotsOf = %v, want %v", got, tc.want)
			}
			for i := range got {
				if got[i] != tc.want[i] {
					t.Fatalf("mergeSlotsOf = %v, want %v", got, tc.want)
				}
			}
		})
	}
}

func TestMergeSlotsOfRefusesARemediatingEntryWithNoRepair(t *testing.T) {
	// Arrange
	rows := []slotRow{{workspace: "w1", state: MergeRemediating, aside: MergeAsideNone}}

	// Act
	_, err := mergeSlotsOf(rows)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "names no repair slot") || !strings.Contains(err.Error(), "w1") {
		t.Fatalf("mergeSlotsOf = %v, want the remediating entry with no repair refused by name", err)
	}
}

// slotClock is a store clock a test moves by hand.
type slotClock struct{ at time.Time }

func (c *slotClock) now() time.Time { return c.at }

// slotQueue is a queue of three merges, a, b and c, in that order, with the
// store's clock under the test's hand: every step happens one minute after
// the last, so each slot moment names the step that wrote it.
type slotQueue struct {
	s       *store
	clock   *slotClock
	repo    RepoKey
	a, b, c WorkspaceID
}

func newSlotQueue(t *testing.T, s *store) *slotQueue {
	t.Helper()
	clock := &slotClock{at: instant}
	s.now = clock.now
	q := &slotQueue{s: s, clock: clock, repo: RepoKey(t.TempDir())}
	q.a, q.b, q.c = testWorkspace(t, s).ID, testWorkspace(t, s).ID, testWorkspace(t, s).ID
	for _, ws := range []WorkspaceID{q.a, q.b, q.c} {
		q.step(t, "queue", func(ctx context.Context) error {
			if err := s.RequestMerge(ctx, q.repo, ws, ownBranch, q.clock.at); err != nil {
				return err
			}
			_, err := s.QueueMerge(ctx, q.repo, ws, NewLeaseID())
			return err
		})
	}
	return q
}

// step moves the clock on a minute and runs one write, answering the moment
// it ran at.
func (q *slotQueue) step(t *testing.T, name string, write func(context.Context) error) time.Time {
	t.Helper()
	q.clock.at = q.clock.at.Add(time.Minute)
	if err := write(context.Background()); err != nil {
		t.Fatalf("%s: %v", name, err)
	}
	return q.clock.at
}

// entry reads WS's entry on the queue.
func (q *slotQueue) entry(t *testing.T, ws WorkspaceID) MergeQueueEntry {
	t.Helper()
	entries, err := q.s.MergeQueue(context.Background(), q.repo)
	if err != nil {
		t.Fatalf("MergeQueue: %v", err)
	}
	for _, e := range entries {
		if e.Workspace == ws {
			return e
		}
	}
	t.Fatalf("workspace %s has no entry on the queue", ws)
	return MergeQueueEntry{}
}

func TestTheSlotMomentIsWrittenWithEverySlotChange(t *testing.T) {
	type check struct {
		ws   func(*slotQueue) WorkspaceID
		slot MergeSlot
		// at answers the moment the entry must carry, from the steps' moments.
		at func(steps map[string]time.Time) time.Time
	}
	a := func(q *slotQueue) WorkspaceID { return q.a }
	b := func(q *slotQueue) WorkspaceID { return q.b }
	c := func(q *slotQueue) WorkspaceID { return q.c }
	type step struct {
		name  string
		write func(*slotQueue, context.Context) error
	}
	admit := func(ws func(*slotQueue) WorkspaceID) func(*slotQueue, context.Context) error {
		return func(q *slotQueue, ctx context.Context) error { return q.s.AdmitMerge(ctx, q.repo, ws(q)) }
	}
	stepAside := func(q *slotQueue, ctx context.Context) error {
		_, err := q.s.StepAsideMerge(ctx, q.repo, q.a, MergeAsideTests)
		return err
	}
	cases := []struct {
		name  string
		steps []step
		want  check
	}{
		{
			name:  "the first merge in line takes the head as it is put in line",
			steps: nil,
			want:  check{ws: a, slot: MergeSlotHead, at: func(m map[string]time.Time) time.Time { return instant.Add(time.Minute) }},
		},
		{
			name:  "admitting the head keeps its head moment",
			steps: []step{{"admit a", admit(a)}},
			want:  check{ws: a, slot: MergeSlotHead, at: func(m map[string]time.Time) time.Time { return instant.Add(time.Minute) }},
		},
		{
			name:  "a merge admitted behind the head keeps waiting",
			steps: []step{{"admit a", admit(a)}, {"admit b", admit(b)}},
			want:  check{ws: b, slot: MergeSlotWaiting, at: func(m map[string]time.Time) time.Time { return instant.Add(2 * time.Minute) }},
		},
		{
			name:  "a head stepping aside takes the fixing slot as it steps aside",
			steps: []step{{"admit a", admit(a)}, {"step aside", stepAside}},
			want:  check{ws: a, slot: MergeSlotFixingTests, at: func(m map[string]time.Time) time.Time { return m["step aside"] }},
		},
		{
			name:  "the merge behind a head stepping aside takes the head as it steps aside",
			steps: []step{{"admit a", admit(a)}, {"step aside", stepAside}},
			want:  check{ws: b, slot: MergeSlotHead, at: func(m map[string]time.Time) time.Time { return m["step aside"] }},
		},
		{
			name: "a remediated merge returning behind the worked-on head waits from its return",
			steps: []step{{"admit a", admit(a)}, {"step aside", stepAside}, {"admit b", admit(b)},
				{"return", func(q *slotQueue, ctx context.Context) error { return q.s.ReturnRemediatedMerge(ctx, q.repo, q.a, nil) }}},
			want: check{ws: a, slot: MergeSlotWaiting, at: func(m map[string]time.Time) time.Time { return m["return"] }},
		},
		{
			name: "a returned merge takes the head again as the head ahead of it lands",
			steps: []step{{"admit a", admit(a)}, {"step aside", stepAside}, {"admit b", admit(b)},
				{"return", func(q *slotQueue, ctx context.Context) error { return q.s.ReturnRemediatedMerge(ctx, q.repo, q.a, nil) }},
				{"b lands", func(q *slotQueue, ctx context.Context) error {
					return q.s.RemoveMergeQueueEntry(ctx, q.repo, q.b, "landed", nil)
				}}},
			want: check{ws: a, slot: MergeSlotHead, at: func(m map[string]time.Time) time.Time { return m["b lands"] }},
		},
		{
			name: "a remediated merge returning into a line with nobody ahead takes the head as it returns",
			steps: []step{{"admit a", admit(a)}, {"step aside", stepAside},
				{"b leaves", func(q *slotQueue, ctx context.Context) error {
					return q.s.RemoveMergeQueueEntry(ctx, q.repo, q.b, "taken off", nil)
				}},
				{"c leaves", func(q *slotQueue, ctx context.Context) error {
					return q.s.RemoveMergeQueueEntry(ctx, q.repo, q.c, "taken off", nil)
				}},
				{"return", func(q *slotQueue, ctx context.Context) error { return q.s.ReturnRemediatedMerge(ctx, q.repo, q.a, nil) }}},
			want: check{ws: a, slot: MergeSlotHead, at: func(m map[string]time.Time) time.Time { return m["return"] }},
		},
		{
			name: "the successor takes the head as the head lands",
			steps: []step{{"admit a", admit(a)},
				{"a lands", func(q *slotQueue, ctx context.Context) error {
					return q.s.RemoveMergeQueueEntry(ctx, q.repo, q.a, "landed", nil)
				}}},
			want: check{ws: b, slot: MergeSlotHead, at: func(m map[string]time.Time) time.Time { return m["a lands"] }},
		},
		{
			name: "an admitted successor takes the head as the head lands",
			steps: []step{{"admit a", admit(a)}, {"admit b", admit(b)},
				{"a lands", func(q *slotQueue, ctx context.Context) error {
					return q.s.RemoveMergeQueueEntry(ctx, q.repo, q.a, "landed", nil)
				}}},
			want: check{ws: b, slot: MergeSlotHead, at: func(m map[string]time.Time) time.Time { return m["a lands"] }},
		},
		{
			name: "a head a restart returns to its place keeps its head moment",
			steps: []step{{"admit a", admit(a)},
				{"restart", func(q *slotQueue, ctx context.Context) error {
					return q.s.ReturnAdmittedMerge(ctx, q.repo, q.a, NewLeaseID())
				}},
				{"readmit a", admit(a)}},
			want: check{ws: a, slot: MergeSlotHead, at: func(m map[string]time.Time) time.Time { return instant.Add(time.Minute) }},
		},
		{
			name: "a waiting merge moved to the front of the waiting keeps waiting",
			steps: []step{{"admit a", admit(a)},
				{"prioritize c", func(q *slotQueue, ctx context.Context) error {
					_, err := q.s.PrioritizeMerge(ctx, q.repo, q.c)
					return err
				}}},
			want: check{ws: c, slot: MergeSlotWaiting, at: func(m map[string]time.Time) time.Time { return instant.Add(3 * time.Minute) }},
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			q := newSlotQueue(t, s)

			// Act
			moments := map[string]time.Time{}
			for _, st := range tc.steps {
				moments[st.name] = q.step(t, st.name, func(ctx context.Context) error { return st.write(q, ctx) })
			}

			// Assert
			got := q.entry(t, tc.want.ws(q))
			if got.Slot != tc.want.slot || !got.SlotSince.Equal(tc.want.at(moments)) {
				t.Fatalf("entry = %s since %v, want %s since %v", got.Slot, got.SlotSince, tc.want.slot, tc.want.at(moments))
			}
		})
	}
}

func TestForgettingTheHeadsWorkspaceHandsTheHeadOnAsItIsForgotten(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	q := newSlotQueue(t, s)

	// Act
	forgot := q.step(t, "forget a", func(ctx context.Context) error {
		_, err := s.Forget(ctx, q.a)
		return err
	})

	// Assert
	got := q.entry(t, q.b)
	if got.Slot != MergeSlotHead || !got.SlotSince.Equal(forgot) {
		t.Fatalf("b = %s since %v, want head since %v", got.Slot, got.SlotSince, forgot)
	}
}

func TestAHandoverReadsTheSlotMomentUnchanged(t *testing.T) {
	// Arrange: a queue on a file, its head admitted and stepped aside, so the
	// merge behind it heads the line.
	s, _ := fileStore(t)
	q := newSlotQueue(t, s)
	q.step(t, "admit a", func(ctx context.Context) error { return s.AdmitMerge(ctx, q.repo, q.a) })
	raced := q.step(t, "step aside", func(ctx context.Context) error {
		_, err := s.StepAsideMerge(ctx, q.repo, q.a, MergeAsideTests)
		return err
	})

	// Act: a new daemon opens the same file.
	reopened, err := Open(context.Background(), s.path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	defer reopened.Close()
	entries, err := reopened.MergeQueue(context.Background(), q.repo)
	if err != nil {
		t.Fatalf("MergeQueue: %v", err)
	}

	// Assert
	if len(entries) != 3 || entries[0].Workspace != q.b || !entries[0].SlotSince.Equal(raced) || entries[2].Workspace != q.a || !entries[2].SlotSince.Equal(raced) {
		t.Fatalf("entries = %+v, want the new head and the merge fixing its tests both since %v", entries, raced)
	}
}

func TestTheThreeSlotMigrationRestampsEveryQueue(t *testing.T) {
	// Arrange -- a layout-122 file as the racing build left it: a head, a
	// merge racing behind it (slot 3), one waiting, and three remediating
	// merges, two fixing tests and one resolving conflicts, each with the
	// progress record that names its repair; the racing switch disabled.
	path := fixtureAt(t, 122)
	execRaw(t, path, `INSERT INTO merge_queue (repo_key, workspace_id, seq, state, enqueued_at, source_kind, slot) VALUES
	 ('/repo', 'head', 1, 1, 1, 0, 2), ('/repo', 'racer', 2, 1, 1, 0, 3), ('/repo', 'waits', 3, 0, 1, 0, 1),
	 ('/repo', 'fix-1', 4, 3, 1, 0, 4), ('/repo', 'fix-2', 5, 3, 1, 0, 4), ('/repo', 'resolve', 6, 3, 1, 0, 4)`)
	execRaw(t, path, `INSERT INTO merge_progress (workspace_id, lease_id, updated_at, document) VALUES
	 ('fix-1', 'l1', 1, '{"step":"fixes"}'), ('fix-2', 'l2', 1, '{"step":"fixes"}'), ('resolve', 'l3', 1, '{"step":"conflicts"}')`)
	execRaw(t, path, `INSERT INTO merge_racing_disabled (id, disabled_at) VALUES (1, 5)`)

	// Act
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open on a layout-122 database: %v", err)
	}
	defer handle.Close()
	s := handle.(*store)

	// Assert
	want := []struct {
		ws    string
		slot  MergeSlot
		aside MergeAside
	}{
		{"head", MergeSlotHead, MergeAsideNone},
		{"racer", MergeSlotWaiting, MergeAsideNone},
		{"waits", MergeSlotWaiting, MergeAsideNone},
		{"fix-1", MergeSlotFixingTests, MergeAsideTests},
		{"fix-2", MergeSlotAwaitingFixingTests, MergeAsideTests},
		{"resolve", MergeSlotResolvingConflicts, MergeAsideConflicts},
	}
	for _, w := range want {
		slot := scalar[int](t, s, `SELECT slot FROM merge_queue WHERE workspace_id = ?`, w.ws)
		aside := scalar[int](t, s, `SELECT aside FROM merge_queue WHERE workspace_id = ?`, w.ws)
		if MergeSlot(slot) != w.slot || MergeAside(aside) != w.aside {
			t.Errorf("%s = slot %s aside %s, want slot %s aside %s", w.ws, MergeSlot(slot), MergeAside(aside), w.slot, w.aside)
		}
	}
	if n := scalar[int](t, s, `SELECT count(*) FROM sqlite_master WHERE name = 'merge_racing_disabled'`); n != 0 {
		t.Fatalf("merge_racing_disabled survived the migration (%d), want it dropped", n)
	}
}

func TestTheStoreRefusesASecondHolderOfAnActiveSlot(t *testing.T) {
	cases := []struct {
		name string
		slot MergeSlot
	}{
		{name: "a second head", slot: MergeSlotHead},
		{name: "a second merge fixing tests", slot: MergeSlotFixingTests},
		{name: "a second merge resolving conflicts", slot: MergeSlotResolvingConflicts},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			q := newSlotQueue(t, s)
			corrupt(t, s, `UPDATE merge_queue SET slot = ? WHERE workspace_id = ?`, int(tc.slot), q.a)

			// Act
			_, err := s.db().ExecContext(context.Background(), `UPDATE merge_queue SET slot = ? WHERE workspace_id = ?`, int(tc.slot), q.b)

			// Assert
			if err == nil || !strings.Contains(err.Error(), "UNIQUE") {
				t.Fatalf("recording a second %s holder = %v, want the one-holder index's refusal", tc.slot, err)
			}
		})
	}
}

func TestAnUndeclaredSlotRefusesTheQueuesRead(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	q := newSlotQueue(t, s)
	corrupt(t, s, `UPDATE merge_queue SET slot = 9 WHERE workspace_id = ?`, q.a)

	// Act
	_, err := s.MergeQueue(context.Background(), q.repo)

	// Assert
	if err == nil {
		t.Fatal("MergeQueue over an undeclared slot = nil, want a decode refusal")
	}
}

func TestSlotsOfDerivesWhatTheStoreStamps(t *testing.T) {
	// Arrange: a queue the store stamped, its head stepped aside for tests
	// and a second for conflicts, read back.
	q, _ := newAsideQueue(t, 3)
	q.asideFromHead(t, q.ws[0], MergeAsideTests)
	q.asideFromHead(t, q.ws[1], MergeAsideConflicts)
	entries, err := q.s.MergeQueue(context.Background(), q.repo)
	if err != nil {
		t.Fatalf("MergeQueue: %v", err)
	}

	// Act
	got, err := SlotsOf(entries)

	// Assert
	if err != nil {
		t.Fatalf("SlotsOf: %v", err)
	}
	for i, entry := range entries {
		if got[i] != entry.Slot {
			t.Fatalf("SlotsOf = %v, want the stamped slot %s for %s", got, entry.Slot, entry.Workspace)
		}
	}
}
