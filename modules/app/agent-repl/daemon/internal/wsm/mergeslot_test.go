package wsm

import (
	"context"
	"slices"
	"testing"
	"time"
)

func TestMergeSlotsOf(t *testing.T) {
	cases := []struct {
		name   string
		states []MergeQueueState
		// returned names the rows marked returned (MergeQueueEntry.Returned).
		returned []int
		want     []MergeSlot
	}{
		{name: "an empty queue holds no slot", states: nil, want: []MergeSlot{}},
		{name: "the first merge in line heads it while it waits", states: []MergeQueueState{MergeQueued, MergeQueued}, want: []MergeSlot{MergeSlotHead, MergeSlotWaiting}},
		{name: "an admitted front heads its line", states: []MergeQueueState{MergeAdmitted}, want: []MergeSlot{MergeSlotHead}},
		{name: "an admitted merge behind the head races", states: []MergeQueueState{MergeAdmitted, MergeAdmitted, MergeQueued}, want: []MergeSlot{MergeSlotHead, MergeSlotRacing, MergeSlotWaiting}},
		{name: "a requested merge holds no slot and never heads", states: []MergeQueueState{MergeRequested, MergeQueued}, want: []MergeSlot{MergeSlotNone, MergeSlotHead}},
		{name: "an admitted merge back from a repair waits behind the head", states: []MergeQueueState{MergeAdmitted, MergeAdmitted, MergeQueued}, returned: []int{1}, want: []MergeSlot{MergeSlotHead, MergeSlotWaiting, MergeSlotWaiting}},
		{name: "an admitted merge back from a repair heads a line it stands first in", states: []MergeQueueState{MergeAdmitted, MergeQueued}, returned: []int{0}, want: []MergeSlot{MergeSlotHead, MergeSlotWaiting}},
		{name: "a remediating merge remediates and never heads", states: []MergeQueueState{MergeRemediating, MergeAdmitted}, want: []MergeSlot{MergeSlotRemediating, MergeSlotHead}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			rows := make([]slotRow, len(tc.states))
			for i, state := range tc.states {
				rows[i] = slotRow{state: state, returned: slices.Contains(tc.returned, i)}
			}

			// Act
			got := mergeSlotsOf(rows)

			// Assert
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
	stepAside := func(q *slotQueue, ctx context.Context) error { return q.s.StepAsideMerge(ctx, q.repo, q.a) }
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
			name:  "a merge admitted behind the head takes a racing slot as it is admitted",
			steps: []step{{"admit a", admit(a)}, {"admit b", admit(b)}},
			want:  check{ws: b, slot: MergeSlotRacing, at: func(m map[string]time.Time) time.Time { return m["admit b"] }},
		},
		{
			name:  "a head stepping aside takes the remediating slot as it steps aside",
			steps: []step{{"admit a", admit(a)}, {"step aside", stepAside}},
			want:  check{ws: a, slot: MergeSlotRemediating, at: func(m map[string]time.Time) time.Time { return m["step aside"] }},
		},
		{
			name:  "the merge behind a head stepping aside takes the head as it steps aside",
			steps: []step{{"admit a", admit(a)}, {"step aside", stepAside}},
			want:  check{ws: b, slot: MergeSlotHead, at: func(m map[string]time.Time) time.Time { return m["step aside"] }},
		},
		{
			// A RETURNED MERGE IS NOT WORKED ON until it is taken up: it waits
			// behind the head, admitted, with nothing of it running.
			name: "a remediated merge returning behind the worked-on head waits from its return",
			steps: []step{{"admit a", admit(a)}, {"step aside", stepAside}, {"admit b", admit(b)},
				{"return", func(q *slotQueue, ctx context.Context) error { return q.s.ReturnRemediatedMerge(ctx, q.repo, q.a, nil) }}},
			want: check{ws: a, slot: MergeSlotWaiting, at: func(m map[string]time.Time) time.Time { return m["return"] }},
		},
		{
			name: "a returned merge taken up behind the head races from its take-up",
			steps: []step{{"admit a", admit(a)}, {"step aside", stepAside}, {"admit b", admit(b)},
				{"return", func(q *slotQueue, ctx context.Context) error { return q.s.ReturnRemediatedMerge(ctx, q.repo, q.a, nil) }},
				{"take up", func(q *slotQueue, ctx context.Context) error { return q.s.TakeUpReturnedMerge(ctx, q.repo, q.a) }}},
			want: check{ws: a, slot: MergeSlotRacing, at: func(m map[string]time.Time) time.Time { return m["take up"] }},
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
			name: "a racing successor takes the head as the head lands",
			steps: []step{{"admit a", admit(a)}, {"admit b", admit(b)},
				{"a lands", func(q *slotQueue, ctx context.Context) error {
					return q.s.RemoveMergeQueueEntry(ctx, q.repo, q.a, "landed", nil)
				}}},
			want: check{ws: b, slot: MergeSlotHead, at: func(m map[string]time.Time) time.Time { return m["a lands"] }},
		},
		{
			name: "a race's winner takes the head as it overtakes",
			steps: []step{{"admit a", admit(a)}, {"admit b", admit(b)},
				{"yield", func(q *slotQueue, ctx context.Context) error { return q.s.YieldMergeHead(ctx, q.repo, q.a, q.b) }}},
			want: check{ws: b, slot: MergeSlotHead, at: func(m map[string]time.Time) time.Time { return m["yield"] }},
		},
		{
			name: "a race's loser races from the moment it is overtaken",
			steps: []step{{"admit a", admit(a)}, {"admit b", admit(b)},
				{"yield", func(q *slotQueue, ctx context.Context) error { return q.s.YieldMergeHead(ctx, q.repo, q.a, q.b) }}},
			want: check{ws: a, slot: MergeSlotRacing, at: func(m map[string]time.Time) time.Time { return m["yield"] }},
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
	// Arrange: a queue on a file, its head admitted and a merge racing.
	s, _ := fileStore(t)
	q := newSlotQueue(t, s)
	q.step(t, "admit a", func(ctx context.Context) error { return s.AdmitMerge(ctx, q.repo, q.a) })
	raced := q.step(t, "admit b", func(ctx context.Context) error { return s.AdmitMerge(ctx, q.repo, q.b) })

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
	if len(entries) != 3 || !entries[0].SlotSince.Equal(instant.Add(time.Minute)) || !entries[1].SlotSince.Equal(raced) {
		t.Fatalf("entries = %+v, want the head since %v and the racing merge since %v", entries, instant.Add(time.Minute), raced)
	}
}

func TestTheSlotBackfillGivesEveryRowItsSlotAtTheMomentItRuns(t *testing.T) {
	// Arrange: a queue whose rows carry no slot, as an earlier build left them.
	s, _ := testStore(t)
	q := newSlotQueue(t, s)
	q.step(t, "admit a", func(ctx context.Context) error { return s.AdmitMerge(ctx, q.repo, q.a) })
	q.step(t, "admit b", func(ctx context.Context) error { return s.AdmitMerge(ctx, q.repo, q.b) })
	corrupt(t, s, `UPDATE merge_queue SET slot = 0, slot_since = 0`)
	before := time.Now().Add(-time.Second)

	// Act
	corrupt(t, s, mergeQueueSlotBackfillDDL)

	// Assert
	after := time.Now().Add(time.Second)
	want := map[WorkspaceID]MergeSlot{q.a: MergeSlotHead, q.b: MergeSlotRacing, q.c: MergeSlotWaiting}
	for ws, slot := range want {
		got := q.entry(t, ws)
		if got.Slot != slot || got.SlotSince.Before(before) || got.SlotSince.After(after) {
			t.Fatalf("%s = %s since %v, want %s since the backfill's moment (%v..%v)", ws, got.Slot, got.SlotSince, slot, before, after)
		}
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
