package merge

import (
	"agent-repld/internal/wsm"
	"context"
	"encoding/json"
	"reflect"
	"testing"
	"unsafe"
)

// recordedDoc reads the harness workspace's progress record.
func recordedDoc(t *testing.T, h *harness) progressDoc {
	t.Helper()
	stored, found, err := h.db.MergeProgressOf(context.Background(), theWorkspace)
	if err != nil || !found {
		t.Fatalf("MergeProgressOf = (%v, %v), want the merge's record", found, err)
	}
	var doc progressDoc
	if err := json.Unmarshal(stored.Document, &doc); err != nil {
		t.Fatalf("decoding the record: %v", err)
	}
	return doc
}

// recordedStep is the step the harness workspace's progress record names.
func recordedStep(t *testing.T, h *harness) string {
	t.Helper()
	return recordedDoc(t, h).Step
}

// heldInGate runs the harness's merge into its test gate and suspends it there,
// as the daemon's exit does.
func heldInGate(t *testing.T, h *harness) {
	t.Helper()
	h.emacsRepo()
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
}

func TestTheTestsRecordNamesItsTipAndTargetWhenNothingWasReplayed(t *testing.T) {
	// Arrange: the branch is already on the target's tip, so no rebasing step
	// is recorded.
	h := newHarness(t)
	h.git.ancestry["0000000000000000000000000000000000000000>feature"] = true

	// Act.
	heldInGate(t, h)

	// Assert.
	doc := recordedDoc(t, h)
	if doc.Step != TabTests || doc.Tip == "" || doc.TargetBranch != "master" {
		t.Fatalf("record = step %q tip %q target %q, want the tests step naming its tip and target", doc.Step, doc.Tip, doc.TargetBranch)
	}
}

// TestTheQueueRecordCarriesTheAwaitingFreeStep pins what a resume draws for a
// merge suspended while it waited for its workspace: the awaiting_free step
// (owner ruling, 2026-10-08), never a step it has not started.
func TestTheQueueRecordCarriesTheAwaitingFreeStep(t *testing.T) {
	// Arrange: a merge held waiting for its workspace to fall free.
	h := newHarness(t)
	h.freeness.busy = true
	h.freeness.waiting = make(chan struct{})
	h.freeness.release = make(chan struct{})
	t.Cleanup(func() { close(h.freeness.release) })
	enqueue(t, h)
	done := admitAsync(h, context.Background())
	<-h.freeness.waiting

	// Act.
	h.o.Drain(context.Background())
	<-done

	// Assert.
	if doc := recordedDoc(t, h); doc.Step != TabQueue || doc.Facts.Step != "awaiting_free" {
		t.Fatalf("record = step %q facts %q, want the queue step with its footer step", doc.Step, doc.Facts.Step)
	}
}

func TestAnAbandonedRunTakesNoStopAtItsStepBoundary(t *testing.T) {
	// Arrange: a restart asked the run to stop at its boundary, and the run
	// was abandoned since.
	h := newHarness(t)
	r := liveRun(t, h, theWorkspace)
	h.o.mu.Lock()
	r.stopAtBoundary = true
	r.abandonedBy = CauseUserDrop
	h.o.mu.Unlock()

	// Act.
	stopped := r.stopAtStepBoundary()

	// Assert.
	if stopped {
		t.Fatal("an abandoned run stopped for the restart instead of ending")
	}
	if _, suspended := r.suspendedFor(); suspended {
		t.Fatal("the restart took an abandoned run")
	}
}

func TestEverySubjectFieldSurvivesTheRecord(t *testing.T) {
	// Arrange: a subject with every field set to a value distinct from its zero.
	var want subject
	v := reflect.ValueOf(&want).Elem()
	for i := 0; i < v.NumField(); i++ {
		f := v.Field(i)
		f = reflect.NewAt(f.Type(), unsafe.Pointer(f.UnsafeAddr())).Elem()
		switch f.Kind() {
		case reflect.String:
			f.SetString("set-" + v.Type().Field(i).Name)
		case reflect.Bool:
			f.SetBool(true)
		default:
			t.Fatalf("subject.%s is a %s this test cannot set; teach it, and carry the field through subjectDoc", v.Type().Field(i).Name, f.Kind())
		}
	}

	// Act: through the record and its JSON, as a restart reads it back.
	raw, err := json.Marshal(toSubjectDoc(want))
	if err != nil {
		t.Fatalf("marshal: %v", err)
	}
	var doc subjectDoc
	if err := json.Unmarshal(raw, &doc); err != nil {
		t.Fatalf("unmarshal: %v", err)
	}
	got := doc.subject()

	// Assert
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("subject through the record = %+v, want %+v: a field a restart would drop", got, want)
	}
}

func TestAResumedAsyncMergeKeepsItsLandingEffects(t *testing.T) {
	// Arrange: an async merge asked to close another workspace and clean up a worktree.
	recorded := subject{branch: "pt2", closes: "ws-merge", closesAlso: "ws-maker", cleanupDir: "/wt/pt-semver"}

	// Act
	resumed := toSubjectDoc(recorded).subject()

	// Assert
	if resumed.closesAlso != "ws-maker" || resumed.cleanupDir != "/wt/pt-semver" {
		t.Fatalf("resumed subject = %+v, want its close and cleanup kept", resumed)
	}
}

// TestARestoredRunTakesBackItsOwnQueueRound covers restoreFrom's queue round:
// a merge put back in line opens a later queue round on its one ledger
// (openLedger), so a restore stands the record's queue round, open or
// settled, never the first.
func TestARestoredRunTakesBackItsOwnQueueRound(t *testing.T) {
	tests := []struct {
		name     string
		settle   bool
		wantOpen bool
	}{
		{name: "an open queue round is taken back open", wantOpen: true},
		{name: "a settled queue round is taken back by its number", settle: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the ledger already holds the merge's first queue round.
			h := newHarness(t)
			r := ledgeredRun(t, h)
			h.db.mu.Lock()
			h.db.ledger[theWorkspace][0].Intervals = []wsm.TabInterval{{Round: 1, Kind: TabQueue, StartedAt: h.clock()}}
			h.db.mu.Unlock()
			if err := r.openLedger(context.Background()); err != nil {
				t.Fatalf("openLedger: %v", err)
			}
			queue := r.openTab(context.Background(), TabQueue)
			if tc.settle {
				r.closeTab(context.Background(), queue, "succeeded")
			}
			if err := r.checkpoint(context.Background(), TabQueue, nil); err != nil {
				t.Fatalf("checkpoint: %v", err)
			}
			restored := ledgeredRun(t, h)

			// Act.
			restored.restoreFrom(r.prog)

			// Assert.
			if restored.queueRound.n != 2 {
				t.Fatalf("queue round = %+v, want round 2", restored.queueRound)
			}
			if _, open := restored.openRounds[restored.queueRound.key()]; open != tc.wantOpen {
				t.Fatalf("queue round open = %v, want %v", open, tc.wantOpen)
			}
		})
	}
}
