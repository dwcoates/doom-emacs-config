package merge

import (
	"context"
	"encoding/json"
	"testing"
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

func TestTheQueueRecordCarriesTheEnqueuedStep(t *testing.T) {
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
	if doc := recordedDoc(t, h); doc.Step != TabQueue || doc.Facts.Step != "enqueued" {
		t.Fatalf("record = step %q facts %q, want the queue step with its footer step", doc.Step, doc.Facts.Step)
	}
}
