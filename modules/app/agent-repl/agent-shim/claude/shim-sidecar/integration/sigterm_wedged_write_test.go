package integration

import (
	"strings"
	"syscall"
	"testing"
	"time"

	storev1 "agentrepl/proto/store/v1"
)

// SUBJECT — SIGTERM WHILE THE STORE IS WEDGED.
//
// The cycle runs on one goroutine, so a sidecar stopped inside WriteBatch could
// not read its signal channel until the call returned. With a store that
// accepted the write and then stopped answering, that was rpcTimeout — 30s of a
// process that had been asked to leave. The signal now ends the wedged call
// after a short settle (cycle.go, shutdownSettle) rather than waiting out the
// timeout, so the cycle exits through its ordinary shutdown record.
//
// NOTHING IS LOST, AND NOT BECAUSE THE WRITE WAS UNDONE. A write already on the
// wire cannot be recalled: the store may commit it after this process is gone.
// What makes that safe is that records and cursor ride ONE transaction, so the
// store ends up holding both or neither, and the next boot resumes from
// whichever cursor it holds — re-reading the same durable bytes into the same
// deterministic write ids either way.

// wedgedShutdownBudget bounds a shutdown that must not wait out rpcTimeout. A
// healthy signalled sidecar leaves in milliseconds and a wedged one just past
// the 250ms settle it gives a sent write; 3s is a wide multiple of that and an
// order of magnitude below the 30s this subject exists to forbid.
const wedgedShutdownBudget = 3 * time.Second

// anyBatch gates the first write of any shape — this subject cares only that
// the producer is stopped inside a store call, not which one.
func anyBatch(*storev1.WriteBatchRequest) bool { return true }

// signalAndTime sends SIGTERM and answers how long the process took to leave.
// It never releases the gate: the whole point is that the process leaves while
// the store is still not answering.
func signalAndTime(t *testing.T, p *sidecarProc, budget time.Duration) time.Duration {
	t.Helper()
	p.stopped = true // this subject owns the stop; the cleanup must not repeat it
	start := time.Now()
	if err := p.cmd.Process.Signal(syscall.SIGTERM); err != nil {
		t.Fatalf("signalling the sidecar: %v", err)
	}
	select {
	case <-p.done:
		return time.Since(start)
	case <-time.After(budget):
		_ = p.cmd.Process.Kill()
		<-p.done
		t.Fatalf("the sidecar was still wedged in its store write %s after SIGTERM (rpc timeout is 30s)", budget)
		return 0
	}
}

// TestSigtermLeavesAWedgedStoreWritePromptly asserts the shutdown latency of a
// sidecar frozen inside a store call it will never get an answer to.
func TestSigtermLeavesAWedgedStoreWritePromptly(t *testing.T) {
	t.Parallel()
	// Arrange: a store that takes the write and never answers.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	gate := fake.gateOnBatch(t, anyBatch)
	proc := startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	g := newGrowingFile(t, tree.sessionPath(captured.Slug, captured.Session))
	for _, line := range captured.Lines[:9] {
		g.AppendLine(line)
	}
	gate.await(ctx, t, "the sidecar's first batch")

	// Act.
	elapsed := signalAndTime(t, proc, wedgedShutdownBudget)

	// Assert.
	if elapsed >= wedgedShutdownBudget {
		t.Fatalf("shutdown took %s, want prompt (<%s) with the store still wedged", elapsed, wedgedShutdownBudget)
	}
	t.Logf("the wedged sidecar left %s after SIGTERM", elapsed)
}

// TestAWedgedShutdownStatesTheInterruptedWrite asserts the record: the
// interrupted write is not swallowed, it is stated — as a write whose outcome
// this process never learned, which is the honest thing to say about a batch
// already on the wire when the settle window ran out.
func TestAWedgedShutdownStatesTheInterruptedWrite(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	gate := fake.gateOnBatch(t, anyBatch)
	proc := startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	g := newGrowingFile(t, tree.sessionPath(captured.Slug, captured.Session))
	for _, line := range captured.Lines[:9] {
		g.AppendLine(line)
	}
	gate.await(ctx, t, "the sidecar's first batch")

	// Act.
	signalAndTime(t, proc, wedgedShutdownBudget)

	// Assert.
	const record = "shutdown interrupted a write that had not answered within"
	got := awaitLog(ctx, t, proc.LogPath, record, func(r logRecord) bool {
		return r.Operation == "shutdown" && strings.Contains(r.Message, record)
	})
	if got.Level != "" && got.Level != "info" {
		t.Fatalf("the replay record is level %q, want info: an interrupted write is not an outage", got.Level)
	}
}
