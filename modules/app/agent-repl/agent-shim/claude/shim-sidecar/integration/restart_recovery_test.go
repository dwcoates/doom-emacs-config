package integration

import (
	"context"
	"testing"
	"time"

	storev1 "agentrepl/proto/store/v1"
)

// SUBJECT 5 — restart recovery: no gaps and no repeats.
//
// The cursor rides the batch and commits in the SAME store transaction as the
// records read at that position, which is the exactly-once guarantee. A
// restarted sidecar resumes from the store's cursor — REWOUND to the in-progress
// turn's first record per R10 — and the re-emitted records mint IDENTICAL
// write_ids, so the store absorbs them as success.

// TestARestartLeavesNoGapAndNoRepeat stops the sidecar mid-file, grows the file,
// restarts, and asserts the book holds each unit exactly once.
func TestARestartLeavesNoGapAndNoRepeat(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	store := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	opts := defaultSidecarOptions(t, store.Socket, tree)
	cut := 9 // stop after the first response's blocks have been written

	// Act: read the head of the file, stop, grow it, read the rest.
	first := startSidecar(t, opts)
	g := newGrowingFile(t, tree.sessionPath(captured.Slug, captured.Session))
	for _, line := range captured.Lines[:cut] {
		g.AppendLine(line)
	}
	awaitCursorAtLeast(ctx, t, store.Client, g.Path(), 1)
	first.Stop()

	for _, line := range captured.Lines[cut:] {
		g.AppendLine(line)
	}
	startSidecar(t, opts)
	lines := store.awaitBookUnits(ctx, t, captured.Session,
		capturedThinking1, capturedBashCall1, capturedThinking2, capturedBashCall2)

	// Assert: no unit appears twice, and the four expected units are all there.
	seen := map[string]int{}
	for _, at := range lines {
		if a := activityOf(at); a != nil {
			seen[a.GetActivityId().GetValue()]++
		}
	}
	for id, n := range seen {
		if n != 1 {
			t.Errorf("unit %q appears %d times in the book after a restart", id, n)
		}
	}
	for _, want := range []string{capturedThinking1, capturedBashCall1, capturedThinking2, capturedBashCall2} {
		if seen[want] == 0 {
			t.Errorf("unit %q is missing after a restart; the book holds %v", want, sortedStrings(keysOf(toSet(seen))))
		}
	}
}

// TestARestartMintsIdenticalWriteIdsForReplayedRecords asserts a re-read yields
// the identical write_id, which is what makes absorption possible at all.
func TestARestartMintsIdenticalWriteIdsForReplayedRecords(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	opts := defaultSidecarOptions(t, fake.Socket, tree)

	// Act: ingest the whole file, stop, and start again over an empty-cursor
	// store so every record is necessarily re-read.
	first := startSidecar(t, opts)
	g := newGrowingFile(t, tree.sessionPath(captured.Slug, captured.Session))
	for _, line := range captured.Lines {
		g.AppendLine(line)
	}
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())
	firstPass := writeIDsByKey(fake.Entries())
	first.Stop()

	before := fake.BatchCount()
	startSidecar(t, opts)
	fake.awaitBatches(ctx, t, before+1)
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())

	// Assert.
	secondPass := writeIDsByKey(fake.Entries()[countEntriesInBatches(fake.Batches()[:before]):])
	for key, id := range secondPass {
		if was, ok := firstPass[key]; ok && was != id {
			t.Errorf("unit %q minted write_id %q on the first read and %q on the second; the digest must be deterministic",
				key, was, id)
		}
	}
}

// TestARestartRewindsToTheInProgressTurnsFirstRecord asserts R10's rewind: the
// sidecar is stopped between a tool_use line and its tool_result, the result is
// then appended, and the result UPSERTS its call rather than landing as residue.
func TestARestartRewindsToTheInProgressTurnsFirstRecord(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	store := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	opts := defaultSidecarOptions(t, store.Socket, tree)

	// Act: stop with the call written and its result still to come.
	first := startSidecar(t, opts)
	g := newGrowingFile(t, tree.sessionPath(captured.Slug, captured.Session))
	for _, line := range captured.Lines[:9] { // through the Bash call, before its result
		g.AppendLine(line)
	}
	// THE WAIT IS ON THE CALL'S OWN UNIT, not on a line count. The subject's
	// whole premise is that the reader was stopped with the CALL written and
	// its result still to come; "at least one line" is satisfied by the user
	// prompt that precedes it, which would stop the reader before the premise
	// held.
	store.awaitBookUnits(ctx, t, captured.Session, capturedBashCall1)
	first.Stop()

	for _, line := range captured.Lines[9:11] { // the hook attachment and the tool_result
		g.AppendLine(line)
	}
	startSidecar(t, opts)
	// And the wait AFTER the restart is on the restarted reader's own durable
	// statement that it read the appended bytes. "At least two lines" is
	// already true of the book the FIRST reader left behind, so it waited for
	// nothing at all and read back a book the result had not reached.
	awaitCursorAtLeast(ctx, t, store.Client, g.Path(), g.Offset())
	lines := store.Book(captured.Session)

	// Assert: the call's unit is settled in place, and nothing doubled.
	var calls int
	for _, at := range lines {
		a := activityOf(at)
		if a == nil || a.GetActivityId().GetValue() != capturedBashCall1 {
			continue
		}
		calls++
		if a.GetBash() == nil {
			t.Errorf("the settled unit %q lost its bash item", capturedBashCall1)
		}
	}
	if calls != 1 {
		t.Fatalf("the call's unit appears %d times after the rewind, wanted exactly once", calls)
	}
}

// writeIDsByKey indexes each upsert_key's most recent write_id.
func writeIDsByKey(entries []*storev1.StoreEntry) map[string]string {
	out := make(map[string]string, len(entries))
	for _, e := range entries {
		out[e.GetUpsertKey()] = e.GetWriteId()
	}
	return out
}

func countEntriesInBatches(batches []*storev1.WriteBatchRequest) int {
	return len(entriesOf(batches))
}

func toSet(counts map[string]int) map[string]bool {
	out := make(map[string]bool, len(counts))
	for k := range counts {
		out[k] = true
	}
	return out
}

// TestAReReadOfAResidueRecordStoresNothingTwice re-aims what used to be a
// subject about ONE RESIDUE ROW ingested twice. Residue is never persisted now,
// so there is no row for the vendor's uuid to key and no write id to compare:
// the idempotence that survives is the reader's, not the store's.
//
// WHAT THE RESTART STILL GUARANTEES, and what this pins:
//   - a re-read is the ORDINARY case — the boot rewind re-reads the in-progress
//     turn on every restart by design — so the same residue records really are
//     classified a second time, which the withholding records show;
//   - and the second pass stores NOTHING for them: no residue row on either
//     pass, and no upsert_key the first pass had not already written, so the
//     replay adds nothing beside itself.
func TestAReReadOfAResidueRecordStoresNothingTwice(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	// The withholding records are the only trace a re-read of residue leaves.
	opts := debugLogging(defaultSidecarOptions(t, fake.Socket, tree))

	// Act: ingest the whole file, stop, and start again over a store that holds
	// no cursor, so every record is necessarily read a second time.
	first := startSidecar(t, opts)
	g := newGrowingFile(t, tree.sessionPath(captured.Slug, captured.Session))
	for _, line := range captured.Lines {
		g.AppendLine(line)
	}
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())
	firstPassKeys := upsertKeySet(fake.Entries())
	firstPassDrops := len(residueDropRecords(t, opts.LogPath))
	first.Stop()

	before := fake.BatchCount()
	startSidecar(t, opts)
	fake.awaitBatches(ctx, t, before+1)
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())
	secondPass := fake.Entries()[countEntriesInBatches(fake.Batches()[:before]):]

	// Assert: the capture really does hold residue, or nothing was checked.
	if firstPassDrops == 0 {
		t.Fatal("the capture produced no residue at all, so nothing was checked")
	}
	// The second reader classified it again — the re-read happened.
	awaitResidueDropsAtLeast(ctx, t, opts.LogPath, firstPassDrops+1)
	// Nor anything else the first pass had not already written: a replay lands
	// on the rows it landed on before, never beside them.
	for _, e := range secondPass {
		if !firstPassKeys[e.GetUpsertKey()] {
			t.Errorf("the re-read wrote a NEW row %q; a replay must land on the rows the first pass wrote", e.GetUpsertKey())
		}
	}
}

// residueDropRecords answers every withholding record the sidecar has written,
// so a subject can count re-classifications rather than only name them.
func residueDropRecords(t *testing.T, logPath string) []logRecord {
	t.Helper()
	var out []logRecord
	for _, r := range readLog(t, logPath) {
		if r.Operation == "residue-drop" {
			out = append(out, r)
		}
	}
	return out
}

// awaitResidueDropsAtLeast waits until the sidecar has said it classified and
// withheld at least n records. A COUNT rather than a match, because the fact
// under test is that the SAME residue was classified a second time, which no
// single record can state.
func awaitResidueDropsAtLeast(ctx context.Context, t *testing.T, logPath string, n int) {
	t.Helper()
	tick := time.NewTicker(pollTick)
	defer tick.Stop()
	for {
		got := len(residueDropRecords(t, logPath))
		if got >= n {
			return
		}
		select {
		case <-ctx.Done():
			t.Fatalf("the sidecar withheld %d residue record(s), wanted at least %d, within the deadline", got, n)
		case <-tick.C:
		}
	}
}
