package integration

import (
	"path/filepath"
	"strings"
	"testing"

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
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	store := startRealStore(t)
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
	lines := awaitBookLines(ctx, t, store.Client, captured.Session, 4)

	// Assert: no unit appears twice, and the four expected units are all there.
	seen := map[string]int{}
	for _, at := range lines {
		if a := activityOf(at.GetLine()); a != nil {
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

// TestAStoreCursorIsHonoredSoOnlyTheTailIsWritten seeds a cursor mid-file and
// asserts the sidecar writes only what follows it.
func TestAStoreCursorIsHonoredSoOnlyTheTailIsWritten(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	path := tree.sessionPath(captured.Slug, captured.Session)

	g := newGrowingFile(t, path)
	var headBytes int64
	for i, line := range captured.Lines {
		start := g.AppendLine(line)
		if i == 8 { // through the first response's Bash call
			headBytes = start + int64(len(line)) + 1
		}
	}
	fake.SeedCursors(&storev1.CursorState{
		FileId: fileID(t, path),
		Path:   path,
		Offset: headBytes,
	})

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	awaitCursorInBatches(ctx, t, fake, path, g.Offset())

	// Assert: nothing before the seeded offset was re-read.
	held := map[string]bool{}
	for _, e := range fake.Entries() {
		held[e.GetUpsertKey()] = true
	}
	if held["activity:"+capturedThinking1] {
		t.Errorf("unit %q lies before the seeded cursor and must not be written", capturedThinking1)
	}
	if !held["activity:"+capturedBashCall2] {
		t.Errorf("unit %q lies after the seeded cursor and must be written; keys held: %v",
			capturedBashCall2, sortedStrings(keysOf(held)))
	}
}

// TestAFreshStoreReadsEveryFileFromZero asserts an empty GetSidecarCursors
// answer is the legitimate fresh-store answer, not a failure.
func TestAFreshStoreReadsEveryFileFromZero(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	fake.SeedCursors() // a fresh store holds none

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	g := newGrowingFile(t, tree.sessionPath(captured.Slug, captured.Session))
	for _, line := range captured.Lines {
		g.AppendLine(line)
	}
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())

	// Assert: the file's very first record was read.
	if !upsertKeySet(fake.Entries())["activity:"+capturedThinking1] {
		t.Fatalf("a fresh store must start every file from zero; the first response's unit was never written")
	}
}

// TestARestartRewindsToTheInProgressTurnsFirstRecord asserts R10's rewind: the
// sidecar is stopped between a tool_use line and its tool_result, the result is
// then appended, and the result UPSERTS its call rather than landing as residue.
func TestARestartRewindsToTheInProgressTurnsFirstRecord(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	store := startRealStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	opts := defaultSidecarOptions(t, store.Socket, tree)

	// Act: stop with the call written and its result still to come.
	first := startSidecar(t, opts)
	g := newGrowingFile(t, tree.sessionPath(captured.Slug, captured.Session))
	for _, line := range captured.Lines[:9] { // through the Bash call, before its result
		g.AppendLine(line)
	}
	awaitBookLines(ctx, t, store.Client, captured.Session, 1)
	first.Stop()

	for _, line := range captured.Lines[9:11] { // the hook attachment and the tool_result
		g.AppendLine(line)
	}
	startSidecar(t, opts)
	lines := awaitBookLines(ctx, t, store.Client, captured.Session, 2)

	// Assert: the call's unit is settled in place, and nothing doubled.
	var calls int
	for _, at := range lines {
		a := activityOf(at.GetLine())
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

// TestARewindIsStatedInTheLog asserts the rewind is a stated decision, naming
// the offset it rewound to, rather than a silent re-read.
func TestARewindIsStatedInTheLog(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	store := startRealStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	opts := defaultSidecarOptions(t, store.Socket, tree)
	secondLog := filepath.Join(t.TempDir(), "sidecar-restarted.log")

	// Act.
	first := startSidecar(t, opts)
	g := newGrowingFile(t, tree.sessionPath(captured.Slug, captured.Session))
	for _, line := range captured.Lines[:9] {
		g.AppendLine(line)
	}
	awaitBookLines(ctx, t, store.Client, captured.Session, 1)
	first.Stop()

	restarted := opts
	restarted.LogPath = secondLog
	startSidecar(t, restarted)

	// Assert.
	rec := awaitLog(ctx, t, secondLog, "the boot rewind record", func(r logRecord) bool {
		return strings.Contains(strings.ToLower(r.Operation+" "+r.Message), "rewind") &&
			samePathAny(r.Context["path"], g.Path())
	})
	if _, ok := rec.Context["offset"]; !ok {
		t.Errorf("the rewind record must name the offset it rewound to; its context was %v", rec.Context)
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
