package integration

import (
	"encoding/json"
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
	lines := awaitBookUnits(ctx, t, store.Client, captured.Session,
		capturedThinking1, capturedBashCall1, capturedThinking2, capturedBashCall2)

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

// TestASeededCursorIsResumedFromTheInProgressTurnsFirstRecord pins the project
// lead's REWIND RULING against a seeded cursor.
//
// THE RULING. On boot each tailer resumes from the store's cursor REWOUND to the
// in-progress turn's first record: re-emitted records mint identical write_ids
// and are absorbed where the store already holds them, and land where it does
// not. So "only the tail is written" is NOT the contract — a cursor mid-turn is
// deliberately walked back, because a converter's joins are in memory and a turn
// read half before a restart has no open call for its results to settle.
//
// What the rewind does NOT license is re-reading EARLIER turns: the scan stops
// at the last turn start at or before the cursor, so records from before it must
// not land. The captured transcript opens with vendor bookkeeping (its
// queue-operation lines) ahead of the only user prompt in the file, which is
// exactly that "before the turn" region.
func TestASeededCursorIsResumedFromTheInProgressTurnsFirstRecord(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	path := tree.sessionPath(captured.Slug, captured.Session)
	opts := defaultSidecarOptions(t, fake.Socket, tree)

	g := newGrowingFile(t, path)
	var headBytes int64
	for i, line := range captured.Lines {
		start := g.AppendLine(line)
		if i == 8 { // through the first response's Bash call
			headBytes = start + int64(len(line)) + 1
		}
	}
	turnStart := turnStartOffsetAtOrBefore(t, captured.Lines, headBytes)
	fake.SeedCursors(&storev1.CursorState{
		FileId: fileID(t, path),
		Path:   path,
		Offset: headBytes,
	})

	// Act.
	startSidecar(t, opts)
	awaitCursorInBatches(ctx, t, fake, path, g.Offset())

	// Assert: the tail after the cursor landed.
	held := upsertKeySet(fake.Entries())
	if !held["activity:"+capturedBashCall2] {
		t.Errorf("unit %q lies after the seeded cursor and must be written; keys held: %v",
			capturedBashCall2, sortedStrings(keysOf(held)))
	}

	// Assert: the rewind is a STATED decision naming the offset it rewound to,
	// and that offset is the in-progress turn's first record.
	rec := awaitLog(ctx, t, opts.LogPath, "the boot rewind record", func(r logRecord) bool {
		return r.Operation == "boot-rewind" && samePathAny(r.Context["path"], path) &&
			strings.Contains(r.Message, "rewound")
	})
	rewound, ok := rec.Context["offset"].(float64)
	if !ok {
		t.Fatalf("the rewind record must name the offset it rewound to; its context was %v", rec.Context)
	}
	if int64(rewound) != turnStart {
		t.Errorf("rewound to offset %d, wanted the in-progress turn's first record at %d", int64(rewound), turnStart)
	}
	if int64(rewound) > headBytes {
		t.Errorf("rewound to offset %d, which is PAST the seeded cursor at %d", int64(rewound), headBytes)
	}

	// Assert: nothing from before that turn was re-read. The file's
	// queue-operation lines are the only records ahead of its single turn start.
	for _, kind := range vendorSpecificKinds(fake.Entries()) {
		if kind == "queue-operation" {
			t.Errorf("a record from BEFORE the in-progress turn was written; the rewind stops at the turn's first record, not at the file's")
		}
	}
}

// turnStartOffsetAtOrBefore answers the byte offset of the last turn-opening
// record at or before limit — a user record carrying PROSE rather than a
// tool_result, which is the boundary the production rewind scans back to.
// Computed here from the fixture's own bytes so the subject never imports the
// production predicate it is checking.
func turnStartOffsetAtOrBefore(t *testing.T, lines []string, limit int64) int64 {
	t.Helper()
	var offset, found int64
	found = -1
	for _, line := range lines {
		start := offset
		offset += int64(len(line)) + 1
		if start >= limit {
			break
		}
		var rec struct {
			Type    string `json:"type"`
			IsMeta  bool   `json:"isMeta"`
			Message struct {
				Content any `json:"content"`
			} `json:"message"`
		}
		if err := json.Unmarshal([]byte(line), &rec); err != nil {
			t.Fatalf("fixture line is not JSON: %v", err)
		}
		if rec.Type != "user" || rec.IsMeta {
			continue
		}
		switch content := rec.Message.Content.(type) {
		case string:
			if content != "" {
				found = start
			}
		case []any:
			for _, raw := range content {
				block, ok := raw.(map[string]any)
				if !ok {
					continue
				}
				if block["type"] == "text" {
					found = start
					break
				}
			}
		}
	}
	if found < 0 {
		t.Fatalf("the fixture holds no turn start at or before offset %d", limit)
	}
	return found
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
