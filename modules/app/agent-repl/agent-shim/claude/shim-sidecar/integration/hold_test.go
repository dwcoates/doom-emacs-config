package integration

import (
	"context"
	"strings"
	"testing"
	"time"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/convert"
)

// SUBJECT 8 — THE HOLD, the one legitimate deferral.
//
// A compaction boundary's meaning depends on the line that follows it: the
// summary that replaced the history. The boundary is written ~1ms before its
// summary, so a poll lands between them. Rather than convert on incomplete
// evidence, the handler HOLDS the trailing frame and the cursor advances SHORT
// of what was read — to the held frame's offset — so the next scan, and a
// restart, both read it again. The hold is bounded to ONE redelivery.

// holdOptions gives a hold subject the PRODUCTION poll interval and the
// interlock that makes it usable.
//
// THE HOLD IS BOUNDED TO ONE REDELIVERY, so a subject that must act between the
// first delivery and the second — append a summary, or read the store while the
// boundary is still held — is acting inside one poll interval. This used to be
// arranged by stretching the interval to 500ms (and to 5s in the restart
// subject) so the second event was far enough away that the test usually won;
// that is a race the subject wins rather than one it cannot lose, and it made
// this family the slowest in the package.
//
// The store is the interlock the system already has: a batch is written
// synchronously inside the cycle and the cursor advances only on a durable
// success, so a store that has not answered is a cycle that has not moved on.
// Each subject below arms a writeGate on the batch that PARKS the cursor and
// does its work while the producer is stopped inside that write. The interval
// is then irrelevant to correctness, so it is production's.
func holdOptions(t *testing.T, storeSocket string, tree *vendorTree) sidecarOptions {
	t.Helper()
	return defaultSidecarOptions(t, storeSocket, tree)
}

// awaitParkedCursor waits for the batch that carried the PARKED cursor — the one
// stating the hold, at exactly the held frame's offset.
//
// It is the hold's own observable event. Waiting for "any cursor" instead cannot
// tell a parked cursor from one that already advanced past the boundary, which
// is the difference every subject in this file turns on.
func awaitParkedCursor(ctx context.Context, t *testing.T, f *fakeStore, path string, held int64) {
	t.Helper()
	tick := time.NewTicker(pollTick)
	defer tick.Stop()
	for {
		for _, b := range f.Batches() {
			cursor := b.GetBatch().GetCursorAdvance()
			if cursor != nil && samePath(cursor.GetPath(), path) && cursor.GetOffset() == held {
				return
			}
		}
		select {
		case <-ctx.Done():
			t.Fatalf("no batch parked the cursor at the held boundary %d for %s within the deadline", held, path)
		case <-tick.C:
		}
	}
}

// compactionFixture writes a session whose last line is a compaction boundary.
type compactionFixture struct {
	Slug           string
	Session        string
	Cwd            string
	BoundaryUUID   string
	BoundaryOffset int64
	File           *growingFile
}

func seedCompactionBoundary(t *testing.T, tree *vendorTree, cwd, session string) compactionFixture {
	t.Helper()
	captured := loadCapturedSession(t)
	slug := cwdSlug(cwd)

	boundary := retargetSession(t, decodeRecord(t, corpusLine(t, "transcript-lines/system-compact_boundary.jsonl", 0)), session, cwd)
	boundaryUUID, _ := boundary["uuid"].(string)
	if boundaryUUID == "" {
		t.Fatalf("the compact_boundary fixture carries no uuid")
	}

	g := newGrowingFile(t, tree.sessionPath(slug, session))
	// A little ordinary work first, so the boundary is not the file's only line.
	g.AppendLine(encodeRecord(t, retargetSession(t, decodeRecord(t, captured.Lines[7]), session, cwd)))
	offset := g.AppendLine(encodeRecord(t, boundary))

	return compactionFixture{
		Slug: slug, Session: session, Cwd: cwd,
		BoundaryUUID: boundaryUUID, BoundaryOffset: offset, File: g,
	}
}

// TestABoundaryWithoutItsSummaryParksTheCursorShort asserts the cursor advances
// only to the held frame's offset, so the boundary is read again.
func TestABoundaryWithoutItsSummaryParksTheCursorShort(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	fx := seedCompactionBoundary(t, tree, "/Users/dodgecoates/hold-cursor-probe",
		"70707070-7070-4070-8070-707070707070")

	// Act: the producer is STOPPED inside the write that parks the cursor, so
	// the position asserted below is the one the hold produced and not whatever
	// the forced redelivery left a moment later.
	gate := fake.gateOnBatch(t, cursorParkedAt(fx.File.Path(), fx.BoundaryOffset))
	// THE PRODUCER IS LET GO WHEN THIS SUBJECT IS DONE WITH IT, before the
	// harness stops it: a sidecar still inside a withheld write does not see
	// SIGTERM until its own rpc timeout expires, so releasing the gate is part
	// of the subject rather than left to a cleanup that runs after the stop.
	defer gate.release()
	startSidecar(t, holdOptions(t, fake.Socket, tree))
	gate.await(ctx, t, "the batch parking the cursor at the held boundary")
	awaitParkedCursor(ctx, t, fake, fx.File.Path(), fx.BoundaryOffset)

	// Assert: the parked cursor is exactly the held frame's offset. Anything
	// short of it would re-read converted records; anything past it would have
	// consumed the boundary the hold exists to defer.
	cs := latestCursorFor(fake.Batches(), fx.File.Path())
	if cs == nil {
		t.Fatalf("the sidecar offered no cursor for %s", fx.File.Path())
	}
	if cs.GetOffset() != fx.BoundaryOffset {
		t.Errorf("the cursor parked at %d, want the held boundary's offset %d", cs.GetOffset(), fx.BoundaryOffset)
	}
}

// TestABoundaryWithoutItsSummaryWritesNothingForIt asserts the held record is
// not converted on incomplete evidence.
func TestABoundaryWithoutItsSummaryWritesNothingForIt(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	fx := seedCompactionBoundary(t, tree, "/Users/dodgecoates/hold-nothing-probe",
		"80808080-8080-4080-8080-808080808080")

	// Act: the producer is STOPPED inside the write that parks the cursor, so
	// "nothing was written for the boundary" is asserted while the hold still
	// stands rather than in whatever window is left before the forced
	// redelivery converts it.
	gate := fake.gateOnBatch(t, cursorParkedAt(fx.File.Path(), fx.BoundaryOffset))
	// THE PRODUCER IS LET GO WHEN THIS SUBJECT IS DONE WITH IT, before the
	// harness stops it: a sidecar still inside a withheld write does not see
	// SIGTERM until its own rpc timeout expires, so releasing the gate is part
	// of the subject rather than left to a cleanup that runs after the stop.
	defer gate.release()
	startSidecar(t, holdOptions(t, fake.Socket, tree))
	gate.await(ctx, t, "the batch parking the cursor at the held boundary")

	// Assert.
	wantKey := "session:context_cut:" + fx.BoundaryUUID
	if entryByUpsertKey(fake.Entries(), wantKey) != nil {
		t.Errorf("the boundary was converted while its summary was still absent (key %q)", wantKey)
	}
}

// TestTheSummaryCoalescesWithItsBoundaryIntoOneRecord asserts the boundary and
// the summary become ONE ContextCut page line of the main agent's book.
func TestTheSummaryCoalescesWithItsBoundaryIntoOneRecord(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	fx := seedCompactionBoundary(t, tree, "/Users/dodgecoates/hold-coalesce-probe",
		"90909090-9090-4090-8090-909090909090")
	summaryText := "Previously: the harness held a background probe alive."

	// Act: the summary is appended while the producer is STOPPED inside the
	// write that parks the cursor. The park is the event that says the boundary
	// was held; the gate is what guarantees the forced redelivery has not run
	// yet, rather than hoping the append wins a poll interval.
	gate := fake.gateOnBatch(t, cursorParkedAt(fx.File.Path(), fx.BoundaryOffset))
	startSidecar(t, holdOptions(t, fake.Socket, tree))
	gate.await(ctx, t, "the batch parking the cursor at the held boundary")
	fx.File.AppendLine(compactSummaryLine(t, fx.Session, fx.Cwd,
		"90909090-9090-4090-8090-90909090abcd", fx.BoundaryUUID, summaryText))
	gate.release()

	wantKey := "session:context_cut:" + fx.BoundaryUUID
	fake.awaitEntry(ctx, t, "the coalesced context cut", func(e *storev1.StoreEntry) bool {
		return e.GetUpsertKey() == wantKey
	})

	// Assert: exactly one record, on the main agent's book, on the compacted arm.
	entries := fake.Entries()
	if n := countUpsertKey(entries, wantKey); n != 1 {
		t.Errorf("the boundary and its summary produced %d records under %q, wanted one coalesced row", n, wantKey)
	}
	e := entryByUpsertKey(entries, wantKey)
	line := e.GetAgentUpdate().GetServeableFrame()
	if line == nil {
		t.Fatalf("a context cut is a page line of the main agent's book: %v", e.GetAgentUpdate())
	}
	if got := line.GetPageAgentId().GetValue(); got != fx.Session {
		t.Errorf("the context cut names book %q, wanted the main agent %q", got, fx.Session)
	}
	cut := contextCutOf(line)
	if cut == nil {
		t.Fatalf("the page line carries no ContextCut: %v", line)
	}
	if cut.GetCompacted() == nil {
		t.Fatalf("a compaction boundary produces the compacted arm: %v", cut.GetCut())
	}
	if got := cut.GetCompacted().GetSummary().GetMarkdown(); got != summaryText {
		t.Errorf("the cut carries summary %q, wanted the summary line's prose %q", got, summaryText)
	}
}

// TestABoundaryRedeliveredTwiceIsConvertedRegardless asserts the hold is
// BOUNDED: on the second delivery the handler converts it whether or not the
// summary ever arrived, so nothing can be held forever.
func TestABoundaryRedeliveredTwiceIsConvertedRegardless(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	fx := seedCompactionBoundary(t, tree, "/Users/dodgecoates/hold-bounded-probe",
		"a0a0a0a0-a0a0-40a0-80a0-a0a0a0a0a0a0")

	// Act: the summary NEVER arrives; the file simply keeps being polled.
	opts := holdOptions(t, fake.Socket, tree)
	startSidecar(t, opts)
	wantKey := "session:context_cut:" + fx.BoundaryUUID
	fake.awaitEntry(ctx, t, "the boundary converted after its bounded redelivery", func(e *storev1.StoreEntry) bool {
		return e.GetUpsertKey() == wantKey
	})

	// Assert: it landed, and the cursor then moved past it.
	cs := awaitCursorPast(ctx, t, fake, fx.File.Path(), fx.BoundaryOffset)
	if cs.GetOffset() <= fx.BoundaryOffset {
		t.Errorf("the cursor stayed at %d after the bounded hold expired; a converted frame releases the cursor",
			cs.GetOffset())
	}

	// Assert the BOUND itself: the frame was held EXACTLY once. The tailer states
	// each rewind it performs, so more than one of those records is a frame held
	// twice — the unbounded deferral this file's whole design forbids — and none
	// would mean the record was converted without ever being held at all.
	//
	// FILE-SCOPED RECORDS ARRIVE THROUGH THE ASYNCHRONOUS ClientLog FORWARD, so
	// a snapshot taken the instant the store holds the entry can predate the
	// hold records. The forward is one FIFO queue, and the forced conversion's
	// own `hold` record is written after the first delivery's `tailer-hold`, so
	// once it has arrived every record this assertion counts has too.
	awaitLog(ctx, t, opts.LogPath, "the forced conversion's hold record", func(r logRecord) bool {
		return r.Operation == "hold" && strings.Contains(r.Message, "no summary followed")
	})
	records := readLog(t, opts.LogPath)
	rewinds := recordsFor(records, "tailer-hold")
	if len(rewinds) != 1 {
		t.Fatalf("the boundary was held %d times, want exactly one redelivery; the log held %v",
			len(rewinds), operationLevels(records))
	}
	if got := rewinds[0].Context["offset"]; got != float64(fx.BoundaryOffset) {
		t.Errorf("the hold rewound to offset %v, want the boundary at %d", got, fx.BoundaryOffset)
	}
	// And the handler converted it on the forced delivery rather than holding
	// again, which the tailer would have had to refuse.
	if got := recordsFor(records, "hold-exhausted"); len(got) != 0 {
		t.Errorf("the handler held the boundary again on its forced redelivery; the tailer had to refuse %d hold(s)", len(got))
	}
	// GIVING UP ON THE WAIT IS STATED, and stated at info: the cut a reader gets
	// from here carries the placeholder rather than a hole, and a summary that
	// names this boundary still supersedes it whenever it lands, so the bound
	// expiring degrades nothing. It must never be silent.
	if got := recordsAt(records, "hold", "warn"); len(got) != 0 {
		t.Errorf("the summary-less conversion was stated %d time(s) at warn, want none; the log held %v",
			len(got), operationLevels(records))
	}
	if got := recordsFor(records, "hold"); len(got) != 2 {
		t.Errorf("the hold was stated %d time(s), want the deferral and the expiry; the log held %v",
			len(got), operationLevels(records))
	}

	// And the cut that landed is the COMPACTED arm with no summary — not a
	// different arm, and not a fabricated one. A boundary whose summary never
	// arrived is still a compaction; what it lacks is the prose.
	e := entryByUpsertKey(fake.Entries(), wantKey)
	cut := contextCutOf(e.GetAgentUpdate().GetServeableFrame())
	if cut == nil {
		t.Fatalf("the converted boundary carries no ContextCut: %v", e.GetAgentUpdate())
	}
	if cut.GetCompacted() == nil {
		t.Fatalf("a compaction boundary produces the compacted arm even with no summary: %v", cut.GetCut())
	}
	// The summary's PROSE is the reader's own STATEMENT about the condition, and
	// never prose the vendor did not write. An empty markdown drew the cut as a
	// hole, which a reader cannot tell from a summary this pipeline lost; the
	// placeholder says which it is, and a summary naming this boundary replaces
	// it on the cut's own key if one ever arrives.
	if got := cut.GetCompacted().GetSummary().GetMarkdown(); got != convert.NoSummaryWritten {
		t.Errorf("the cut carries summary prose %q, want the stated placeholder %q", got, convert.NoSummaryWritten)
	}
}

// TestABoundaryHeldOnceIsNotWrittenTwice asserts the redelivered record is
// converted ONCE — the write_id absorbs a repeat rather than doubling the row.
func TestABoundaryHeldOnceIsNotWrittenTwice(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	fx := seedCompactionBoundary(t, tree, "/Users/dodgecoates/hold-once-probe",
		"b0b0b0b0-b0b0-40b0-80b0-b0b0b0b0b0b0")
	summaryText := "Previously: nothing much."

	// Act: the summary is appended while the producer is STOPPED inside the
	// write that parks the cursor, so the redelivery that follows is guaranteed
	// to be the one that reads it.
	gate := fake.gateOnBatch(t, cursorParkedAt(fx.File.Path(), fx.BoundaryOffset))
	startSidecar(t, holdOptions(t, fake.Socket, tree))
	gate.await(ctx, t, "the batch parking the cursor at the held boundary")
	fx.File.AppendLine(compactSummaryLine(t, fx.Session, fx.Cwd,
		"b0b0b0b0-b0b0-40b0-80b0-b0b0b0b0abcd", fx.BoundaryUUID, summaryText))
	gate.release()
	wantKey := "session:context_cut:" + fx.BoundaryUUID
	fake.awaitEntry(ctx, t, "the coalesced context cut", func(e *storev1.StoreEntry) bool {
		return e.GetUpsertKey() == wantKey
	})
	awaitCursorPast(ctx, t, fake, fx.File.Path(), fx.BoundaryOffset)

	// Assert.
	ids := map[string]bool{}
	for _, e := range fake.Entries() {
		if e.GetUpsertKey() == wantKey {
			ids[e.GetWriteId()] = true
		}
	}
	if len(ids) != 1 {
		t.Errorf("the held record minted %d distinct write_ids across its deliveries: %v; a re-read must be deterministic",
			len(ids), sortedStrings(keysOf(ids)))
	}
}
