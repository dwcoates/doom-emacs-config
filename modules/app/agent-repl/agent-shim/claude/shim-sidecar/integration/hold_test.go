package integration

import (
	"context"
	"testing"
	"time"

	storev1 "agentrepl/proto/store/v1"
)

// SUBJECT 8 — THE HOLD, the one legitimate deferral.
//
// A compaction boundary's meaning depends on the line that follows it: the
// summary that replaced the history. The boundary is written ~1ms before its
// summary, so a poll lands between them. Rather than convert on incomplete
// evidence, the handler HOLDS the trailing frame and the cursor advances SHORT
// of what was read — to the held frame's offset — so the next scan, and a
// restart, both read it again. The hold is bounded to ONE redelivery.

// holdOptions gives a hold subject a poll interval it can act INSIDE.
//
// THE HOLD IS BOUNDED TO ONE REDELIVERY, so a subject that must append the
// summary between the first delivery and the second is racing the poll tick. At
// the suite's ordinary 50ms that race is real: the forced redelivery can land
// before the append does, and the subject then asserts the coalescing path
// against a run that took the bounded-expiry path. A longer tick removes the
// race outright, and the subjects still WAIT on the parked cursor rather than on
// the tick — nothing here is timed, only unhurried.
func holdOptions(t *testing.T, storeSocket string, tree *vendorTree) sidecarOptions {
	t.Helper()
	opts := defaultSidecarOptions(t, storeSocket, tree)
	opts.PollInterval = 500 * time.Millisecond
	return opts
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
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	fx := seedCompactionBoundary(t, tree, "/Users/dodgecoates/hold-cursor-probe",
		"70707070-7070-4070-8070-707070707070")

	// Act.
	startSidecar(t, holdOptions(t, fake.Socket, tree))
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
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	fx := seedCompactionBoundary(t, tree, "/Users/dodgecoates/hold-nothing-probe",
		"80808080-8080-4080-8080-808080808080")

	// Act.
	startSidecar(t, holdOptions(t, fake.Socket, tree))
	awaitParkedCursor(ctx, t, fake, fx.File.Path(), fx.BoundaryOffset)

	// Assert.
	wantKey := "session:context_cut:" + fx.BoundaryUUID
	if entryByUpsertKey(fake.Entries(), wantKey) != nil {
		t.Errorf("the boundary was converted while its summary was still absent (key %q)", wantKey)
	}
}

// TestTheSummaryCoalescesWithItsBoundaryIntoOneRecord asserts the boundary and
// the summary become ONE ContextCut page line of the main agent's book.
func TestTheSummaryCoalescesWithItsBoundaryIntoOneRecord(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	fx := seedCompactionBoundary(t, tree, "/Users/dodgecoates/hold-coalesce-probe",
		"90909090-9090-4090-8090-909090909090")
	summaryText := "Previously: the harness held a background probe alive."

	// Act.
	startSidecar(t, holdOptions(t, fake.Socket, tree))
	// The summary is appended once the cursor is PARKED, which is the event that
	// says the boundary was held and its forced redelivery has not run yet.
	awaitParkedCursor(ctx, t, fake, fx.File.Path(), fx.BoundaryOffset)
	fx.File.AppendLine(compactSummaryLine(t, fx.Session, fx.Cwd,
		"90909090-9090-4090-8090-90909090abcd", fx.BoundaryUUID, summaryText))

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
	// GIVING UP ON THE SUMMARY IS STATED, and stated as a degradation: the cut a
	// reader gets from here is missing prose the vendor may yet have written, so
	// the bound expiring is a warning rather than a routine info beat.
	if got := recordsAt(records, "hold", "warn"); len(got) != 1 {
		t.Errorf("the summary-less conversion was stated %d time(s) at warn, want exactly one; the log held %v",
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
	// The summary's PROSE is what must be absent. The message itself is always
	// present (internal/convert/contextcut.go builds it unconditionally, so the
	// field's absence is not expressible), and the empty markdown IS the "no
	// summary" statement: a reader renders a hole where the discarded history
	// was, rather than prose the vendor never wrote.
	if got := cut.GetCompacted().GetSummary().GetMarkdown(); got != "" {
		t.Errorf("the cut carries summary prose %q; no summary line was ever written, and one must not be invented", got)
	}
}

// TestABoundaryHeldOnceIsNotWrittenTwice asserts the redelivered record is
// converted ONCE — the write_id absorbs a repeat rather than doubling the row.
func TestABoundaryHeldOnceIsNotWrittenTwice(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	fx := seedCompactionBoundary(t, tree, "/Users/dodgecoates/hold-once-probe",
		"b0b0b0b0-b0b0-40b0-80b0-b0b0b0b0b0b0")
	summaryText := "Previously: nothing much."

	// Act.
	startSidecar(t, holdOptions(t, fake.Socket, tree))
	awaitParkedCursor(ctx, t, fake, fx.File.Path(), fx.BoundaryOffset)
	fx.File.AppendLine(compactSummaryLine(t, fx.Session, fx.Cwd,
		"b0b0b0b0-b0b0-40b0-80b0-b0b0b0b0abcd", fx.BoundaryUUID, summaryText))
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
