package integration

import (
	"testing"

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
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	awaitCursorInBatches(ctx, t, fake, fx.File.Path(), 1)

	// Assert.
	cs := latestCursorFor(fake.Batches(), fx.File.Path())
	if cs == nil {
		t.Fatalf("the sidecar offered no cursor for %s", fx.File.Path())
	}
	if cs.GetOffset() > fx.BoundaryOffset {
		t.Errorf("the cursor advanced to %d, past the held boundary at %d; a hold parks the cursor BEFORE the held frame",
			cs.GetOffset(), fx.BoundaryOffset)
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
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	awaitCursorInBatches(ctx, t, fake, fx.File.Path(), 1)

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
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	awaitCursorInBatches(ctx, t, fake, fx.File.Path(), 1)
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
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
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
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	awaitCursorInBatches(ctx, t, fake, fx.File.Path(), 1)
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
