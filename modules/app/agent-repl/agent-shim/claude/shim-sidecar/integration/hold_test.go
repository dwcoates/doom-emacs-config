package integration

import (
	"strings"
	"testing"

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
	fx := seedCompactionBoundary(t, tree, "/work/hold-bounded-probe",
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
