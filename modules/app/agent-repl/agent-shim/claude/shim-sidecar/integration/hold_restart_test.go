package integration

import (
	"path/filepath"
	"testing"

	storev1 "agentrepl/proto/store/v1"
)

// CRITIQUE 18 — the hold ACROSS a restart, and the keep-alive bit across a
// compaction.
//
// The hold's whole point is that the cursor advances SHORT of what was read, so
// the held frame is read again — by the next scan, and equally by a RESTART. The
// existing hold subjects all stay inside one process, which leaves the restart
// half of that sentence untested even though it is the half that costs a record
// if it is wrong: a cursor parked short and a boot that skipped the frame anyway
// is a compaction boundary nobody ever sees.
//
// The keep-alive bit is the other in-memory state a compaction sits in the middle
// of. It is ONE REMEMBERED BOOL cleared only by the next non-keepalive user
// prompt — and a compaction boundary is not one — so a keep-alive turn that spans
// a compaction keeps withholding everything, the cut included.

// TestAHeldBoundaryIsConvertedOnceAfterARestart stops the sidecar while the
// boundary is held, writes the summary that settles it, and asserts the restarted
// process converts it — exactly once, with the summary.
func TestAHeldBoundaryIsConvertedOnceAfterARestart(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	store := startRealStore(t)
	// THE HOLD IS BOUNDED TO ONE REDELIVERY, and the redelivery is the NEXT
	// poll of this file — so the subject has to cut the reader off between the
	// two, and at the production poll interval that is one 50ms window. It used
	// to be arranged by stretching the interval to 5s, which made this the
	// slowest subject in the package and still only meant the test USUALLY won
	// the race.
	//
	// The store is the interlock the system already has: the batch that parks
	// the cursor is written synchronously inside the cycle, and the cycle does
	// not move on until the store answers. A recording proxy in front of the
	// real store COMMITS that batch upstream and then withholds the answer, so
	// the reader is frozen with the boundary in hand, the cursor durably parked
	// short of it, and no next poll possible — for as long as this subject
	// needs, at production's own interval.
	proxy := startProxyStore(t, store.Socket)
	tree := newVendorTree(t)
	fx := seedCompactionBoundary(t, tree, "/Users/dodgecoates/hold-restart-probe",
		"f1f1f1f1-f1f1-4f1f-8f1f-f1f1f1f1f1f1")
	opts := defaultSidecarOptions(t, proxy.Socket, tree)
	summaryText := "Previously: the reader was stopped with a boundary in hand."

	// Act: stop with the boundary held and the cursor parked short of it.
	gate := proxy.gateOnBatch(t, cursorParkedAt(fx.File.Path(), fx.BoundaryOffset))
	first := startSidecar(t, opts)
	gate.await(ctx, t, "the batch parking the cursor at the held boundary")
	awaitBookLines(ctx, t, store.Client, fx.Session, 1)
	cs := awaitCursorAtLeast(ctx, t, store.Client, fx.File.Path(), 1)
	if cs.GetOffset() > fx.BoundaryOffset {
		t.Fatalf("the cursor stood at %d, past the held boundary at %d, before the restart even happened",
			cs.GetOffset(), fx.BoundaryOffset)
	}
	// KILLED WHERE IT STANDS, not asked to leave: a process frozen inside a
	// withheld write does not see SIGTERM until its own rpc timeout expires,
	// and letting the write finish first would hand it the very poll this
	// subject exists to cut it off before.
	first.Kill()
	gate.release()

	fx.File.AppendLine(compactSummaryLine(t, fx.Session, fx.Cwd,
		"f1f1f1f1-f1f1-4f1f-8f1f-f1f1f1f1abcd", fx.BoundaryUUID, summaryText))
	restarted := opts
	restarted.LogPath = filepath.Join(t.TempDir(), "sidecar-restarted.log")
	startSidecar(t, restarted)
	// The restarted reader's OWN statement that it converted the frame it was
	// stopped holding: the cursor only moves past the boundary once the frame
	// parked short of it has been written. A wait for "at least two lines"
	// would be satisfied by any second line at all.
	awaitCursorAtLeast(ctx, t, store.Client, fx.File.Path(), fx.BoundaryOffset+1)
	lines := bookLines(ctx, t, store.Client, fx.Session, 500)

	// Assert: exactly one context cut, carrying the summary that settled it.
	var cuts []*storev1.StorePageLine
	for _, at := range lines {
		if contextCutOf(at.GetLine()) != nil {
			cuts = append(cuts, at.GetLine())
		}
	}
	if len(cuts) != 1 {
		t.Fatalf("the book holds %d context cuts after the restart, wanted exactly one", len(cuts))
	}
	cut := contextCutOf(cuts[0])
	if cut.GetCompacted() == nil {
		t.Fatalf("the cut is not on the compacted arm: %v", cut.GetCut())
	}
	if got := cut.GetCompacted().GetSummary().GetMarkdown(); got != summaryText {
		t.Errorf("the cut carries summary %q, wanted the summary written after the restart %q", got, summaryText)
	}
}

// TestAKeepAliveTurnSpanningACompactionStillWithholdsTheCut asserts the cut
// itself is withheld while the bit is set: a compaction inside a keep-alive turn
// is not a user-visible separation, because the turn was never the user's.
func TestAKeepAliveTurnSpanningACompactionStillWithholdsTheCut(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	cwd := "/Users/dodgecoates/keepalive-compaction-probe"
	slug := cwdSlug(cwd)
	session := "f2f2f2f2-f2f2-4f2f-8f2f-f2f2f2f2f2f2"

	prompt := setUserText(t,
		retargetSession(t, decodeRecord(t, captured.Lines[3]), session, cwd),
		keepaliveMarker+"cache ping")
	boundary := retargetSession(t, decodeRecord(t, corpusLine(t, "transcript-lines/system-compact_boundary.jsonl", 0)), session, cwd)
	boundaryUUID, _ := boundary["uuid"].(string)
	if boundaryUUID == "" {
		t.Fatalf("the compact_boundary fixture carries no uuid")
	}

	// Act: the marked prompt, then a compaction with its summary, inside one turn.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	g := newGrowingFile(t, tree.sessionPath(slug, session))
	g.AppendLine(encodeRecord(t, prompt))
	g.AppendLine(encodeRecord(t, boundary))
	g.AppendLine(compactSummaryLine(t, session, cwd,
		"f2f2f2f2-f2f2-4f2f-8f2f-f2f2f2f2abcd", boundaryUUID, "Previously: nothing served."))
	wantKey := "session:context_cut:" + boundaryUUID
	fake.awaitEntry(ctx, t, "the coalesced context cut", func(e *storev1.StoreEntry) bool {
		return e.GetUpsertKey() == wantKey
	})

	// Assert: the cut landed on the keepalive arm, never as a page line.
	e := entryByUpsertKey(fake.Entries(), wantKey)
	if e.GetAgentUpdate().GetServeableFrame() != nil {
		t.Errorf("the context cut of a keep-alive turn reached a page line of book %q",
			e.GetAgentUpdate().GetServeableFrame().GetPageAgentId().GetValue())
	}
	if e.GetAgentUpdate().GetUnservedItem().GetKeepalive() == nil {
		t.Errorf("the context cut did not land on the keepalive arm: %v", e.GetAgentUpdate())
	}
}

// TestAKeepAliveBitSurvivesACompactionForTheWorkAfterIt asserts the bit is not
// cleared by the compaction: assistant work written AFTER the cut is still
// withheld, because only a non-keepalive user prompt closes the turn.
func TestAKeepAliveBitSurvivesACompactionForTheWorkAfterIt(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	cwd := "/Users/dodgecoates/keepalive-after-compaction-probe"
	slug := cwdSlug(cwd)
	session := "f3f3f3f3-f3f3-4f3f-8f3f-f3f3f3f3f3f3"

	prompt := setUserText(t,
		retargetSession(t, decodeRecord(t, captured.Lines[3]), session, cwd),
		keepaliveMarker+"cache ping")
	boundary := retargetSession(t, decodeRecord(t, corpusLine(t, "transcript-lines/system-compact_boundary.jsonl", 0)), session, cwd)
	boundaryUUID, _ := boundary["uuid"].(string)
	thinking := retargetSession(t, decodeRecord(t, captured.Lines[12]), session, cwd)
	call := retargetSession(t, decodeRecord(t, captured.Lines[13]), session, cwd)

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	g := newGrowingFile(t, tree.sessionPath(slug, session))
	g.AppendLine(encodeRecord(t, prompt))
	g.AppendLine(encodeRecord(t, boundary))
	g.AppendLine(compactSummaryLine(t, session, cwd,
		"f3f3f3f3-f3f3-4f3f-8f3f-f3f3f3f3abcd", boundaryUUID, "Previously: nothing served."))
	g.AppendLine(encodeRecord(t, thinking))
	g.AppendLine(encodeRecord(t, call))
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())

	// Assert.
	if len(keepalivesOf(fake.Entries())) == 0 {
		t.Fatalf("nothing landed on the keepalive arm at all, so the bit was never set")
	}
	for _, line := range linesForBook(fake.Entries(), session) {
		if a := activityOf(line); a != nil {
			t.Errorf("unit %q reached a page line after a compaction inside a keep-alive turn; only a non-keepalive user prompt closes it",
				a.GetActivityId().GetValue())
		}
	}
}
