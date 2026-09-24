package integration

import (
	"testing"
)

// SUBJECT — WHERE the keep-alive marker sits in a prompt.
//
// The marker is a PREFIX rule, and the reason it has to be is that the marker's
// own text is ordinary prose: a person quoting it while asking about keep-alive
// prompts must not have their turn silently withheld from their own feed. The
// two placements are therefore the two edges of one rule, and they must be
// driven separately — a subject that only proves the opening marker withholds a
// turn is equally satisfied by a reader matching the marker ANYWHERE.

// TestAKeepAliveMarkerOpeningThePromptWithholdsTheWholeTurn asserts the marker
// at position 0 of the first text block classifies the turn, so none of its
// records is stored.
func TestAKeepAliveMarkerOpeningThePromptWithholdsTheWholeTurn(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	cwd := "/Users/dodgecoates/keepalive-prefix-probe"
	slug := cwdSlug(cwd)
	session := "7a7a7a7a-7a7a-47a7-87a7-7a7a7a7a7a7a"

	// Act: the captured prompt, with the marker put in FRONT of its text.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	g := newGrowingFile(t, tree.sessionPath(slug, session))
	appendRecords(t, g, markedTurn(t, captured, session, cwd, keepaliveMarker+" hold the session open")...)
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())

	// Assert.
	if entries := fake.Entries(); len(entries) != 0 {
		t.Fatalf("a keep-alive turn stored %d entrie(s) (keys %v), want none", len(entries), upsertKeysOf(entries))
	}
}

// TestAKeepAliveMarkerQuotedMidPromptIsServedNormally asserts the other edge:
// the same bytes anywhere but position 0 are ORDINARY PROSE, and the turn is
// served like any other.
//
// A PERSON ASKING ABOUT KEEP-ALIVE PROMPTS IS THE CASE THIS PROTECTS. Withholding
// their turn would erase their own question and every answer to it from their
// feed, with no error anywhere to explain where it went.
func TestAKeepAliveMarkerQuotedMidPromptIsServedNormally(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	cwd := "/Users/dodgecoates/keepalive-quoted-probe"
	slug := cwdSlug(cwd)
	session := "7b7b7b7b-7b7b-47b7-87b7-7b7b7b7b7b7b"

	// Act: the marker QUOTED inside the prompt rather than opening it.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	g := newGrowingFile(t, tree.sessionPath(slug, session))
	appendRecords(t, g, markedTurn(t, captured, session, cwd,
		"what does the "+keepaliveMarker+" marker actually do?")...)
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())

	// Assert: the turn's units reached the main agent's book.
	entries := fake.Entries()
	if len(linesForBook(entries, session)) == 0 {
		t.Fatalf("a turn whose prompt merely QUOTES the marker produced no page line at all; its records were %v",
			upsertKeysOf(entries))
	}
}

// markedTurn re-points the captured session's REAL user prompt at this test's
// session, replaces its first text block, and chains the captured response's
// thinking line and Bash call after it, so the placement is the only thing that
// differs between the two subjects above.
func markedTurn(t *testing.T, captured capturedSession, session, cwd, text string) []map[string]any {
	t.Helper()
	return chained(t,
		setUserText(t, retargetSession(t, decodeRecord(t, captured.Lines[3]), session, cwd), text),
		retargetSession(t, decodeRecord(t, captured.Lines[7]), session, cwd),
		retargetSession(t, decodeRecord(t, captured.Lines[8]), session, cwd),
	)
}
