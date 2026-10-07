package integration

import (
	"testing"

	storev1 "agentrepl/proto/store/v1"
)

// SUBJECT — the TRANSCRIPT carry, which is the spool carry's harder twin.
//
// A vendor record is one JSONL line, and a poll can land anywhere inside it. The
// bounded partial-line carry (`CursorState.carry`) is what makes a cut line a
// non-event: the incomplete tail is remembered, the cursor still advances over
// the bytes that were read, and the line parses ONCE and WHOLE when its last
// byte arrives — however many polls it took. The carry rides the cursor into the
// store, so a sidecar that boots onto a half-written line resumes mid-line
// rather than re-reading or losing it.

// splitInThree cuts a line into three non-empty pieces, so the carry is
// exercised across TWO poll boundaries rather than one.
func splitInThree(t *testing.T, line string) (string, string, string) {
	t.Helper()
	if len(line) < 6 {
		t.Fatalf("line %q is too short to cut in three", line)
	}
	a := len(line) / 3
	b := 2 * len(line) / 3
	return line[:a], line[a:b], line[b:]
}

// TestATranscriptLineSplitAcrossThreePollsConvertsOnceAndWhole grows one vendor
// record in three pieces, waiting for the reader to observe each, and asserts
// the record produced exactly one entry and no residue.
func TestATranscriptLineSplitAcrossThreePollsConvertsOnceAndWhole(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	cwd := "/work/transcript-carry-probe"
	slug := cwdSlug(cwd)
	session := "0a0a0a0a-0a0a-40a0-80a0-0a0a0a0a0a0a"
	line := encodeRecord(t, retargetSession(t, decodeRecord(t, captured.Lines[7]), session, cwd)) + "\n"
	one, two, three := splitInThree(t, line)

	// Act: three appends, each observed before the next is written.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	g := newGrowingFile(t, tree.sessionPath(slug, session))
	g.AppendRaw([]byte(one))
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())
	g.AppendRaw([]byte(two))
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())
	g.AppendRaw([]byte(three))
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())

	// Assert.
	entries := fake.Entries()
	if n := len(unitEntries(entries, capturedThinking1)); n != 1 {
		t.Fatalf("the split record produced %d entries for unit %q, wanted exactly one; keys were %v",
			n, capturedThinking1, upsertKeysOf(entries))
	}
	if got := unparsedOf(entries); len(got) != 0 {
		t.Errorf("a line cut across polls landed as unparsed residue: %v", got)
	}
}

// TestATranscriptLineSplitAcrossThreePollsIsNeverPartiallyConverted asserts the
// other half: no entry is produced from a PREFIX of the record. The first two
// appends move the cursor and write nothing at all.
func TestATranscriptLineSplitAcrossThreePollsIsNeverPartiallyConverted(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	cwd := "/work/transcript-carry-partial-probe"
	slug := cwdSlug(cwd)
	session := "0b0b0b0b-0b0b-40b0-80b0-0b0b0b0b0b0b"
	line := encodeRecord(t, retargetSession(t, decodeRecord(t, captured.Lines[7]), session, cwd)) + "\n"
	one, two, three := splitInThree(t, line)

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	g := newGrowingFile(t, tree.sessionPath(slug, session))
	g.AppendRaw([]byte(one))
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())
	g.AppendRaw([]byte(two))
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())
	midway := fake.Entries()
	g.AppendRaw([]byte(three))
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())

	// Assert.
	if len(midway) != 0 {
		t.Errorf("two thirds of a record produced %d entries; a partial line must convert to nothing: %v",
			len(midway), upsertKeysOf(midway))
	}
	if n := len(unitEntries(fake.Entries(), capturedThinking1)); n != 1 {
		t.Errorf("after the last byte arrived the record produced %d entries, wanted one", n)
	}
}

// TestASeededCarryIsResumedFromTheStoreOnBoot asserts the carry is DURABLE: a
// sidecar handed a cursor whose carry holds the head of a line reads only the
// remaining bytes and still converts that line whole.
//
// The file deliberately holds NO turn start, so the boot rewind leaves the
// store's cursor — and its carry — exactly where the store put it. That is the
// only state in which a seeded carry is observable at all.
func TestASeededCarryIsResumedFromTheStoreOnBoot(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	cwd := "/work/transcript-carry-boot-probe"
	slug := cwdSlug(cwd)
	session := "0c0c0c0c-0c0c-40c0-80c0-0c0c0c0c0c0c"
	line := encodeRecord(t, retargetSession(t, decodeRecord(t, captured.Lines[7]), session, cwd)) + "\n"
	head := line[:len(line)/2]

	// The whole line is already on disk; the store's cursor says the first half
	// was read and carried.
	g := newGrowingFile(t, tree.sessionPath(slug, session))
	g.AppendRaw([]byte(line))
	fake.SeedCursors(&storev1.CursorState{
		FileId:     fileID(t, g.Path()),
		Path:       resolved(g.Path()),
		Offset:     int64(len(head)),
		Carry:      []byte(head),
		Conversion: currentConversion(),
	})

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())

	// Assert.
	entries := fake.Entries()
	if n := len(unitEntries(entries, capturedThinking1)); n != 1 {
		t.Fatalf("a boot onto a seeded carry produced %d entries for unit %q, wanted exactly one; keys were %v",
			n, capturedThinking1, upsertKeysOf(entries))
	}
	if got := unparsedOf(entries); len(got) != 0 {
		t.Errorf("the seeded carry was not prepended to the bytes read: %v", got)
	}
}
