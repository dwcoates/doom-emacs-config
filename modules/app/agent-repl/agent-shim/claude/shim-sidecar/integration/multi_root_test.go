package integration

import (
	"path/filepath"
	"testing"
)

// SUBJECT 10 — discovery spans BOTH config roots and nothing else.
//
// The second account's transcripts are invisible unless its config dir is a
// discovery root; a file in a directory that is neither root is not the
// sidecar's to read.

// TestBothConfigRootsAreDiscovered asserts a transcript under each root is
// ingested.
func TestBothConfigRootsAreDiscovered(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	primary := newVendorTree(t)
	secondary := newVendorTreeSharingSpool(t, primary.SpoolRoot)
	captured := loadCapturedSession(t)

	cwdA := "/work/root-a-probe"
	cwdB := "/work/root-b-probe"
	sessionA := "f0f0f0f0-f0f0-40f0-80f0-f0f0f0f0f0f0"
	sessionB := "f1f1f1f1-f1f1-41f1-81f1-f1f1f1f1f1f1"

	opts := defaultSidecarOptions(t, fake.Socket, primary)
	opts.ConfigRoots = []string{primary.Root, secondary.Root}

	// Act.
	startSidecar(t, opts)
	a := newGrowingFile(t, primary.sessionPath(cwdSlug(cwdA), sessionA))
	a.AppendLine(encodeRecord(t, retargetSession(t, decodeRecord(t, captured.Lines[7]), sessionA, cwdA)))
	b := newGrowingFile(t, secondary.sessionPath(cwdSlug(cwdB), sessionB))
	b.AppendLine(encodeRecord(t, retargetSession(t, decodeRecord(t, captured.Lines[7]), sessionB, cwdB)))
	awaitCursorInBatches(ctx, t, fake, a.Path(), a.Offset())
	awaitCursorInBatches(ctx, t, fake, b.Path(), b.Offset())

	// Assert.
	for _, session := range []string{sessionA, sessionB} {
		if len(linesForBook(fake.Entries(), session)) == 0 {
			t.Errorf("no page line was written for the book %q; both config roots are discovery roots", session)
		}
	}
}

// TestAFileOutsideEveryRootIsIgnored asserts discovery is bounded by the roots
// it was given.
func TestAFileOutsideEveryRootIsIgnored(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	outside := unownedDir(t)

	cwd := "/work/outside-root-probe"
	insideSession := "f2f2f2f2-f2f2-42f2-82f2-f2f2f2f2f2f2"
	outsideSession := "f3f3f3f3-f3f3-43f3-83f3-f3f3f3f3f3f3"

	// Act: a file under a root and an identical one outside every root.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	inside := newGrowingFile(t, tree.sessionPath(cwdSlug(cwd), insideSession))
	inside.AppendLine(encodeRecord(t, retargetSession(t, decodeRecord(t, captured.Lines[7]), insideSession, cwd)))

	stray := newGrowingFile(t, filepath.Join(outside, outsideSession+".jsonl"))
	stray.AppendLine(encodeRecord(t, retargetSession(t, decodeRecord(t, captured.Lines[7]), outsideSession, cwd)))
	awaitCursorInBatches(ctx, t, fake, inside.Path(), inside.Offset())

	// Assert.
	if latestCursorFor(fake.Batches(), stray.Path()) != nil {
		t.Errorf("a file under neither config root was read: %s", stray.Path())
	}
	if len(linesForBook(fake.Entries(), outsideSession)) != 0 {
		t.Errorf("a file under neither config root produced page lines for book %q", outsideSession)
	}
}
