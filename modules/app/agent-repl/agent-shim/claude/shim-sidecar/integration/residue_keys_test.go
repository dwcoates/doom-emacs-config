package integration

import (
	"strings"
	"testing"
)

// SUBJECT — RESIDUE IS CLASSIFIED AND NEVER PERSISTED (owner ruling 2026-09-13).
//
// These subjects once pinned the two RESIDUE KEY spellings, because a stored
// residue row was the evidence a line had been read. THE ROW IS GONE: every
// residue arm — `vendor_specific` of any kind, `unknown`, and `unparsed` — is
// still framed and classified exactly as before and is then withheld at the
// sidecar's single write path, so there is no upsert_key left to compare.
//
// WHAT SURVIVES IS THE CLASSIFICATION, and that is what these subjects pin now.
// The reader still says, per record, WHICH residue it read — the arm plus the
// arm's own discriminator — and the store still holds none of it. A classifier
// that quietly stopped naming what the vendor recorded, or a write path that let
// one arm slip through, still fails here; only the key spelling is unobservable.
//
// THE RECORDS ARE VERBOSE, so every subject here runs the sidecar through
// `debugLogging`: without it the reader's own account of what it withheld is
// never emitted and the subject would assert nothing.

// TestAWithheldRecordIsClassifiedByItsVendorKind drives a real withheld class —
// `system/local_command`, which the converter understands and deliberately does
// not carry.
//
// RE-AIMED: it asserted the row was keyed by the vendor's own uuid, which was
// the cross-plane collapse rule. No row is written, so the uuid key is not
// observable from here; what remains worth pinning is that the record is read,
// named as the vendor's own kind, and stored nowhere.
func TestAWithheldRecordIsClassifiedByItsVendorKind(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	cwd := "/Users/dodgecoates/residue-uuid-probe"
	slug := cwdSlug(cwd)
	session := "8a8a8a8a-8a8a-48a8-88a8-8a8a8a8a8a8a"
	rec := retargetSession(t,
		decodeRecord(t, corpusLine(t, "transcript-lines/system-local_command.jsonl", 0)), session, cwd)
	uuid, _ := rec["uuid"].(string)
	if uuid == "" {
		t.Fatalf("the system/local_command fixture carries no uuid")
	}
	opts := debugLogging(defaultSidecarOptions(t, fake.Socket, tree))

	// Act.
	startSidecar(t, opts)
	g := newGrowingFile(t, tree.sessionPath(slug, session))
	g.AppendLine(encodeRecord(t, retargetSession(t, decodeRecord(t, captured.Lines[7]), session, cwd)))
	g.AppendLine(encodeRecord(t, rec))
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())

	// Assert: the reader named the vendor's own kind, and stored nothing for it.
	awaitResidueWithheld(ctx, t, opts.LogPath, "vendor_specific/system/local_command")
	requireNoResidueStored(t, fake.Entries())
	// The record reached no row AT ALL — not a page line and not an unserved
	// one — so nothing anywhere in the store names it.
	for _, e := range fake.Entries() {
		if strings.Contains(e.GetUpsertKey(), uuid) {
			t.Errorf("the withheld record produced a row keyed %q", e.GetUpsertKey())
		}
	}
}

// TestAnUnparsableLineIsClassifiedAgainstItsFile asserts the fallback: a line
// that cannot be READ still produces an account of itself, and that account
// names the file whose bytes it was.
//
// RE-AIMED: it asserted the row's key was `residue:file:<path>:<offset>` and
// that the row restated its source, offset and parse error. No row is written,
// so the file COORDINATES survive only as the withheld record's `file_id`; the
// byte offset and the parse error are no longer observable from a test.
func TestAnUnparsableLineIsClassifiedAgainstItsFile(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	cwd := "/Users/dodgecoates/residue-file-probe"
	slug := cwdSlug(cwd)
	session := "8b8b8b8b-8b8b-48b8-88b8-8b8b8b8b8b8b"
	opts := debugLogging(defaultSidecarOptions(t, fake.Socket, tree))

	// Act.
	startSidecar(t, opts)
	g := newGrowingFile(t, tree.sessionPath(slug, session))
	g.AppendLine(encodeRecord(t, retargetSession(t, decodeRecord(t, captured.Lines[7]), session, cwd)))
	g.AppendLine(`{"type":"assistant","uuid":"` + session + `",`)
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())

	// Assert: the unreadable bytes were classified against their own file, and
	// the reader still advanced past them rather than stalling on a line it
	// could store nothing for.
	rec := awaitResidueWithheld(ctx, t, opts.LogPath, "unparsed")
	if got, want := rec.Context["file_id"], fileID(t, g.Path()); got != want {
		t.Errorf("the withheld unparsed record names file_id %v, wanted the file it was read from %q", got, want)
	}
	requireNoResidueStored(t, fake.Entries())
}

// TestTheTwoResiduePopulationsStayDistinct asserts what the two key spaces
// existed for: a uuid-bearing residue and an unreadable line are separately
// attributable, so a tally says WHICH vendor behavior produced the volume.
//
// RE-AIMED: it asserted the `residue:` and `residue:file:` key spaces could
// never collide. With no rows there are no keys, and the distinction now lives
// in the labels the classifier mints — which is the property the separate key
// space was protecting.
func TestTheTwoResiduePopulationsStayDistinct(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	cwd := "/Users/dodgecoates/residue-spaces-probe"
	slug := cwdSlug(cwd)
	session := "8c8c8c8c-8c8c-48c8-88c8-8c8c8c8c8c8c"
	withheld := retargetSession(t,
		decodeRecord(t, corpusLine(t, "transcript-lines/system-local_command.jsonl", 0)), session, cwd)
	opts := debugLogging(defaultSidecarOptions(t, fake.Socket, tree))

	// Act: both kinds of residue in one file.
	startSidecar(t, opts)
	g := newGrowingFile(t, tree.sessionPath(slug, session))
	g.AppendLine(encodeRecord(t, retargetSession(t, decodeRecord(t, captured.Lines[7]), session, cwd)))
	g.AppendLine(encodeRecord(t, withheld))
	g.AppendLine(`{"type":"assistant"`)
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())

	// Assert.
	for _, label := range []string{"vendor_specific/system/local_command", "unparsed"} {
		awaitResidueWithheld(ctx, t, opts.LogPath, label)
	}
	requireNoResidueStored(t, fake.Entries())
}

// TestWithheldResidueAnnouncesNoRow asserts residue attributes itself to
// nothing at all: an unclassifiable record may belong to nobody, so it names no
// agent — and since it is never persisted, it names no ROW either. The reader's
// withholding record therefore carries no upsert_key, because a record naming a
// key nobody can look up is an untraceable announcement.
func TestWithheldResidueAnnouncesNoRow(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	cwd := "/Users/dodgecoates/residue-toplevel-probe"
	slug := cwdSlug(cwd)
	session := "70707070-7070-4070-8070-70707070aaaa"
	opts := debugLogging(defaultSidecarOptions(t, fake.Socket, tree))

	// Act.
	startSidecar(t, opts)
	g := newGrowingFile(t, tree.sessionPath(slug, session))
	g.AppendLine(encodeRecord(t, retargetSession(t, decodeRecord(t, captured.Lines[7]), session, cwd)))
	g.AppendLine(`{"type":"assistant","uuid":"` + session + `",`)
	rec := awaitResidueWithheld(ctx, t, opts.LogPath, "unparsed")

	// Assert.
	if key, ok := rec.Context["upsert_key"]; ok && key != "" {
		t.Errorf("the withholding record names upsert_key %v; it announces no row, and a key nobody can look up is untraceable", key)
	}
	requireNoResidueStored(t, fake.Entries())
}
