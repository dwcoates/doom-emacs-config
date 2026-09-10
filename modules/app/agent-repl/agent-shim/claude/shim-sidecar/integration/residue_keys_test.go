package integration

import (
	"fmt"
	"testing"
)

// SUBJECT — the two RESIDUE KEY spellings, as literals on the wire.
//
// `residue:<vendor record uuid>` is what makes the two planes COLLAPSE: the
// shim and the sidecar see the same vendor record and either may store it, and
// keyed by the vendor's own uuid both writes land on ONE row. A record with no
// uuid — an unparsed line — has nothing the other plane could agree on, so it is
// keyed by where it lives: `residue:file:<path>:<offset>`, a visibly separate
// space no path can collide with a uuid in.
//
// THE LITERALS ARE THE SUBJECT. A digest, a rename, or a path spelled
// differently from the reader's own normalized one all pass every structural
// assertion and silently break cross-plane collapse, so both keys are compared
// as strings.

// TestAWithheldRecordIsKeyedByTheVendorsOwnUuid drives a real withheld class —
// `system/local_command`, which the converter understands and deliberately does
// not carry — and asserts the key is the record's uuid.
func TestAWithheldRecordIsKeyedByTheVendorsOwnUuid(t *testing.T) {
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

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	g := newGrowingFile(t, tree.sessionPath(slug, session))
	g.AppendLine(encodeRecord(t, retargetSession(t, decodeRecord(t, captured.Lines[7]), session, cwd)))
	g.AppendLine(encodeRecord(t, rec))
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())

	// Assert.
	wantKey := "residue:" + uuid
	e := entryByUpsertKey(fake.Entries(), wantKey)
	if e == nil {
		t.Fatalf("no entry was keyed %q; keys were %v", wantKey, upsertKeysOf(fake.Entries()))
	}
	if got := e.GetAgentUpdate().GetUnservedItem().GetVendorSpecific().GetKind(); got != "system/local_command" {
		t.Errorf("the withheld record landed as kind %q, wanted %q", got, "system/local_command")
	}
	if e.GetAgentUpdate().GetServeableFrame() != nil {
		t.Errorf("a withheld record reached a page: %v", e.GetAgentUpdate().GetServeableFrame())
	}
}

// TestAnUnparsableLineIsKeyedByItsFileCoordinates asserts the fallback: a line
// that cannot be READ has no uuid, so its key names the file and the offset the
// bytes start at — in the reader's own discovery-normalized spelling of the path.
func TestAnUnparsableLineIsKeyedByItsFileCoordinates(t *testing.T) {
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

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	g := newGrowingFile(t, tree.sessionPath(slug, session))
	g.AppendLine(encodeRecord(t, retargetSession(t, decodeRecord(t, captured.Lines[7]), session, cwd)))
	offset := g.AppendLine(`{"type":"assistant","uuid":"` + session + `",`)
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())

	// Assert.
	wantKey := fmt.Sprintf("residue:file:%s:%d", resolved(g.Path()), offset)
	e := entryByUpsertKey(fake.Entries(), wantKey)
	if e == nil {
		t.Fatalf("no entry was keyed %q; keys were %v", wantKey, upsertKeysOf(fake.Entries()))
	}
	unparsed := e.GetAgentUpdate().GetUnservedItem().GetUnparsed()
	if unparsed == nil {
		t.Fatalf("the entry under %q is not on the unparsed arm: %v", wantKey, e.GetAgentUpdate())
	}
	if unparsed.GetOffset() != uint64(offset) {
		t.Errorf("the unparsed record states offset %d, wanted %d", unparsed.GetOffset(), offset)
	}
	if !samePath(unparsed.GetSource(), g.Path()) {
		t.Errorf("the unparsed record names source %q, wanted %q", unparsed.GetSource(), g.Path())
	}
	if unparsed.GetParseError() == "" {
		t.Errorf("the unparsed record states no parse error, so nothing can be investigated")
	}
}

// TestTheTwoResidueSpacesNeverCollide asserts the shape rule the fallback exists
// for: a file-keyed residue is always in its own `residue:file:` space, so no
// path can ever be mistaken for a vendor uuid.
func TestTheTwoResidueSpacesNeverCollide(t *testing.T) {
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
	uuid, _ := withheld["uuid"].(string)

	// Act: both kinds of residue in one file.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	g := newGrowingFile(t, tree.sessionPath(slug, session))
	g.AppendLine(encodeRecord(t, retargetSession(t, decodeRecord(t, captured.Lines[7]), session, cwd)))
	g.AppendLine(encodeRecord(t, withheld))
	offset := g.AppendLine(`{"type":"assistant"`)
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())

	// Assert.
	uuidKey := "residue:" + uuid
	fileKey := fmt.Sprintf("residue:file:%s:%d", resolved(g.Path()), offset)
	for _, want := range []string{uuidKey, fileKey} {
		if entryByUpsertKey(fake.Entries(), want) == nil {
			t.Fatalf("no entry was keyed %q; keys were %v", want, upsertKeysOf(fake.Entries()))
		}
	}
	if uuidKey == fileKey {
		t.Fatalf("the two residue spaces produced one key %q", uuidKey)
	}
}
