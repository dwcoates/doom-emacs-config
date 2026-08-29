package integration

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	storev1 "agentrepl/proto/store/v1"
)

// SUBJECT 4 — who a spool belongs to, and what happens when nobody claims it.
//
// A spool that appears BEFORE the transcript line naming it is retained and
// re-checked, never dropped; an unclassifiable task-id prefix is a loud
// total-ingestion violation whose bytes still land as residue; and the /tmp
// versus /private/tmp spellings of one file are one file.

// TestASpoolSeenBeforeItsOwnerIsRetained asserts an unowned spool is kept and
// re-checked rather than discarded.
func TestASpoolSeenBeforeItsOwnerIsRetained(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	cwd := "/Users/dodgecoates/spool-orphan-probe"
	slug := cwdSlug(cwd)
	session := "10101010-1010-4010-8010-101010101010"
	spoolPath := tree.spoolPath(slug, session, capturedSpoolTask1)

	// Act: the spool exists first; its owner appears only afterwards.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	spool := newGrowingFile(t, spoolPath)
	spool.AppendRaw([]byte("output written before anyone claimed it\n"))
	awaitAnyCursorFor(ctx, t, fake, spoolPath)

	// Assert: the file is being read, and nothing said it was dropped.
	if latestCursorFor(fake.Batches(), spoolPath) == nil {
		t.Fatalf("an unowned spool must stay discovered and be re-checked, never dropped")
	}
}

// TestAnUnownedSpoolIsAttributedOnceItsOwnerAppears asserts the retained spool
// is attributed to the spawning call as soon as the transcript names it.
func TestAnUnownedSpoolIsAttributedOnceItsOwnerAppears(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	cwd := "/Users/dodgecoates/spool-late-owner-probe"
	slug := cwdSlug(cwd)
	session := "20202020-2020-4020-8020-202020202020"
	captured := loadCapturedSession(t)
	spoolPath := tree.spoolPath(slug, session, capturedSpoolTask1)

	call := retargetSession(t, decodeRecord(t, captured.Lines[8]), session, cwd)
	result := retargetSession(t, decodeRecord(t, captured.Lines[10]), session, cwd)
	result = setToolResultText(t, result, backgroundLaunchText(capturedSpoolTask1, spoolPath))
	result = setNested(t, result, "toolUseResult", "backgroundTaskId", capturedSpoolTask1)

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	spool := newGrowingFile(t, spoolPath)
	spool.AppendRaw([]byte("orphaned output\n"))
	awaitAnyCursorFor(ctx, t, fake, spoolPath)

	parent := newGrowingFile(t, tree.sessionPath(slug, session))
	parent.AppendLine(encodeRecord(t, call))
	parent.AppendLine(encodeRecord(t, result))
	fake.awaitEntry(ctx, t, "a bash frame for the now-resolved owner", func(e *storev1.StoreEntry) bool {
		return e.GetAgentUpdate().GetBash().GetRun().GetValue() == capturedBashCall1
	})

	// Assert.
	frames := bashFramesForRun(fake.Entries(), capturedBashCall1)
	if len(frames) == 0 {
		t.Fatalf("the spool was never attributed to its owner; runs seen: %v", runsSeen(fake.Entries()))
	}
}

// TestAnUnclassifiableSpoolPrefixIsRefusedLoudly asserts a task id with no
// known kind prefix is an ERROR — a total-ingestion violation stated out loud.
func TestAnUnclassifiableSpoolPrefixIsRefusedLoudly(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	cwd := "/Users/dodgecoates/spool-prefix-probe"
	slug := cwdSlug(cwd)
	session := "30303030-3030-4030-8030-303030303030"
	opts := defaultSidecarOptions(t, fake.Socket, tree)
	// z* is none of b* (shell), a* (agent transcript) or w* (workflow journal).
	spoolPath := tree.spoolPath(slug, session, "z0uncla551f1able")

	// Act.
	startSidecar(t, opts)
	spool := newGrowingFile(t, spoolPath)
	spool.AppendRaw([]byte("bytes nobody can classify\n"))

	// Assert.
	awaitLog(ctx, t, opts.LogPath, "the unclassifiable-prefix refusal", func(r logRecord) bool {
		return r.Level == "error" && samePathAny(r.Context["path"], spoolPath)
	})
}

// TestAnUnclassifiableSpoolStillLandsAsResidue asserts the refusal does not
// drop the bytes: nothing on disk is ever lost.
func TestAnUnclassifiableSpoolStillLandsAsResidue(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	cwd := "/Users/dodgecoates/spool-residue-probe"
	slug := cwdSlug(cwd)
	session := "40404040-4040-4040-8040-404040404040"
	spoolPath := tree.spoolPath(slug, session, "z0uncla551f1able")
	payload := "bytes nobody can classify\n"

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	spool := newGrowingFile(t, spoolPath)
	spool.AppendRaw([]byte(payload))
	fake.awaitEntry(ctx, t, "residue naming the unclassifiable spool", func(e *storev1.StoreEntry) bool {
		u := e.GetAgentUpdate().GetUnservedItem().GetUnparsed()
		return u != nil && samePath(u.GetSource(), spoolPath)
	})

	// Assert.
	var found bool
	for _, r := range unparsedOf(fake.Entries()) {
		if !samePath(r.GetSource(), spoolPath) {
			continue
		}
		found = true
		if !strings.Contains(r.GetRaw(), strings.TrimSpace(payload)) {
			t.Errorf("residue for %s carries %q, wanted the file's bytes %q", spoolPath, r.GetRaw(), payload)
		}
		if r.GetParseError() == "" {
			t.Errorf("residue for %s states no parse_error, so it is not investigable", spoolPath)
		}
	}
	if !found {
		t.Fatalf("the refused spool's bytes never landed as residue")
	}
}

// TestOneSpoolReachedByTwoPathSpellingsIsOneFile asserts the /tmp ->
// /private/tmp normalization: a spool root reached through a symlink and the
// owner's resolved output path must not read as two files.
func TestOneSpoolReachedByTwoPathSpellingsIsOneFile(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	base := t.TempDir()
	realSpoolRoot := filepath.Join(base, "spool-real")
	linkSpoolRoot := filepath.Join(base, "spool-link")
	mustMkdirAll(t, realSpoolRoot)
	if err := os.Symlink(realSpoolRoot, linkSpoolRoot); err != nil {
		t.Fatalf("symlink %s -> %s: %v", linkSpoolRoot, realSpoolRoot, err)
	}
	tree := &vendorTree{t: t, Root: filepath.Join(base, "config-root"), SpoolRoot: linkSpoolRoot}
	mustMkdirAll(t, filepath.Join(tree.Root, "projects"))

	cwd := "/Users/dodgecoates/spool-symlink-probe"
	slug := cwdSlug(cwd)
	session := "50505050-5050-4050-8050-505050505050"
	captured := loadCapturedSession(t)

	// The owner names the RESOLVED spelling; the sidecar is pointed at the LINK.
	resolvedSpool := filepath.Join(realSpoolRoot, "claude-"+spoolUID, slug, session, "tasks", capturedSpoolTask1+".output")
	linkedSpool := tree.spoolPath(slug, session, capturedSpoolTask1)

	call := retargetSession(t, decodeRecord(t, captured.Lines[8]), session, cwd)
	result := retargetSession(t, decodeRecord(t, captured.Lines[10]), session, cwd)
	result = setToolResultText(t, result, backgroundLaunchText(capturedSpoolTask1, resolvedSpool))
	result = setNested(t, result, "toolUseResult", "backgroundTaskId", capturedSpoolTask1)

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	parent := newGrowingFile(t, tree.sessionPath(slug, session))
	parent.AppendLine(encodeRecord(t, call))
	parent.AppendLine(encodeRecord(t, result))
	awaitCursorInBatches(ctx, t, fake, parent.Path(), parent.Offset())

	spool := newGrowingFile(t, linkedSpool)
	spool.AppendRaw([]byte("only written once\nEXIT=0\n"))
	awaitCursorInBatches(ctx, t, fake, linkedSpool, spool.Offset())

	// Assert: one file identity, one contiguous delta sequence.
	ids := map[string]bool{}
	for _, b := range fake.Batches() {
		cs := b.GetBatch().GetCursorAdvance()
		if cs == nil {
			continue
		}
		if samePath(cs.GetPath(), resolvedSpool) {
			ids[cs.GetFileId()] = true
		}
	}
	if len(ids) > 1 {
		t.Errorf("one spool reached by two spellings produced %d file identities: %v", len(ids), sortedStrings(keysOf(ids)))
	}
	var accumulated uint64
	for _, frame := range bashFramesForRun(fake.Entries(), capturedBashCall1) {
		up := frame.GetUpdate()
		if up == nil {
			continue
		}
		if up.GetFromOffset() != accumulated {
			t.Fatalf("the two spellings were read as two files: an update states from_offset %d where %d was accumulated",
				up.GetFromOffset(), accumulated)
		}
		accumulated += uint64(len(up.GetNewOutput()))
	}
}
