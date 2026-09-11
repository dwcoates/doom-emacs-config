package integration

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	storev1 "agentrepl/proto/store/v1"
)

// SUBJECT 4 — who a spool belongs to, and what happens when nobody claims it.
//
// A spool that appears BEFORE the transcript line naming it is retained and
// re-checked, never dropped; an unclassifiable task-id prefix is a loud
// total-ingestion violation whose bytes still land as residue; and the /tmp
// versus /private/tmp spellings of one file are one file.

// TestAnUnownedSpoolIsHeldUntilItsWindowLapsesAndThenLandsAsResidue asserts the
// whole shape of the hold, which the design states in two halves:
//
//   - HELD MEANS DISCOVERED AND RE-CHECKED, NOT TAILED. A spool whose spawning
//     call has not been read names no run, so reading it would mean either
//     inventing an owner or keying its output on the spool path's runtime id.
//     Nothing is read, so no cursor is offered for it.
//   - AN AGED UNOWNED SPOOL IS NEVER DROPPED. Once the bounded wait lapses the
//     bytes are ingested attributed to the residue path, with a WARNING, and the
//     file keeps being tailed — so a cursor appears exactly then and not before.
func TestAnUnownedSpoolIsHeldUntilItsWindowLapsesAndThenLandsAsResidue(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	cwd := "/Users/dodgecoates/spool-orphan-probe"
	slug := cwdSlug(cwd)
	session := "10101010-1010-4010-8010-101010101010"
	spoolPath := tree.spoolPath(slug, session, capturedSpoolTask1)
	payload := "output written before anyone claimed it\n"
	// The suite default (200ms). This used to be 2s so that "held but not yet
	// lapsed" was a wide enough window for the assertion below to land inside;
	// the assertion is an ORDERING now (see below) rather than a snapshot taken
	// during a race, so the window buys nothing and the subject pays production
	// nothing to wait it out.
	opts := defaultSidecarOptions(t, fake.Socket, tree)

	// Act: the spool exists and no transcript ever names it. It is created AFTER
	// the first production cycle so it is a steady-state spool, not startup
	// backlog: a spool already on disk when the reader's first scan runs is
	// caught up on and summarized (see the catch-up subjects), while one that
	// appears while the reader is running is the per-file degradation this
	// subject asserts.
	startSidecar(t, opts)
	awaitFirstProductionCycle(ctx, t, opts.LogPath)
	spool := newGrowingFile(t, spoolPath)
	spool.AppendRaw([]byte(payload))
	awaitLog(ctx, t, opts.LogPath, "the spool being held", func(r logRecord) bool {
		return r.Operation == "hold-spool" && samePathAny(r.Context["path"], spoolPath)
	})

	// Act (the second half): let the bounded wait lapse.
	awaitLog(ctx, t, opts.LogPath, "the hold expiring", func(r logRecord) bool {
		return r.Operation == "hold-expired" && samePathAny(r.Context["path"], spoolPath)
	})
	fake.awaitEntry(ctx, t, "residue naming the aged spool", func(e *storev1.StoreEntry) bool {
		u := e.GetAgentUpdate().GetUnservedItem().GetUnparsed()
		return u != nil && samePath(u.GetSource(), spoolPath)
	})

	// Assert (the first half): HELD IS NOT TAILED, stated as an ordering on the
	// wire rather than as a look taken while the hold happened to still stand.
	// The write stream is in write order, so "no cursor for this spool was
	// offered before the batch that carried its residue" says exactly what the
	// design says — nothing was read until the wait lapsed — and says it
	// whatever the window's length or the machine's load.
	batches := fake.Batches()
	residueAt := -1
	for i, b := range batches {
		for _, e := range b.GetBatch().GetEntries() {
			u := e.GetAgentUpdate().GetUnservedItem().GetUnparsed()
			if u != nil && samePath(u.GetSource(), spoolPath) {
				residueAt = i
				break
			}
		}
		if residueAt >= 0 {
			break
		}
	}
	if residueAt < 0 {
		t.Fatalf("no batch carried residue naming %s, so there is no lapse to order anything against", spoolPath)
	}
	for _, b := range batches[:residueAt] {
		if cs := b.GetBatch().GetCursorAdvance(); cs != nil && samePath(cs.GetPath(), spoolPath) {
			t.Fatalf("a held spool was tailed: a cursor for %s was offered at %d before its bytes were ingested as residue, while its owner was unknown",
				spoolPath, cs.GetOffset())
		}
	}

	// Assert (the second half): the bytes landed and the file is being read.
	awaitAnyCursorFor(ctx, t, fake, spoolPath)
	var found bool
	for _, r := range unparsedOf(fake.Entries()) {
		if !samePath(r.GetSource(), spoolPath) {
			continue
		}
		found = true
		if !strings.Contains(r.GetRaw(), strings.TrimSpace(payload)) {
			t.Errorf("residue for %s carries %q, wanted the spool's bytes %q", spoolPath, r.GetRaw(), payload)
		}
	}
	if !found {
		t.Fatal("an aged unowned spool's bytes were dropped rather than ingested as residue")
	}
}

// TestTheHoldOfAnUnownedSpoolIsStatedAsAWarningWhenItLapses asserts the lapse is
// a stated degradation rather than a silent reclassification: the bytes stop
// being a shell run's output and become residue, and that is worth saying once.
func TestTheHoldOfAnUnownedSpoolIsStatedAsAWarningWhenItLapses(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	cwd := "/Users/dodgecoates/spool-orphan-warning-probe"
	slug := cwdSlug(cwd)
	session := "11111111-1111-4111-8111-111111111111"
	spoolPath := tree.spoolPath(slug, session, capturedSpoolTask1)
	opts := defaultSidecarOptions(t, fake.Socket, tree)

	// Act: created after the first cycle so it is a steady-state spool whose
	// lapse is a per-file degradation, not startup backlog (which is summarized).
	startSidecar(t, opts)
	awaitFirstProductionCycle(ctx, t, opts.LogPath)
	spool := newGrowingFile(t, spoolPath)
	spool.AppendRaw([]byte("nobody claimed this\n"))

	// Assert.
	rec := awaitLog(ctx, t, opts.LogPath, "the hold-expiry warning", func(r logRecord) bool {
		return r.Operation == "hold-expired" && samePathAny(r.Context["path"], spoolPath)
	})
	if rec.Level != "warn" {
		t.Errorf("the hold expired at level %q; an aged unowned spool is a degradation and is stated as a WARNING", rec.Level)
	}
	_ = fake
}

// TestStartupCatchUpSummarizesABacklogOfUnownedSpools asserts the realtest-1
// flood is leveled: a sidecar that starts with a backlog of pre-existing
// unclaimed spools states ONE summary rather than one warning per spool. The
// spools are created BEFORE the sidecar starts, so they are exactly the
// historical corpus a restart catches up on.
func TestStartupCatchUpSummarizesABacklogOfUnownedSpools(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	cwd := "/Users/dodgecoates/spool-catchup-probe"
	slug := cwdSlug(cwd)
	session := "12121212-1212-4212-8212-121212121212"
	opts := defaultSidecarOptions(t, fake.Socket, tree)
	tasks := []string{"b0aaaaaaa", "b0bbbbbbb", "b0ccccccc"}
	var spoolPaths []string
	for _, task := range tasks {
		path := tree.spoolPath(slug, session, task)
		spool := newGrowingFile(t, path)
		spool.AppendRaw([]byte("backlog output nobody claimed\n"))
		spoolPaths = append(spoolPaths, path)
	}

	// Act: the sidecar starts with the backlog already on disk.
	startSidecar(t, opts)
	for _, spoolPath := range spoolPaths {
		sp := spoolPath
		fake.awaitEntry(ctx, t, "residue for the backlog spool", func(e *storev1.StoreEntry) bool {
			u := e.GetAgentUpdate().GetUnservedItem().GetUnparsed()
			return u != nil && samePath(u.GetSource(), sp)
		})
	}
	rec := awaitLog(ctx, t, opts.LogPath, "the spool catch-up summary", func(r logRecord) bool {
		return r.Operation == "catchup-summary" && r.Context["reason"] == "spool_unclaimed"
	})

	// Assert: the backlog is summarized, not stated one spool at a time. The
	// bytes still landed as residue (awaited above), so nothing was silenced.
	if rec.Level != "warn" {
		t.Errorf("the catch-up summary is at level %q, want warn", rec.Level)
	}
	records := readLog(t, opts.LogPath)
	var summed int
	for _, r := range records {
		if r.Operation == "catchup-summary" && r.Context["reason"] == "spool_unclaimed" {
			if c, ok := r.Context["repeat_count"].(float64); ok {
				summed += int(c)
			}
		}
	}
	if summed != len(tasks) {
		t.Errorf("the catch-up summaries counted %d backlog spools, want %d", summed, len(tasks))
	}
	for _, r := range records {
		if r.Operation == "hold-expired" && r.Level == "warn" {
			t.Errorf("a backlog spool was stated as a per-file warning: %v", r.Context)
		}
	}
}

// TestAnUnownedSpoolIsAttributedOnceItsOwnerAppears asserts the retained spool
// is attributed to the spawning call as soon as the transcript names it — which
// is the point of holding rather than reading it: the run's whole output lands
// under the call's identity, with no prefix of it stranded as residue.
func TestAnUnownedSpoolIsAttributedOnceItsOwnerAppears(t *testing.T) {
	t.Parallel()
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
	opts := defaultSidecarOptions(t, fake.Socket, tree)
	// The owner arrives well inside the window, which is the ordinary case: the
	// launch line is written when the task starts.
	opts.UnownedSpoolWindow = 30 * time.Second

	call := retargetSession(t, decodeRecord(t, captured.Lines[8]), session, cwd)
	result := retargetSession(t, decodeRecord(t, captured.Lines[10]), session, cwd)
	result = setToolResultText(t, result, backgroundLaunchText(capturedSpoolTask1, spoolPath))
	result = setNested(t, result, "toolUseResult", "backgroundTaskId", capturedSpoolTask1)

	// Act: the spool appears first and is HELD, so no cursor is offered for it.
	startSidecar(t, opts)
	spool := newGrowingFile(t, spoolPath)
	spool.AppendRaw([]byte("orphaned output\n"))
	awaitLog(ctx, t, opts.LogPath, "the spool being held", func(r logRecord) bool {
		return r.Operation == "hold-spool" && samePathAny(r.Context["path"], spoolPath)
	})

	parent := newGrowingFile(t, tree.sessionPath(slug, session))
	parent.AppendLine(encodeRecord(t, call))
	parent.AppendLine(encodeRecord(t, result))
	fake.awaitEntry(ctx, t, "a bash frame for the now-resolved owner", func(e *storev1.StoreEntry) bool {
		return e.GetAgentUpdate().GetBash().GetRun().GetValue() == capturedBashCall1
	})

	// Assert: the whole spool landed under the spawning call, none of it as
	// residue.
	frames := bashFramesForRun(fake.Entries(), capturedBashCall1)
	if len(frames) == 0 {
		t.Fatalf("the spool was never attributed to its owner; runs seen: %v", runsSeen(fake.Entries()))
	}
	for _, r := range unparsedOf(fake.Entries()) {
		if samePath(r.GetSource(), spoolPath) {
			t.Errorf("a spool that was claimed inside its window still had bytes ingested as residue: %q", r.GetRaw())
		}
	}
}

// TestAnUnclassifiableSpoolPrefixIsRefusedLoudly asserts a task id with no
// known kind prefix is an ERROR — a total-ingestion violation stated out loud.
func TestAnUnclassifiableSpoolPrefixIsRefusedLoudly(t *testing.T) {
	t.Parallel()
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
	t.Parallel()
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
	t.Parallel()
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

// TestTheSpoolRootIsAcceptedAtEitherLevel asserts --spool-root works pointed at
// the claude-<uid> directory ITSELF, not only at its parent: the launchd default
// names /tmp while a mock harness names /tmp/claude-<uid>, and the same file must
// be discovered either way. The fixture does not move — only the flag's level.
func TestTheSpoolRootIsAcceptedAtEitherLevel(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	cwd := "/Users/dodgecoates/spool-uid-level-probe"
	slug := cwdSlug(cwd)
	session := "60606060-6060-4060-8060-60606060aaaa"
	spoolPath := tree.spoolPath(slug, session, capturedSpoolTask1)

	opts := defaultSidecarOptions(t, fake.Socket, tree)
	// The uid directory itself, one level below the launchd default.
	opts.SpoolRoot = filepath.Join(tree.SpoolRoot, "claude-"+spoolUID)

	// Act.
	startSidecar(t, opts)
	spool := newGrowingFile(t, spoolPath)
	spool.AppendRaw([]byte("discovered from the uid level too\n"))

	// Assert.
	awaitAnyCursorFor(ctx, t, fake, spoolPath)
}

// TestResidueCarriesNoTopLevel asserts residue names no agent: an unparsed
// record may belong to nothing, and top_level is UNSET rather than guessed.
func TestResidueCarriesNoTopLevel(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	cwd := "/Users/dodgecoates/residue-toplevel-probe"
	slug := cwdSlug(cwd)
	session := "70707070-7070-4070-8070-70707070aaaa"
	spoolPath := tree.spoolPath(slug, session, "z0uncla551f1able")

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	spool := newGrowingFile(t, spoolPath)
	spool.AppendRaw([]byte("bytes belonging to nobody\n"))
	fake.awaitEntry(ctx, t, "residue naming the unclassifiable spool", func(e *storev1.StoreEntry) bool {
		u := e.GetAgentUpdate().GetUnservedItem().GetUnparsed()
		return u != nil && samePath(u.GetSource(), spoolPath)
	})

	// Assert.
	for _, e := range fake.Entries() {
		u := e.GetAgentUpdate().GetUnservedItem().GetUnparsed()
		if u == nil || !samePath(u.GetSource(), spoolPath) {
			continue
		}
		if got := e.GetAgentUpdate().GetTopLevel(); got != nil {
			t.Errorf("residue entry %q names top_level %q; residue that names no agent must leave it UNSET",
				e.GetUpsertKey(), got.GetValue())
		}
	}
}
