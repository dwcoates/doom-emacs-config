package integration

import (
	"context"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	storev1 "agentrepl/proto/store/v1"
)

// SUBJECT 4 — who a spool belongs to, and what happens when nobody claims it.
//
// ONLY WHAT IS RENDERED IS READ. A spool is read for the run a spawning call
// claimed and for nothing else: one that appears BEFORE the transcript line
// naming it is held and re-checked, never read, and stays unread after its
// window lapses — until a launch claims it. A spool whose task id has no a/b/w
// prefix is refused loudly and never read. The /tmp versus /private/tmp
// spellings of one file are one file.
//
// "NEVER READ" IS ASSERTED ON A POSITIVE SIGNAL: the reader re-resolves every
// UNWATCHED spool on every rescan and says so at debug, so a later record naming
// the spool is proof it is still unwatched — a watched file is never resolved
// again. The store then holds no cursor for it and the log no residue.

// awaitRestatedAfter waits for a record of `operation` naming path that was
// written AFTER log index `after`: the next rescan re-resolving a spool it still
// does not watch.
func awaitRestatedAfter(ctx context.Context, t *testing.T, logPath, path, operation string, after int) {
	t.Helper()
	tick := time.NewTicker(pollTick)
	defer tick.Stop()
	for {
		for i, r := range readLog(t, logPath) {
			if i > after && r.Operation == operation && samePathAny(r.Context["path"], path) {
				return
			}
		}
		select {
		case <-ctx.Done():
			t.Fatalf("no %s record for %s after log index %d: the spool was not re-resolved, so it is being watched", operation, path, after)
		case <-tick.C:
		}
	}
}

// requireNeverRead asserts a spool left no trace of a read: no cursor offered
// for it, and no residue classified out of it.
func requireNeverRead(t *testing.T, fake *fakeStore, logPath, path string) {
	t.Helper()
	if cs := latestCursorFor(fake.Batches(), path); cs != nil {
		t.Fatalf("a spool nothing renders was read: the store was offered a cursor for %s at offset %d", path, cs.GetOffset())
	}
	id := fileID(t, path)
	for _, r := range readLog(t, logPath) {
		if r.Operation == "residue-drop" && r.Context["file_id"] == id {
			t.Fatalf("a spool nothing renders was read: its bytes were classified as residue: %v", r.Context)
		}
	}
}

// logIndexOf answers where in the log a record matching `match` first appears,
// or -1.
func logIndexOf(t *testing.T, logPath string, match func(logRecord) bool) int {
	t.Helper()
	for i, r := range readLog(t, logPath) {
		if match(r) {
			return i
		}
	}
	return -1
}

// TestAnUnownedSpoolIsNeverReadEvenAfterItsWindowLapses asserts the whole shape
// of the hold:
//
//   - HELD MEANS DISCOVERED AND RE-CHECKED, NOT READ. A spool whose spawning call
//     has not been read names no run, so reading it would mean either inventing
//     an owner or keying its output on the spool path's runtime id.
//   - A LAPSED HOLD CHANGES WHAT IS SAID, NOT WHAT IS READ. Nothing renders a
//     spool no call claimed, so once the window lapses it is stated once and
//     left unread — however much the file keeps growing.
func TestAnUnownedSpoolIsNeverReadEvenAfterItsWindowLapses(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	cwd := "/work/spool-orphan-probe"
	slug := cwdSlug(cwd)
	session := "10101010-1010-4010-8010-101010101010"
	spoolPath := tree.spoolPath(slug, session, capturedSpoolTask1)
	// The re-resolution this subject waits on is stated at DEBUG.
	opts := debugLogging(defaultSidecarOptions(t, fake.Socket, tree))

	// Act: the spool appears after catch-up, so it is steady state, and keeps
	// growing past its window as a test log does.
	startSidecar(t, opts)
	awaitCatchupEnd(ctx, t, opts.LogPath)
	spool := newGrowingFile(t, spoolPath)
	spool.AppendRaw([]byte("output written before anyone claimed it\n"))
	awaitLog(ctx, t, opts.LogPath, "the hold expiring", func(r logRecord) bool {
		return r.Operation == "hold-expired" && samePathAny(r.Context["path"], spoolPath)
	})
	spool.AppendRaw([]byte("and it kept growing\n"))
	lapsedAt := logIndexOf(t, opts.LogPath, func(r logRecord) bool {
		return r.Operation == "hold-expired" && samePathAny(r.Context["path"], spoolPath)
	})
	awaitRestatedAfter(ctx, t, opts.LogPath, spoolPath, "hold-spool", lapsedAt)

	// Assert.
	requireNeverRead(t, fake, opts.LogPath, spoolPath)
}

// TestTheLapseOfAnUnownedSpoolIsStatedAtInfo asserts the lapse is a stated
// decision rather than a silent one: the spool is not read, and that is worth
// saying once, with its path and its reason.
func TestTheLapseOfAnUnownedSpoolIsStatedAtInfo(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	cwd := "/work/spool-orphan-warning-probe"
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
	rec := awaitLog(ctx, t, opts.LogPath, "the hold-expiry record", func(r logRecord) bool {
		return r.Operation == "hold-expired" && samePathAny(r.Context["path"], spoolPath)
	})
	if rec.Level != "info" {
		t.Errorf("the hold expired at level %q; not reading what nothing renders is the rule working and is stated at INFO", rec.Level)
	}
	if rec.Context["reason"] != "spool_unclaimed" {
		t.Errorf("the hold-expiry record states reason %v, want spool_unclaimed", rec.Context["reason"])
	}
	_ = fake
}

// backlogPayload is the one line each backlog spool holds.
const backlogPayload = "backlog output nobody claimed\n"

// TestStartupCatchUpSummarizesABacklogOfUnownedSpools asserts the realtest-1
// flood is leveled: a sidecar that starts with a backlog of pre-existing
// unclaimed spools states ONE summary rather than one record per spool — and
// reads none of them. The spools are created BEFORE the sidecar starts, so they
// are exactly the historical corpus a restart catches up on.
func TestStartupCatchUpSummarizesABacklogOfUnownedSpools(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	cwd := "/work/spool-catchup-probe"
	slug := cwdSlug(cwd)
	session := "12121212-1212-4212-8212-121212121212"
	// The re-resolution the never-read half waits on is stated at DEBUG.
	opts := debugLogging(defaultSidecarOptions(t, fake.Socket, tree))
	tasks := []string{"b0aaaaaaa", "b0bbbbbbb", "b0ccccccc"}
	var spoolPaths []string
	for _, task := range tasks {
		path := tree.spoolPath(slug, session, task)
		spool := newGrowingFile(t, path)
		spool.AppendRaw([]byte(backlogPayload))
		spoolPaths = append(spoolPaths, path)
	}

	// Act: the sidecar starts with the backlog already on disk.
	startSidecar(t, opts)
	rec := awaitLog(ctx, t, opts.LogPath, "the spool catch-up summary", func(r logRecord) bool {
		return r.Operation == "catchup-summary" && r.Context["reason"] == "spool_unclaimed"
	})
	summaryAt := logIndexOf(t, opts.LogPath, func(r logRecord) bool {
		return r.Operation == "catchup-summary" && r.Context["reason"] == "spool_unclaimed"
	})
	for _, spoolPath := range spoolPaths {
		awaitRestatedAfter(ctx, t, opts.LogPath, spoolPath, "hold-spool", summaryAt)
	}

	// Assert: the backlog is summarized, not stated one spool at a time...
	if rec.Level != "info" {
		t.Errorf("the catch-up summary is at level %q, want info", rec.Level)
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
		if r.Operation == "hold-expired" && r.Level == "info" {
			t.Errorf("a backlog spool was stated as a per-file record: %v", r.Context)
		}
	}
	// ...and none of it was read.
	for _, spoolPath := range spoolPaths {
		requireNeverRead(t, fake, opts.LogPath, spoolPath)
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
	cwd := "/work/spool-late-owner-probe"
	slug := cwdSlug(cwd)
	session := "20202020-2020-4020-8020-202020202020"
	captured := loadCapturedSession(t)
	spoolPath := tree.spoolPath(slug, session, capturedSpoolTask1)
	opts := defaultSidecarOptions(t, fake.Socket, tree)
	// The owner arrives well inside the window, which is the ordinary case: the
	// launch line is written when the task starts.
	opts.UnownedSpoolWindow = 30 * time.Second
	// THE PER-ITEM DETAIL LIVES AT DEBUG DURING CATCH-UP. This record is one of
	// the six corpus-walk operations the startup catch-up window levels (see
	// "Startup catch-up"), and this subject asserts the per-item record rather
	// than the summary, so it reads the log at the threshold the detail is
	// written to.
	opts = debugLogging(opts)

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
	// NOT ONE LINE OF IT WAS EVER CLASSIFIED AS RESIDUE. Residue is never
	// stored, so an absence in the store would now be true of a spool that fell
	// to residue as well; the reader's own withholding record is what separates
	// "attributed" from "read as bytes belonging to nobody".
	id := fileID(t, spoolPath)
	for _, r := range readLog(t, opts.LogPath) {
		if r.Operation == "residue-drop" && r.Context["file_id"] == id {
			t.Errorf("a spool that was claimed inside its window still had bytes classified as residue: %v", r.Context)
		}
	}
	requireNoResidueStored(t, fake.Entries())
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
	cwd := "/work/spool-prefix-probe"
	slug := cwdSlug(cwd)
	session := "30303030-3030-4030-8030-303030303030"
	opts := defaultSidecarOptions(t, fake.Socket, tree)
	// z* is none of b* (shell), a* (agent transcript) or w* (workflow journal).
	spoolPath := tree.spoolPath(slug, session, "z0uncla551f1able")

	// Act: the spool appears after catch-up, so its skip is a steady-state
	// decision stated at INFO rather than a demoted catch-up record.
	startSidecar(t, opts)
	awaitCatchupEnd(ctx, t, opts.LogPath)
	spool := newGrowingFile(t, spoolPath)
	spool.AppendRaw([]byte("bytes nobody can classify\n"))

	// Assert.
	awaitLog(ctx, t, opts.LogPath, "the unclassifiable-prefix refusal", func(r logRecord) bool {
		return r.Level == "error" && samePathAny(r.Context["path"], spoolPath)
	})
}

// TestAnUnclassifiableSpoolIsNeverRead asserts the refusal is the whole
// outcome: no conversion can be selected for the bytes, so nothing could render
// them, and the reader states why it skipped the file rather than reading it.
func TestAnUnclassifiableSpoolIsNeverRead(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	cwd := "/work/spool-residue-probe"
	slug := cwdSlug(cwd)
	session := "40404040-4040-4040-8040-404040404040"
	spoolPath := tree.spoolPath(slug, session, "z0uncla551f1able")
	// The repeat of the skip this subject waits on is stated at DEBUG.
	opts := debugLogging(defaultSidecarOptions(t, fake.Socket, tree))

	// Act: the spool appears after catch-up, so its skip is a steady-state
	// decision stated at INFO rather than a demoted catch-up record.
	startSidecar(t, opts)
	awaitCatchupEnd(ctx, t, opts.LogPath)
	spool := newGrowingFile(t, spoolPath)
	spool.AppendRaw([]byte("bytes nobody can classify\n"))
	rec := awaitLog(ctx, t, opts.LogPath, "the skip decision", func(r logRecord) bool {
		return r.Operation == "spool-skip" && r.Level == "info" && samePathAny(r.Context["path"], spoolPath)
	})
	skippedAt := logIndexOf(t, opts.LogPath, func(r logRecord) bool {
		return r.Operation == "spool-skip" && r.Level == "info" && samePathAny(r.Context["path"], spoolPath)
	})
	awaitRestatedAfter(ctx, t, opts.LogPath, spoolPath, "spool-skip", skippedAt)

	// Assert.
	if rec.Context["reason"] != "unrecognized_prefix" {
		t.Errorf("the skip states reason %v, want unrecognized_prefix", rec.Context["reason"])
	}
	requireNeverRead(t, fake, opts.LogPath, spoolPath)
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
	tree := &vendorTree{t: t, Root: filepath.Join(base, "config-root"), SpoolRoot: linkSpoolRoot, live: liveRootFor(t)}
	mustMkdirAll(t, filepath.Join(tree.Root, "projects"))

	cwd := "/work/spool-symlink-probe"
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

	// Assert: one file identity, one tail whose accounting never runs backwards.
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
	// Read as two files, two windows would supersede one tail row in turn and
	// the accounting would run backwards; read as one, it never does.
	if got := requireLatestTail(t, capturedBashCall1, bashFramesForRun(fake.Entries(), capturedBashCall1)); !strings.HasSuffix(got, "only written once\nEXIT=0\n") {
		t.Errorf("the run's tail is %q, want it to end on the bytes written once", got)
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
	cwd := "/work/spool-uid-level-probe"
	slug := cwdSlug(cwd)
	session := "60606060-6060-4060-8060-60606060aaaa"
	spoolPath := tree.spoolPath(slug, session, capturedSpoolTask1)

	opts := defaultSidecarOptions(t, fake.Socket, tree)
	// The uid directory itself, one level below the launchd default.
	opts.SpoolRoot = filepath.Join(tree.SpoolRoot, "claude-"+spoolUID)

	// Act: the spool appears after catch-up, so its first hold is stated at
	// INFO rather than demoted as a catch-up record.
	startSidecar(t, opts)
	awaitCatchupEnd(ctx, t, opts.LogPath)
	spool := newGrowingFile(t, spoolPath)
	spool.AppendRaw([]byte("discovered from the uid level too\n"))

	// Assert: discovered from the uid level — the hold names the file, which
	// only a discovered spool can have.
	awaitLog(ctx, t, opts.LogPath, "the spool being held", func(r logRecord) bool {
		return r.Operation == "hold-spool" && samePathAny(r.Context["path"], spoolPath)
	})
	_ = fake
}
