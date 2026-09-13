package integration

import (
	"context"
	"os"
	"path/filepath"
	"testing"
	"time"

	storev1 "agentrepl/proto/store/v1"
)

// SUBJECT 4 — who a spool belongs to, and what happens when nobody claims it.
//
// A spool that appears BEFORE the transcript line naming it is retained and
// re-checked, never dropped; an unclassifiable task-id prefix is a loud
// total-ingestion violation whose bytes are still read whole and classified as
// residue; and the /tmp versus /private/tmp spellings of one file are one file.
//
// RESIDUE IS NEVER PERSISTED, so "landed as residue" is asserted where the
// evidence now is: the reader's own withholding record, plus a cursor that
// reached the end of the file. The store must hold no residue row at all.

// awaitResidueWithheldNamingFile waits for the sidecar's statement that it
// classified a record read from ONE file as residue and did not store it.
//
// THE FILE IS THE POINT HERE. `awaitResidueWithheld` matches a label anywhere in
// the process, which is enough for a subject about a label but not for one about
// a particular spool: these subjects say "THIS file's bytes were read and
// classified", and the record's file_id is what says which file that was.
func awaitResidueWithheldNamingFile(ctx context.Context, t *testing.T, logPath, path, label string) logRecord {
	t.Helper()
	id := fileID(t, path)
	return awaitLog(ctx, t, logPath, "residue "+label+" classified and withheld for "+path, func(r logRecord) bool {
		return r.Operation == "residue-drop" && r.Context["reason"] == label && r.Context["file_id"] == id
	})
}

// firstResidueWithheldIndexFor answers where in the log the reader FIRST said it
// had classified residue out of one file, or -1. It is an index rather than a
// timestamp because the log is written in order, and ordering is the only thing
// a subject about "nothing was read before X" needs.
func firstResidueWithheldIndexFor(t *testing.T, logPath, path string) int {
	t.Helper()
	id := fileID(t, path)
	for i, r := range readLog(t, logPath) {
		if r.Operation == "residue-drop" && r.Context["file_id"] == id {
			return i
		}
	}
	return -1
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

// TestAnUnownedSpoolIsHeldUntilItsWindowLapsesAndThenLandsAsResidue asserts the
// whole shape of the hold, which the design states in two halves:
//
//   - HELD MEANS DISCOVERED AND RE-CHECKED, NOT TAILED. A spool whose spawning
//     call has not been read names no run, so reading it would mean either
//     inventing an owner or keying its output on the spool path's runtime id.
//     Nothing is read, so no cursor is offered for it.
//   - AN AGED UNOWNED SPOOL IS NEVER DROPPED. Once the bounded wait lapses the
//     bytes are READ WHOLE and classified as residue, with a WARNING, and the
//     file keeps being tailed — so a cursor appears exactly then and not before.
//     RESIDUE IS NEVER PERSISTED, so what the reader saw is stated in the log
//     rather than stored: the file is still read to its end, and the store holds
//     no row for it.
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
	// THE WITHHOLDING IS STATED PER RECORD AT DEBUG, because it is the steady
	// state rather than news. This subject asserts the per-record statement, so
	// it reads the log at the threshold that statement is written to.
	opts := debugLogging(defaultSidecarOptions(t, fake.Socket, tree))

	// Act: the spool exists and no transcript ever names it. It is created AFTER
	// the first production cycle so it is a steady-state spool, not startup
	// backlog: a spool already on disk when the reader's first scan runs is
	// caught up on and summarized (see the catch-up subjects), while one that
	// appears while the reader is running is the per-file degradation this
	// subject asserts.
	startSidecar(t, opts)
	awaitCatchupEnd(ctx, t, opts.LogPath)
	spool := newGrowingFile(t, spoolPath)
	spool.AppendRaw([]byte(payload))
	awaitLog(ctx, t, opts.LogPath, "the spool being held", func(r logRecord) bool {
		return r.Operation == "hold-spool" && samePathAny(r.Context["path"], spoolPath)
	})

	// Act (the second half): let the bounded wait lapse.
	awaitLog(ctx, t, opts.LogPath, "the hold expiring", func(r logRecord) bool {
		return r.Operation == "hold-expired" && samePathAny(r.Context["path"], spoolPath)
	})
	awaitResidueWithheldNamingFile(ctx, t, opts.LogPath, spoolPath, "unparsed")

	// Assert (the first half): HELD IS NOT TAILED, stated as an ordering in the
	// log rather than as a look taken while the hold happened to still stand.
	// Every line of an unowned spool classifies as `unparsed`, so the reader's
	// first withholding record for this file IS the moment it first read it, and
	// "the lapse was written before it" says exactly what the design says —
	// nothing was read until the wait lapsed — whatever the window's length or
	// the machine's load.
	lapsedAt := logIndexOf(t, opts.LogPath, func(r logRecord) bool {
		return r.Operation == "hold-expired" && samePathAny(r.Context["path"], spoolPath)
	})
	readAt := firstResidueWithheldIndexFor(t, opts.LogPath, spoolPath)
	if readAt < 0 {
		t.Fatalf("the reader never said it classified %s, so there is no read to order against the lapse", spoolPath)
	}
	if lapsedAt < 0 || lapsedAt > readAt {
		t.Fatalf("a held spool was tailed: %s was read and classified at log record %d, before its hold expired at %d, while its owner was unknown",
			spoolPath, readAt, lapsedAt)
	}

	// Assert (the second half): the file is being read, and read WHOLE — the
	// cursor reaching the end of the payload is what says the bytes were
	// ingested rather than dropped, now that residue leaves no row behind.
	awaitCursorInBatches(ctx, t, fake, spoolPath, spool.Offset())

	// Assert (the invariant): none of it was stored. The bytes were read,
	// classified and counted; residue is never persisted.
	requireNoResidueStored(t, fake.Entries())
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
	rec := awaitLog(ctx, t, opts.LogPath, "the hold-expiry record", func(r logRecord) bool {
		return r.Operation == "hold-expired" && samePathAny(r.Context["path"], spoolPath)
	})
	if rec.Level != "info" {
		t.Errorf("the hold expired at level %q; an aged unowned spool is the mandate working and is stated at INFO", rec.Level)
	}
	_ = fake
}

// backlogPayload is the one line each backlog spool holds, so a file's withheld
// tally is exactly one `unparsed` record and its summary's count is readable.
const backlogPayload = "backlog output nobody claimed\n"

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
	// The per-line withholding statement is DEBUG; the leveling this subject is
	// about is stated at INFO either way.
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
	for _, spoolPath := range spoolPaths {
		// EVERY BACKLOG SPOOL IS READ WHOLE. Its bytes are never stored — they
		// classify as residue — so the cursor reaching the end of the file is
		// what says the reader ingested it rather than skipped it.
		awaitCursorInBatches(ctx, t, fake, spoolPath, int64(len(backlogPayload)))
	}
	rec := awaitLog(ctx, t, opts.LogPath, "the spool catch-up summary", func(r logRecord) bool {
		return r.Operation == "catchup-summary" && r.Context["reason"] == "spool_unclaimed"
	})
	// EVERY BACKLOG SPOOL WAS ALSO CLASSIFIED, one record per line read. The
	// bytes are never stored, so this is what says the reader saw them rather
	// than merely stepped its cursor past them.
	for _, spoolPath := range spoolPaths {
		awaitResidueWithheldNamingFile(ctx, t, opts.LogPath, spoolPath, "unparsed")
	}

	// Assert: the backlog is summarized, not stated one spool at a time. The
	// bytes were still read whole and classified (awaited above), so nothing was
	// silenced — only unstored.
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
	requireNoResidueStored(t, fake.Entries())
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

// TestAnUnclassifiableSpoolIsStillReadWholeAndClassifiedAsResidue asserts the
// refusal does not drop the bytes: nothing on disk is ever skipped. The reader
// still reads the file to its end and still classifies what it read — and
// because residue is never persisted, what it read is stated in the log and the
// cursor, not in a row.
func TestAnUnclassifiableSpoolIsStillReadWholeAndClassifiedAsResidue(t *testing.T) {
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
	// The withholding is stated per record at DEBUG, which is where this
	// subject's evidence lives.
	opts := debugLogging(defaultSidecarOptions(t, fake.Socket, tree))

	// Act.
	startSidecar(t, opts)
	spool := newGrowingFile(t, spoolPath)
	spool.AppendRaw([]byte(payload))
	awaitResidueWithheldNamingFile(ctx, t, opts.LogPath, spoolPath, "unparsed")

	// Assert: read to the end of the file — the cursor is what says the bytes
	// were consumed rather than skipped over.
	awaitCursorInBatches(ctx, t, fake, spoolPath, spool.Offset())
	// And stored nowhere.
	requireNoResidueStored(t, fake.Entries())
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

// TestWithheldResidueAnnouncesNoRow asserts residue attributes itself to
// nothing at all: an unclassifiable record may belong to nobody, so it names no
// agent — and now that it is never persisted, it names no ROW either. The
// reader's withholding record therefore carries no upsert_key, because a record
// naming a key nobody can look up is an untraceable announcement.
func TestWithheldResidueAnnouncesNoRow(t *testing.T) {
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
	opts := debugLogging(defaultSidecarOptions(t, fake.Socket, tree))

	// Act.
	startSidecar(t, opts)
	spool := newGrowingFile(t, spoolPath)
	spool.AppendRaw([]byte("bytes belonging to nobody\n"))
	rec := awaitResidueWithheldNamingFile(ctx, t, opts.LogPath, spoolPath, "unparsed")

	// Assert.
	if key, ok := rec.Context["upsert_key"]; ok && key != "" {
		t.Errorf("the withholding record names upsert_key %v; it announces no row, and a key nobody can look up is untraceable", key)
	}
	requireNoResidueStored(t, fake.Entries())
}
