package integration

import (
	"path/filepath"
	"testing"
	"time"
)

// SUBJECTS — four branches the harness already had the hooks for and nothing
// drove: a store that violates the refusal contract, a parked file surviving a
// store bounce, two calls claiming one spool, and a stop that arrives before
// the spool it cancels has an owner.
//
// EVERY ONE OF THEM IS A BRANCH WHERE GUESSING IS THE TEMPTING ANSWER, and the
// contract's answer in each case is to say so out loud and do the conservative
// thing instead.

// TestAKindlessRefusalIsAContractViolationThatStillSuspends drives the fake
// store's FailWritesWithoutAKind hook: a refusal carrying NEITHER arm.
//
// The kind is the arm that says whether a retry can help, so a store that omits
// it has told the producer nothing actionable. Ruling R-S2's answer is both
// halves at once: the violation is stated as an ERROR, and the refusal is then
// treated as the RECOVERABLE kind — production suspends and recovers — rather
// than parking a file on a verdict the store never actually gave.
func TestAKindlessRefusalIsAContractViolationThatStillSuspends(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	opts := defaultSidecarOptions(t, fake.Socket, tree)
	fake.FailWritesWithoutAKind(2, "the store refused without saying what kind of refusal this is")

	// Act.
	startSidecar(t, opts)
	g := newGrowingFile(t, tree.sessionPath(captured.Slug, captured.Session))
	for _, line := range captured.Lines {
		g.AppendLine(line)
	}
	awaitLog(ctx, t, opts.LogPath, "the kindless-refusal error", func(r logRecord) bool {
		if r.Operation != "storeclient-write-batch" || r.Level != "error" {
			return false
		}
		_, named := r.Context["refusal_kind"]
		return !named
	})

	// Assert: it suspended like an outage...
	awaitLog(ctx, t, opts.LogPath, "the suspension a recoverable refusal opens", func(r logRecord) bool {
		return r.Operation == "production-suspended" && r.Level == "warn"
	})
	// ...and recovered, rather than parking the file on a verdict nobody gave.
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())
	for _, r := range logsForOperation(readLog(t, opts.LogPath), "producer-defect") {
		t.Errorf("a refusal carrying NO kind parked the file as a producer defect; a retry was never ruled out: %v", r.Context)
	}
}

// TestAParkedFileStaysParkedAcrossAStoreBounce asserts the park is a verdict
// about the FILE's bytes, not a mood of one store process.
//
// A parked file's batch can never be accepted — the same bytes yield the same
// batch — so a reader that quietly resumed it after the store came back would
// re-offer the identical refusal on every bounce, which is the loop R-S2
// forbids. The file's cursor must still be nowhere, and the defect still stated
// exactly once.
func TestAParkedFileStaysParkedAcrossAStoreBounce(t *testing.T) {
	t.Parallel()
	// Arrange: a store whose socket and database outlive the process, so the
	// second one is genuinely the same store — and still holds the decoy row
	// that makes the sidecar's batch a permanent producer defect.
	ctx, cancel := testContext(t)
	defer cancel()
	socket := shortSocketPath(t, "park-bounce")
	dbPath := filepath.Join(t.TempDir(), "store.db")
	store := startRealStoreAt(t, socket, dbPath)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	cwd := "/Users/dodgecoates/park-bounce-probe"
	session := "e6e6e6e6-e6e6-4e6e-8e6e-e6e6e6e6e6e6"
	opts := defaultSidecarOptions(t, socket, tree)

	seedDecoyRow(ctx, t, store.Client, "claude-shim:"+session,
		decoyEntry("activity:"+capturedThinking1, "decoy-bounce-"+capturedThinking1, "seeded-by-the-suite"))
	g := newGrowingFile(t, tree.sessionPath(cwdSlug(cwd), session))
	g.AppendLine(encodeRecord(t, retargetSession(t, decodeRecord(t, captured.Lines[7]), session, cwd)))

	// Act: park the file, bounce the store, and give the recovered store a
	// SECOND file whose ingest proves many cycles have run since.
	startSidecar(t, opts)
	awaitLog(ctx, t, opts.LogPath, "the producer-defect record", func(r logRecord) bool {
		return r.Operation == "producer-defect" && samePathAny(r.Context["path"], g.Path())
	})
	store.Stop()
	recovered := startRealStoreAt(t, socket, dbPath)

	otherCwd := "/Users/dodgecoates/park-bounce-other-probe"
	otherSession := "e7e7e7e7-e7e7-4e7e-8e7e-e7e7e7e7e7e7"
	other := newGrowingFile(t, tree.sessionPath(cwdSlug(otherCwd), otherSession))
	other.AppendLine(encodeRecord(t, retargetSession(t, decodeRecord(t, captured.Lines[12]), otherSession, otherCwd)))
	awaitCursorAtLeast(ctx, t, recovered.Client, other.Path(), other.Offset())

	// Assert: the parked file is still parked, and still stated once.
	if cs := cursorByPath(ctx, t, recovered.Client, g.Path()); cs != nil {
		t.Errorf("the parked file advanced a cursor to %d after a store bounce; a park is a verdict about its bytes, which a bounce does not change",
			cs.GetOffset())
	}
	var stated int
	for _, r := range logsForOperation(readLog(t, opts.LogPath), "producer-defect") {
		if samePathAny(r.Context["path"], g.Path()) {
			stated++
		}
	}
	if stated != 1 {
		t.Errorf("the producer defect was stated %d times across a store bounce, want exactly once", stated)
	}
}

// TestTwoLaunchesClaimingOneSpoolAttributeNothingAndSayWhy asserts a conflicted
// task resolves to NOTHING rather than to a guess.
//
// Two spawning calls naming one task id is a vendor state the reader cannot
// arbitrate: picking either one puts a run's output in another run's card, and
// there is no evidence that favors the first claim over the second. So the task
// is permanently unresolvable, the conflict is its own ERROR operation, and the
// spool is never read rather than attributed to a guess: no run claims it, so
// nothing renders it.
func TestTwoLaunchesClaimingOneSpoolAttributeNothingAndSayWhy(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	cwd := "/Users/dodgecoates/spool-conflict-probe"
	slug := cwdSlug(cwd)
	session := "21212121-2121-4121-8121-212121212121"
	spoolPath := tree.spoolPath(slug, session, capturedSpoolTask1)
	// The re-resolution the never-read half waits on is stated at DEBUG.
	opts := debugLogging(defaultSidecarOptions(t, fake.Socket, tree))
	opts.UnownedSpoolWindow = 200 * time.Millisecond

	// The SAME task id, launched by two different calls. Both pairs are the
	// captured launch, re-pointed; the second's call id is the transcript's
	// other Bash call, so the two claims are genuinely distinct.
	firstCall := retargetSession(t, decodeRecord(t, captured.Lines[8]), session, cwd)
	firstResult := retargetSession(t, decodeRecord(t, captured.Lines[10]), session, cwd)
	firstResult = setToolResultText(t, firstResult, backgroundLaunchText(capturedSpoolTask1, spoolPath))
	firstResult = setNested(t, firstResult, "toolUseResult", "backgroundTaskId", capturedSpoolTask1)

	secondCall := setToolUseID(t, firstCall, capturedBashCall2)
	secondResult := setToolUseID(t, firstResult, capturedBashCall2)

	// Act.
	startSidecar(t, opts)
	parent := newGrowingFile(t, tree.sessionPath(slug, session))
	parent.AppendLine(encodeRecord(t, firstCall))
	parent.AppendLine(encodeRecord(t, firstResult))
	parent.AppendLine(encodeRecord(t, secondCall))
	parent.AppendLine(encodeRecord(t, secondResult))
	rec := awaitLog(ctx, t, opts.LogPath, "the spawn-conflict record", func(r logRecord) bool {
		return r.Operation == "record-spawn-conflict"
	})

	// Assert: the conflict is loud and names the contested task...
	if rec.Level != "error" {
		t.Errorf("a task two calls claim was reported at %q; it makes the task permanently unattributable and is an error", rec.Level)
	}
	if got, _ := rec.Context["task_id"].(string); got != capturedSpoolTask1 {
		t.Errorf("the conflict record names task %q, wanted the contested task %q; without it the conflict is not investigable", got, capturedSpoolTask1)
	}

	// ...and the spool is attributed to NEITHER call: it is held, its window
	// lapses, and it is never read.
	spool := newGrowingFile(t, spoolPath)
	spool.AppendRaw([]byte("output nobody can be sure owns it\n"))
	awaitLog(ctx, t, opts.LogPath, "the contested spool's hold expiring", func(r logRecord) bool {
		return r.Operation == "hold-expired" && samePathAny(r.Context["path"], spoolPath)
	})
	lapsedAt := logIndexOf(t, opts.LogPath, func(r logRecord) bool {
		return r.Operation == "hold-expired" && samePathAny(r.Context["path"], spoolPath)
	})
	awaitRestatedAfter(ctx, t, opts.LogPath, spoolPath, "hold-spool", lapsedAt)
	requireNeverRead(t, fake, opts.LogPath, spoolPath)
	for _, run := range []string{capturedBashCall1, capturedBashCall2} {
		if frames := bashFramesForRun(fake.Entries(), run); len(frames) != 0 {
			t.Errorf("the contested spool was attributed to run %q anyway (%d frames); a conflicted task resolves to nothing, never to a guess",
				run, len(frames))
		}
	}
}

// TestAStopArrivingBeforeTheSpoolIsClaimedCancelsItOnClaim asserts a stop is
// remembered rather than dropped when it lands in the window where the spool
// has no owner yet.
//
// A spool is frequently written before the transcript line naming it, so the
// stop for a task can genuinely arrive while its file is still HELD. Dropping
// it there would leave the run open forever with no terminal; the reader keeps
// one pending stop per task and applies it the moment the spool is claimed.
func TestAStopArrivingBeforeTheSpoolIsClaimedCancelsItOnClaim(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	cwd := "/Users/dodgecoates/stop-before-claim-probe"
	slug := cwdSlug(cwd)
	session := "22222222-2222-4222-8222-222222222222"
	spoolPath := tree.spoolPath(slug, session, capturedSpoolTask1)
	opts := defaultSidecarOptions(t, fake.Socket, tree)
	// Long enough that the hold cannot lapse into residue while the subject is
	// arranging the claim: the point is the stop crossing an UNCLAIMED spool.
	opts.UnownedSpoolWindow = 30 * time.Second
	// THE PER-ITEM DETAIL LIVES AT DEBUG DURING CATCH-UP. This record is one of
	// the six corpus-walk operations the startup catch-up window levels (see
	// "Startup catch-up"), and this subject asserts the per-item record rather
	// than the summary, so it reads the log at the threshold the detail is
	// written to.
	opts = debugLogging(opts)

	stop := retargetTaskStop(t,
		retargetSession(t, decodeRecord(t, corpusLine(t, "tool-results/task_stop.jsonl", 0)), session, cwd),
		capturedSpoolTask1, "local_bash")
	stopCall := setToolUseID(t,
		renameToolUse(t, retargetSession(t, decodeRecord(t, captured.Lines[8]), session, cwd), "TaskStop"),
		toolUseIDOfResult(t, stop))

	launchCall := retargetSession(t, decodeRecord(t, captured.Lines[8]), session, cwd)
	launchResult := setNested(t,
		setToolResultText(t, retargetSession(t, decodeRecord(t, captured.Lines[10]), session, cwd),
			backgroundLaunchText(capturedSpoolTask1, spoolPath)),
		"toolUseResult", "backgroundTaskId", capturedSpoolTask1)

	// Act: the spool exists and is HELD; the stop lands while it is; only then
	// does the launch line name its owner.
	startSidecar(t, opts)
	spool := newGrowingFile(t, spoolPath)
	spool.AppendRaw([]byte("said this much before it was stopped\n"))
	awaitLog(ctx, t, opts.LogPath, "the spool being held", func(r logRecord) bool {
		return r.Operation == "hold-spool" && samePathAny(r.Context["path"], spoolPath)
	})

	parent := newGrowingFile(t, tree.sessionPath(slug, session))
	parent.AppendLine(encodeRecord(t, stopCall))
	parent.AppendLine(encodeRecord(t, stop))
	awaitLog(ctx, t, opts.LogPath, "the stop being remembered against the task", func(r logRecord) bool {
		return r.Operation == "task-stopped"
	})

	parent.AppendLine(encodeRecord(t, launchCall))
	parent.AppendLine(encodeRecord(t, launchResult))

	// Assert: the run settles interrupted by the user, carrying what it said.
	rows := awaitBashRunTerminal(ctx, t, storeClient(fake.Socket), capturedBashCall1)
	requireBashReplayOrder(t, capturedBashCall1, rows)
	interrupted := rows[len(rows)-1].GetSuccess().GetInterrupted()
	if interrupted == nil {
		t.Fatalf("a stop that arrived before the claim never cancelled the run; it ended on %v", describeBashRows(rows))
	}
	if interrupted.GetByUser() == nil {
		t.Errorf("the remembered stop settled with cause %v, wanted by_user; a TaskStop is a person's decision however late it is applied",
			interrupted.GetCause())
	}
	if want := requireLatestTail(t, capturedBashCall1, rows); interrupted.GetOutput().GetText().GetStdout() != want {
		t.Errorf("the cancelled terminal carries stdout %q, wanted exactly the run's joined deltas %q",
			interrupted.GetOutput().GetText().GetStdout(), want)
	}
}
