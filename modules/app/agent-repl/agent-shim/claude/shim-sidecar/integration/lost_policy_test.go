package integration

import (
	"context"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
)

// SUBJECT — the LOST policy, exercised end to end against the real sidecar.
//
// LOST IS ITS OWN WORD: it means "we stopped seeing it", never "we know it
// failed". The reader has no view of process liveness, so the most it can ever
// say about a detached run that went quiet is HOW it stopped seeing it, and the
// arm IS that account: file_vanished, went_silent, swept_up.
//
// THE WINDOWS ARE THE SUBJECT, WHICH IS WHY THEY ARE FLAGS. A production grace
// of 30s and a shell-silence window of 30m cannot be waited out, and waiting is
// not what would be tested anyway: what is tested is that the conclusion is
// reached, named, and carried to the run's terminal. So the sidecar runs with
// windows of tens of milliseconds and every wait here is a bounded receive on a
// real signal — a store read, a cursor advance, a line in the log — never a
// sleep. There is no time.Sleep in this file.
//
// TWO SURFACES CARRY THE CONCLUSION, and each subject asserts exactly one:
//
//   - THE LOG owns the REASON. store.v1 and conversation.v1 have no DetachedLost
//     message this wave, so how we stopped seeing the run survives only in the
//     record the reader writes (operation "lost-terminal").
//   - THE WIRE owns the TERMINAL: AgentBash.success.interrupted carrying the
//     output so far and NO cause arm, because neither a person nor a timeout cut
//     this run — we merely stopped seeing it.

// staleWindows are the windows every subject here runs with, spelled once. The
// window under test is short; the others are long enough that no OTHER arm can
// reach a conclusion first and steal the subject.
const (
	shortGrace   = 60 * time.Millisecond
	shortSilence = 150 * time.Millisecond
	longWindow   = 30 * time.Second
	fastRescan   = 25 * time.Millisecond
	fastPoll     = 20 * time.Millisecond
)

// lostOptions builds sidecar options whose LOST windows are all long, so a
// subject can shorten exactly the one it is about.
func lostOptions(t *testing.T, storeSocket string, tree *vendorTree) sidecarOptions {
	t.Helper()
	opts := defaultSidecarOptions(t, storeSocket, tree)
	opts.PollInterval = fastPoll
	opts.RescanEvery = fastRescan
	opts.StaleGrace = longWindow
	opts.StaleShellSilence = longWindow
	opts.StaleAgentSilence = longWindow
	opts.StaleWorkflowSilence = longWindow
	return opts
}

// awaitLostConclusion waits for the reader's own statement about one file: the
// operation is "lost-terminal" and the record names the reason it concluded.
//
// The reason is matched in the message text because it has no context key of
// its own — there is no DetachedLost on the wire and no correlation key for a
// reason, and inventing either here would be inventing contract.
func awaitLostConclusion(ctx context.Context, t *testing.T, logPath, path, reason string) logRecord {
	t.Helper()
	return awaitLog(ctx, t, logPath, "the "+reason+" conclusion for "+path, func(r logRecord) bool {
		return r.Operation == "lost-terminal" &&
			samePathAny(r.Context["path"], path) &&
			strings.Contains(r.Message, reason)
	})
}

// lostConclusions lists every conclusion the reader has stated for one file so
// far, which is how a subject asserts that NONE was reached.
func lostConclusions(t *testing.T, logPath, path string) []logRecord {
	t.Helper()
	var out []logRecord
	for _, r := range readLog(t, logPath) {
		if !samePathAny(r.Context["path"], path) {
			continue
		}
		// The policy's own statement, and the reader's attempt to spell it as a
		// terminal. Nothing else in either operation is a conclusion: a settled
		// run says it can no longer BE concluded, which is the opposite.
		concluded := r.Operation == "lost-terminal" ||
			(r.Operation == "lost-policy" && strings.Contains(r.Message, "concluded LOST reason="))
		if concluded {
			out = append(out, r)
		}
	}
	return out
}

// awaitInterruptedTerminal polls one book until the detached run's unit carries
// the interrupted terminal. The store read is the signal; the ticker paces it.
func awaitInterruptedTerminal(ctx context.Context, t *testing.T, store *realStore, book, run string) *conversationv1.AgentBashInterrupted {
	t.Helper()
	tick := time.NewTicker(pollTick)
	defer tick.Stop()
	for {
		for _, at := range bookLines(ctx, t, store.Client, book, 200) {
			activity := activityOf(at.GetLine())
			if activity == nil || activity.GetActivityId().GetValue() != run {
				continue
			}
			if cut := activity.GetBash().GetSuccess().GetInterrupted(); cut != nil {
				return cut
			}
		}
		select {
		case <-ctx.Done():
			t.Fatalf("the run %q in book %q never settled as interrupted within the deadline", run, book)
		case <-tick.C:
		}
	}
}

// requireNoCause states the one thing the interrupted arm must NOT say. A cause
// names a decision — a person stopped it, its own timeout cut it — and the
// reader knows neither: it stopped seeing the file, which is not a claim about
// why the run ended.
func requireNoCause(t *testing.T, cut *conversationv1.AgentBashInterrupted) {
	t.Helper()
	if cut.GetByUser() != nil {
		t.Errorf("the LOST terminal blames a person; we only stopped seeing the run")
	}
	if cut.GetTimedOut() != nil {
		t.Errorf("the LOST terminal blames a timeout; we only stopped seeing the run")
	}
}

// ---------------------------------------------------------------------------
// (a) file_vanished — the run's file disappeared under the reader.
// ---------------------------------------------------------------------------

// TestAVanishedSpoolIsConcludedFileVanished asserts the reason: the grace window
// absorbs the ordinary rename race, and past it the disappearance IS the
// conclusion.
func TestAVanishedSpoolIsConcludedFileVanished(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	store := startRealStore(t)
	tree := newVendorTree(t)
	fx := seedDetachedShell(t, tree, "/Users/dodgecoates/lost-vanished-probe",
		"a1a1a1a1-a1a1-4a1a-8a1a-a1a1a1a1a1a1")
	opts := lostOptions(t, store.Socket, tree)
	opts.StaleGrace = shortGrace

	// Act: the spool is read, then deleted under the reader.
	startSidecar(t, opts)
	awaitCursorAtLeast(ctx, t, store.Client, fx.Parent.Path(), fx.Parent.Offset())
	spool := newGrowingFile(t, fx.SpoolPath)
	spool.AppendRaw([]byte("all it managed to say\n"))
	awaitCursorAtLeast(ctx, t, store.Client, fx.SpoolPath, spool.Offset())
	spool.Remove()

	// Assert.
	awaitLostConclusion(ctx, t, opts.LogPath, fx.SpoolPath, "file_vanished")
}

// TestAVanishedSpoolSettlesItsRunAsInterrupted asserts the terminal: the run is
// closed with the output it managed to produce, under the interrupted arm.
func TestAVanishedSpoolSettlesItsRunAsInterrupted(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	store := startRealStore(t)
	tree := newVendorTree(t)
	session := "a2a2a2a2-a2a2-4a2a-8a2a-a2a2a2a2a2a2"
	fx := seedDetachedShell(t, tree, "/Users/dodgecoates/lost-vanished-terminal-probe", session)
	opts := lostOptions(t, store.Socket, tree)
	opts.StaleGrace = shortGrace

	// Act.
	startSidecar(t, opts)
	awaitCursorAtLeast(ctx, t, store.Client, fx.Parent.Path(), fx.Parent.Offset())
	spool := newGrowingFile(t, fx.SpoolPath)
	spool.AppendRaw([]byte("all it managed to say\n"))
	awaitCursorAtLeast(ctx, t, store.Client, fx.SpoolPath, spool.Offset())
	spool.Remove()

	// Assert.
	cut := awaitInterruptedTerminal(ctx, t, store, session, fx.CallID)
	requireNoCause(t, cut)
	if got := cut.GetOutput().GetText().GetStdout(); !strings.Contains(got, "all it managed to say") {
		t.Errorf("the LOST terminal carries stdout %q, wanted the output the run had produced", got)
	}
}

// ---------------------------------------------------------------------------
// (b) went_silent — the file is still there and stopped growing.
// ---------------------------------------------------------------------------

// TestASpoolThatStopsGrowingIsConcludedWentSilent asserts the reason for a file
// that is still present: no EXIT marker ever arrived, and its silence outlasted
// the shell window.
func TestASpoolThatStopsGrowingIsConcludedWentSilent(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	store := startRealStore(t)
	tree := newVendorTree(t)
	fx := seedDetachedShell(t, tree, "/Users/dodgecoates/lost-silent-probe",
		"b1b1b1b1-b1b1-4b1b-8b1b-b1b1b1b1b1b1")
	opts := lostOptions(t, store.Socket, tree)
	opts.StaleShellSilence = shortSilence

	// Act: bytes, then nothing — and no `EXIT=` line to settle it.
	startSidecar(t, opts)
	awaitCursorAtLeast(ctx, t, store.Client, fx.Parent.Path(), fx.Parent.Offset())
	spool := newGrowingFile(t, fx.SpoolPath)
	spool.AppendRaw([]byte("started, then stopped saying anything\n"))
	awaitCursorAtLeast(ctx, t, store.Client, fx.SpoolPath, spool.Offset())

	// Assert.
	awaitLostConclusion(ctx, t, opts.LogPath, fx.SpoolPath, "went_silent")
}

// TestASilentSpoolSettlesItsRunAsInterrupted asserts the same terminal reaches
// the wire for a run that merely went quiet.
func TestASilentSpoolSettlesItsRunAsInterrupted(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	store := startRealStore(t)
	tree := newVendorTree(t)
	session := "b2b2b2b2-b2b2-4b2b-8b2b-b2b2b2b2b2b2"
	fx := seedDetachedShell(t, tree, "/Users/dodgecoates/lost-silent-terminal-probe", session)
	opts := lostOptions(t, store.Socket, tree)
	opts.StaleShellSilence = shortSilence

	// Act.
	startSidecar(t, opts)
	awaitCursorAtLeast(ctx, t, store.Client, fx.Parent.Path(), fx.Parent.Offset())
	spool := newGrowingFile(t, fx.SpoolPath)
	spool.AppendRaw([]byte("started, then stopped saying anything\n"))
	awaitCursorAtLeast(ctx, t, store.Client, fx.SpoolPath, spool.Offset())

	// Assert.
	cut := awaitInterruptedTerminal(ctx, t, store, session, fx.CallID)
	requireNoCause(t, cut)
	if got := cut.GetOutput().GetText().GetStdout(); !strings.Contains(got, "started, then stopped") {
		t.Errorf("the LOST terminal carries stdout %q, wanted the output the run had produced", got)
	}
}

// ---------------------------------------------------------------------------
// (c) swept_up — a spool nobody ever claimed, whose file predates the reboot.
// ---------------------------------------------------------------------------

// preBootStamp is a time no machine's boot can be older than, which is what
// makes a fixture's file "not touched since before the reboot" without the test
// knowing when this machine actually booted.
var preBootStamp = time.Date(2001, 1, 1, 0, 0, 0, 0, time.UTC)

// seedPreBootSpool writes an unclaimed spool whose bytes predate the reboot: no
// transcript ever names it, so no owner will ever appear for it.
func seedPreBootSpool(t *testing.T, tree *vendorTree, cwd, session, payload string) string {
	t.Helper()
	path := tree.spoolPath(cwdSlug(cwd), session, capturedSpoolTask1)
	mustMkdirAll(t, filepath.Dir(path))
	if err := os.WriteFile(path, []byte(payload), 0o644); err != nil {
		t.Fatalf("write %s: %v", path, err)
	}
	// Stamped rather than grown: an appended byte would make the file look
	// alive, and the whole point of this fixture is that it has not been
	// written since before the machine booted.
	if err := os.Chtimes(path, preBootStamp, preBootStamp); err != nil {
		t.Fatalf("stamp %s: %v", path, err)
	}
	return path
}

// TestAPreBootUnclaimedSpoolIsConcludedSweptUp asserts the reason: nothing
// survives a reboot, so a run whose file has not been written since before the
// machine booted was never going to report again.
func TestAPreBootUnclaimedSpoolIsConcludedSweptUp(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	opts := lostOptions(t, fake.Socket, tree)
	// The hold is what keeps an unclaimed spool from being tailed at all, so it
	// is shortened too: the subject is the conclusion, not the wait.
	opts.UnownedSpoolWindow = time.Millisecond
	spoolPath := seedPreBootSpool(t, tree, "/Users/dodgecoates/lost-swept-probe",
		"c1c1c1c1-c1c1-4c1c-8c1c-c1c1c1c1c1c1", "output from before the reboot\n")

	// Act.
	startSidecar(t, opts)

	// Assert.
	awaitLostConclusion(ctx, t, opts.LogPath, spoolPath, "swept_up")
}

// TestAPreBootUnclaimedSpoolsBytesStillLandAsResidue asserts the other half:
// concluding a run LOST is never a licence to drop what is on disk. Nobody
// claimed the spool, so its bytes land as unparsed residue naming it as their
// source.
func TestAPreBootUnclaimedSpoolsBytesStillLandAsResidue(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	opts := lostOptions(t, fake.Socket, tree)
	opts.UnownedSpoolWindow = time.Millisecond
	payload := "output from before the reboot\n"
	spoolPath := seedPreBootSpool(t, tree, "/Users/dodgecoates/lost-swept-residue-probe",
		"c2c2c2c2-c2c2-4c2c-8c2c-c2c2c2c2c2c2", payload)

	// Act.
	startSidecar(t, opts)
	fake.awaitEntry(ctx, t, "residue naming the swept spool", func(e *storev1.StoreEntry) bool {
		u := e.GetAgentUpdate().GetUnservedItem().GetUnparsed()
		return u != nil && samePath(u.GetSource(), spoolPath)
	})

	// Assert.
	var found bool
	for _, residue := range unparsedOf(fake.Entries()) {
		if !samePath(residue.GetSource(), spoolPath) {
			continue
		}
		found = true
		if !strings.Contains(residue.GetRaw(), strings.TrimSpace(payload)) {
			t.Errorf("residue for %s carries %q, wanted the file's bytes %q", spoolPath, residue.GetRaw(), payload)
		}
	}
	if !found {
		t.Fatalf("the swept spool's bytes never landed as residue; a LOST conclusion is not a licence to drop them")
	}
}

// ---------------------------------------------------------------------------
// (d) a run that is still growing is NOT lost.
// ---------------------------------------------------------------------------

// TestASpoolThatKeepsGrowingIsNeverConcludedLost asserts the negative the whole
// policy rests on: growth is the only evidence of liveness a file reader has,
// and a file that keeps producing it is never concluded LOST however short the
// window is.
//
// THE FENCE IS ANOTHER FILE, not elapsed time. A second, unclaimed spool is
// left silent from the start; the moment the reader concludes THAT one LOST we
// know the sweep has run under the same short window, so the growing spool's
// having no conclusion is a decision rather than an absence of one.
func TestASpoolThatKeepsGrowingIsNeverConcludedLost(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	store := startRealStore(t)
	tree := newVendorTree(t)
	session := "d1d1d1d1-d1d1-4d1d-8d1d-d1d1d1d1d1d1"
	fx := seedDetachedShell(t, tree, "/Users/dodgecoates/lost-growing-probe", session)
	opts := lostOptions(t, store.Socket, tree)
	opts.StaleShellSilence = shortSilence
	// The fence: an unclassifiable spool is tailed at once and claimed by
	// nobody, so it goes silent immediately and concludes under the same window.
	fencePath := tree.spoolPath(cwdSlug("/Users/dodgecoates/lost-growing-probe"), session, "z0uncla551f1able")

	// Act.
	startSidecar(t, opts)
	awaitCursorAtLeast(ctx, t, store.Client, fx.Parent.Path(), fx.Parent.Offset())
	fence := newGrowingFile(t, fencePath)
	fence.AppendRaw([]byte("said once and never again\n"))
	spool := newGrowingFile(t, fx.SpoolPath)
	for chunk := 0; ; chunk++ {
		spool.AppendRaw([]byte("still going\n"))
		awaitCursorAtLeast(ctx, t, store.Client, fx.SpoolPath, spool.Offset())
		if len(lostConclusions(t, opts.LogPath, fencePath)) > 0 {
			break
		}
		select {
		case <-ctx.Done():
			t.Fatalf("the silent fence spool was never concluded LOST after %d growth rounds, so this subject proved nothing", chunk+1)
		default:
		}
	}

	// Assert.
	if stated := lostConclusions(t, opts.LogPath, fx.SpoolPath); len(stated) != 0 {
		t.Fatalf("the growing spool was concluded LOST while it was still producing output: %+v", stated)
	}
}
