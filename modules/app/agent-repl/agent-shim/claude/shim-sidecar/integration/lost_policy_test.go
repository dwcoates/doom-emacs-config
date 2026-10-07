package integration

import (
	"context"
	"os"
	"path/filepath"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
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
// TWO SURFACES CARRY THE CONCLUSION, and they carry the SAME conclusion:
//
//   - THE WIRE owns it. AgentBash.success.interrupted carries the output so far
//     and cause = `lost`, whose DetachedLost arm names HOW we concluded it:
//     file_vanished, went_silent, swept_up (agent_activity.proto). It is NOT a
//     cause-less interrupted: an earlier reading of this contract held that
//     conversation.v1 had no DetachedLost message this wave, and that reading
//     was simply wrong — the message and all three of its arms are on the wire,
//     reached through AgentBashInterrupted.cause.lost, so a consumer can draw
//     "we stopped seeing it, this way" instead of an unexplained cut.
//   - THE LOG joins it back to the sweep, through the `reason` key on the
//     "lost-terminal" record — the same word the wire's arm carries.
//
// `by_user` and `timed_out` remain the two things a LOST terminal must never
// say: both name a decision, and the reader knows of neither.

// staleWindows are the windows every subject here runs with, spelled once. The
// window under test is short; the others are long enough that no OTHER arm can
// reach a conclusion first and steal the subject.
const (
	shortGrace   = 60 * time.Millisecond
	shortSilence = 150 * time.Millisecond
	// growthSilence is the shell-silence window of the ONE subject that must
	// KEEP a file alive across the window rather than let it fall silent.
	//
	// Every other short-window subject writes once and then waits for a
	// verdict, so a longer window only costs it patience. The growth subject
	// runs a loop — append, wait for the reader's cursor to acknowledge the
	// append — and it LOSES if any single iteration takes longer than the
	// window, which makes the window a bound on the harness's own latency
	// rather than on the policy. One iteration is a file write plus a store
	// round trip, measured at ~10ms quiet and ~150ms worst under this
	// package's own parallelism; 750ms is ~5x that worst case. It is the one
	// place in this file where the wall clock is INHERENT: the policy under
	// test is a silence window, and a subject about staying non-silent has to
	// stay non-silent for one.
	growthSilence = 750 * time.Millisecond
	longWindow    = 30 * time.Second
	fastRescan    = 25 * time.Millisecond
	fastPoll      = 20 * time.Millisecond
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
// THE REASON IS MATCHED ON ITS OWN KEY, not in the message text. It has one
// (`reason`), carrying the same word the wire's DetachedLost arm carries, so a
// terminal and the sweep that concluded it join on it — and a substring match
// against prose would go on passing after the arm itself regressed, which is
// exactly how the missing wire assertion survived.
func awaitLostConclusion(ctx context.Context, t *testing.T, logPath, path, reason string) logRecord {
	t.Helper()
	// ANY of the reader's four outcomes is the conclusion this waits for. They
	// are separate operations so a reader can tell a minted terminal from a
	// refused one; a wait that named only the minted one would hang forever on a
	// residue spool, which names no run and can never mint one.
	return awaitLog(ctx, t, logPath, "the "+reason+" conclusion for "+path, func(r logRecord) bool {
		return lostTerminalOperations[r.Operation] &&
			samePathAny(r.Context["path"], path) &&
			r.Context["reason"] == reason
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
		// The policy's own statement, and every outcome of the reader's attempt
		// to spell it as a terminal. Nothing else in `lost-policy` is a
		// conclusion: a settled run says it can no longer BE concluded, which is
		// the opposite — and the policy states its conclusions at WARN while its
		// ordinary bookkeeping is verbose, so the LEVEL separates them without
		// reading a sentence.
		concluded := (r.Operation == "lost-policy" && r.Level == "warn") ||
			lostTerminalOperations[r.Operation]
		if concluded {
			out = append(out, r)
		}
	}
	return out
}

// awaitInterruptedTerminal polls the run's own frames until one carries the
// interrupted terminal.
//
// IT READS THE RUN'S FRAMES, NOT A BOOK. A detached run's frames are
// StoreAgentUpdate.bash and are NEVER page lines — the spawning CALL is already
// the page line, which the detached-work subject pins explicitly — so a book
// read can never see this terminal, however long it waits. The wire the sidecar
// wrote is where the terminal is, and that is what this reads.
func awaitInterruptedTerminal(ctx context.Context, t *testing.T, f *fakeStore, run string) *conversationv1.AgentBashInterrupted {
	t.Helper()
	tick := time.NewTicker(pollTick)
	defer tick.Stop()
	for {
		for _, frame := range bashFramesForRun(f.Entries(), run) {
			if cut := frame.GetSuccess().GetInterrupted(); cut != nil {
				return cut
			}
		}
		select {
		case <-ctx.Done():
			t.Fatalf("the run %q never settled as interrupted within the deadline; runs seen: %v",
				run, runsSeen(f.Entries()))
		case <-tick.C:
		}
	}
}

// requireLostCause states what a LOST terminal's cause MUST and MUST NOT say.
//
// IT MUST SAY `lost`, NAMING THE ARM. A cause-less interrupted is an
// unexplained cut, and the wire has the vocabulary to do better: DetachedLost's
// three arms are exactly the reader's own conclusions. A terminal that leaves
// the cause unset throws away the only account of what was observed.
//
// IT MUST NOT SAY `by_user` OR `timed_out`. Both name a DECISION — a person
// stopped it, its own timeout cut it — and the reader knows of neither; it
// stopped seeing the file, which is not a claim about why the run ended.
func requireLostCause(t *testing.T, cut *conversationv1.AgentBashInterrupted, want string) {
	t.Helper()
	if cut.GetByUser() != nil {
		t.Errorf("the LOST terminal blames a person; we only stopped seeing the run")
	}
	if cut.GetTimedOut() != nil {
		t.Errorf("the LOST terminal blames a timeout; we only stopped seeing the run")
	}
	lost := cut.GetLost()
	if lost == nil {
		t.Fatalf("the LOST terminal states no cause at all; it must carry cause=lost naming HOW we concluded it (cause was %v)", cut.GetCause())
	}
	if got := lostArmName(lost); got != want {
		t.Errorf("the LOST terminal names the arm %q, wanted %q", got, want)
	}
}

// lostArmName spells a DetachedLost's arm in the reader's own vocabulary, which
// is the same word the log's `reason` key carries.
func lostArmName(lost *conversationv1.DetachedLost) string {
	switch {
	case lost.GetFileVanished() != nil:
		return "file_vanished"
	case lost.GetWentSilent() != nil:
		return "went_silent"
	case lost.GetSweptUp() != nil:
		return "swept_up"
	default:
		return "unset"
	}
}

// ---------------------------------------------------------------------------
// (a) file_vanished — the run's file disappeared under the reader.
// ---------------------------------------------------------------------------

// TestAVanishedSpoolIsConcludedFileVanished asserts the reason: the grace window
// absorbs the ordinary rename race, and past it the disappearance IS the
// conclusion.
func TestAVanishedSpoolIsConcludedFileVanished(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	store := startRealStore(t)
	tree := newVendorTree(t)
	fx := seedDetachedShell(t, tree, "/work/lost-vanished-probe",
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
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	session := "a2a2a2a2-a2a2-4a2a-8a2a-a2a2a2a2a2a2"
	fx := seedDetachedShell(t, tree, "/work/lost-vanished-terminal-probe", session)
	opts := lostOptions(t, fake.Socket, tree)
	opts.StaleGrace = shortGrace

	// Act.
	startSidecar(t, opts)
	awaitCursorInBatches(ctx, t, fake, fx.Parent.Path(), fx.Parent.Offset())
	spool := newGrowingFile(t, fx.SpoolPath)
	spool.AppendRaw([]byte("all it managed to say\n"))
	awaitCursorInBatches(ctx, t, fake, fx.SpoolPath, spool.Offset())
	spool.Remove()

	// Assert.
	cut := awaitInterruptedTerminal(ctx, t, fake, fx.CallID)
	requireLostCause(t, cut, "file_vanished")
	// THE TERMINAL OWES EXACTLY THE RUN'S DELTAS, byte for byte: a reader that
	// concatenates the deltas and a reader that takes the terminal's stdout must
	// see the same output, and a containment check could not tell those apart.
	wantStdout := requireLatestTail(t, fx.CallID, bashFramesForRun(fake.Entries(), fx.CallID))
	if got := cut.GetOutput().GetText().GetStdout(); got != wantStdout {
		t.Errorf("the LOST terminal carries stdout %q, wanted exactly the run's joined deltas %q", got, wantStdout)
	}
}

// ---------------------------------------------------------------------------
// (b) went_silent — the file is still there and stopped growing.
// ---------------------------------------------------------------------------

// TestASpoolThatStopsGrowingIsConcludedWentSilent asserts the reason for a file
// that is still present: no EXIT marker ever arrived, and its silence outlasted
// the shell window.
func TestASpoolThatStopsGrowingIsConcludedWentSilent(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	store := startRealStore(t)
	tree := newVendorTree(t)
	fx := seedDetachedShell(t, tree, "/work/lost-silent-probe",
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
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	session := "b2b2b2b2-b2b2-4b2b-8b2b-b2b2b2b2b2b2"
	fx := seedDetachedShell(t, tree, "/work/lost-silent-terminal-probe", session)
	opts := lostOptions(t, fake.Socket, tree)
	opts.StaleShellSilence = shortSilence

	// Act.
	startSidecar(t, opts)
	awaitCursorInBatches(ctx, t, fake, fx.Parent.Path(), fx.Parent.Offset())
	spool := newGrowingFile(t, fx.SpoolPath)
	spool.AppendRaw([]byte("started, then stopped saying anything\n"))
	awaitCursorInBatches(ctx, t, fake, fx.SpoolPath, spool.Offset())

	// Assert.
	cut := awaitInterruptedTerminal(ctx, t, fake, fx.CallID)
	requireLostCause(t, cut, "went_silent")
	wantStdout := requireLatestTail(t, fx.CallID, bashFramesForRun(fake.Entries(), fx.CallID))
	if got := cut.GetOutput().GetText().GetStdout(); got != wantStdout {
		t.Errorf("the LOST terminal carries stdout %q, wanted exactly the run's joined deltas %q", got, wantStdout)
	}
}

// ---------------------------------------------------------------------------
// (c) a spool nobody ever claimed, whose file predates the reboot.
// ---------------------------------------------------------------------------
//
// swept_up for a CLAIMED pre-boot spool is on the wire (lost_wire_test.go). An
// UNCLAIMED one names no run and nothing renders it, so it is never read, never
// tracked, and never concluded: a LOST verdict for it would settle nothing.

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

// TestAPreBootUnclaimedSpoolIsNeverRead asserts that concluding nothing about a
// spool nobody claimed is not a licence to read it either: its bytes are never
// read, and no verdict is stated for it.
func TestAPreBootUnclaimedSpoolIsNeverRead(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	opts := debugLogging(lostOptions(t, fake.Socket, tree))
	opts.UnownedSpoolWindow = time.Millisecond
	spoolPath := seedPreBootSpool(t, tree, "/work/lost-swept-residue-probe",
		"c2c2c2c2-c2c2-4c2c-8c2c-c2c2c2c2c2c2", "output from before the reboot\n")

	// Act.
	startSidecar(t, opts)
	awaitCatchupEnd(ctx, t, opts.LogPath)
	endAt := logIndexOf(t, opts.LogPath, func(r logRecord) bool { return r.Operation == "catchup-end" })
	awaitRestatedAfter(ctx, t, opts.LogPath, spoolPath, "hold-spool", endAt)

	// Assert.
	requireNeverRead(t, fake, opts.LogPath, spoolPath)
	if got := lostConclusions(t, opts.LogPath, spoolPath); len(got) != 0 {
		t.Fatalf("a spool nothing claimed was concluded LOST %d time(s); it names no run: %v", len(got), got)
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
// THE FENCE IS ANOTHER FILE, not elapsed time. A second run, launched in the
// same session and so claimed and read like the subject's own, is left silent
// after one line; the moment the reader concludes THAT one LOST we know the
// sweep has run under the same short window, so the growing spool's having no
// conclusion is a decision rather than an absence of one.
func TestASpoolThatKeepsGrowingIsNeverConcludedLost(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	store := startRealStore(t)
	tree := newVendorTree(t)
	session := "d1d1d1d1-d1d1-4d1d-8d1d-d1d1d1d1d1d1"
	fx := seedDetachedShell(t, tree, "/work/lost-growing-probe", session)
	opts := lostOptions(t, store.Socket, tree)
	opts.StaleShellSilence = growthSilence
	// The fence: a second claimed run that says one line and goes silent, so it
	// concludes under the same window.
	fencePath := appendDetachedLaunch(t, fx, "b0fence", capturedBashCall2)

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
