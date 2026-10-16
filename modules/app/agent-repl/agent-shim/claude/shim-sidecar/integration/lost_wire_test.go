package integration

import (
	"context"
	"os"
	"path/filepath"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
)

// SUBJECT — the LOST conclusion AS A FACT ON THE WIRE.
//
// lost_policy_test.go asserts that a conclusion is REACHED and how the reader
// states it in its log. This file asserts the other half, which had no coverage
// at all: that the conclusion reaches a CONSUMER. conversation.v1 carries
// DetachedLost with exactly the reader's three arms, reached through
// AgentBashInterrupted.cause.lost, so "we stopped seeing it, this way" is
// something a reader of the conversation can draw — and until it is asserted,
// a regression that dropped the arm would leave every lost run showing as an
// unexplained cut with the suite still green.

// awaitLostTerminalOnTheWire follows a run's frames until one carries the
// interrupted terminal, and answers its `lost` cause.
//
// IT DEMANDS THE CAUSE. A terminal with the cause unset is exactly the
// regression this file exists to catch, so an interrupted frame without one is
// not an answer to wait past — it is the failure, stated as such.
func awaitLostTerminalOnTheWire(ctx context.Context, t *testing.T, f *fakeStore, run string) *conversationv1.DetachedLost {
	t.Helper()
	cut := awaitInterruptedTerminal(ctx, t, f, run)
	lost := cut.GetLost()
	if lost == nil {
		t.Fatalf("run %s settled interrupted with cause %v; a LOST run must carry cause=lost naming HOW we concluded it", run, cut.GetCause())
	}
	return lost
}

// TestALostTerminalNamesHowItWasConcludedOnTheWire is a TABLE over the two arms
// a live sidecar can be driven to conclude, each through its own evidence.
//
// THE ARM AND THE LOG MUST AGREE. They are two statements of one conclusion,
// and a reader joins them on the `reason` key — so the subject asserts the pair
// rather than either alone: an arm that disagreed with the record that produced
// it would be worse than a missing arm, because it would look authoritative.
func TestALostTerminalNamesHowItWasConcludedOnTheWire(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name string
		// arm is the DetachedLost arm the wire must carry, and it is the same
		// word the log's `reason` key must carry.
		arm string
		// tune shortens exactly the window this evidence runs out of.
		tune func(*sidecarOptions)
		// provoke produces the evidence, given the spool the run is writing.
		provoke func(t *testing.T, spool *growingFile)
	}{
		{
			name: "the file disappeared under the reader",
			arm:  "file_vanished",
			tune: func(o *sidecarOptions) { o.StaleGrace = shortGrace },
			// A REMOVED FILE, not a truncated one: the grace window exists to
			// absorb the ordinary rename race, and past it the disappearance
			// itself is the whole conclusion.
			provoke: func(t *testing.T, spool *growingFile) { spool.Remove() },
		},
		{
			name: "the file is still there and stopped growing",
			arm:  "went_silent",
			tune: func(o *sidecarOptions) { o.StaleShellSilence = shortSilence },
			// Nothing at all: silence IS the evidence, and the file staying on
			// disk is what makes this a different arm from the one above.
			provoke: func(t *testing.T, spool *growingFile) {},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			ctx, cancel := testContext(t)
			defer cancel()
			fake := startFakeStore(t)
			tree := newVendorTree(t)
			fx := seedDetachedShell(t, tree, "/work/lost-wire-"+tc.arm,
				sessionUUIDFor(tc.arm))
			opts := lostOptions(t, fake.Socket, tree)
			tc.tune(&opts)

			// Act.
			startSidecar(t, opts)
			awaitCursorInBatches(ctx, t, fake, fx.Parent.Path(), fx.Parent.Offset())
			spool := newGrowingFile(t, fx.SpoolPath)
			spool.AppendRaw([]byte("what it managed to say\n"))
			awaitCursorInBatches(ctx, t, fake, fx.SpoolPath, spool.Offset())
			tc.provoke(t, spool)

			// Assert: the wire names the arm...
			lost := awaitLostTerminalOnTheWire(ctx, t, fake, fx.CallID)
			if got := lostArmName(lost); got != tc.arm {
				t.Errorf("the wire names the arm %q, wanted %q", got, tc.arm)
			}
			// ...and the reader's own record names the same word, on its own
			// key, so the two can be joined.
			rec := awaitLostConclusion(ctx, t, opts.LogPath, fx.SpoolPath, tc.arm)
			if got := rec.Context["reason"]; got != tc.arm {
				t.Errorf("the lost-terminal record's reason key is %v, wanted %q", got, tc.arm)
			}
		})
	}
}

// sessionUUIDFor spells a distinct session uuid per table case, so two cases of
// one table never share a vendor tree's session directory.
func sessionUUIDFor(arm string) string {
	switch arm {
	case "file_vanished":
		return "e1e1e1e1-e1e1-4e1e-8e1e-e1e1e1e1e1e1"
	default:
		return "e2e2e2e2-e2e2-4e2e-8e2e-e2e2e2e2e2e2"
	}
}

// TestAClaimedPreBootSpoolSettlesSweptUpOnTheWire is the arm that had NO wire
// coverage at all.
//
// swept_up was only ever produced for an UNCLAIMED spool — a residue spool
// naming no run, which seam.go refuses a terminal for on purpose, because there
// is no unit to settle. So the arm reached the log and never once reached the
// wire, and nothing would have noticed if it could not. THIS spool is CLAIMED:
// its launch pair is in the transcript, so it names a run, and its terminal is
// owed.
//
// THE FILE PREDATES THE REBOOT, which is the whole evidence: nothing survives a
// reboot, so a run whose file has not been written since before the machine
// booted was never going to report again. It is asserted BEFORE any silence
// window can expire, because a went_silent verdict reaching the wire first
// would satisfy a laxer subject while proving nothing about swept_up.
func TestAClaimedPreBootSpoolSettlesSweptUpOnTheWire(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	session := "e3e3e3e3-e3e3-4e3e-8e3e-e3e3e3e3e3e3"
	fx := seedDetachedShell(t, tree, "/work/lost-swept-wire-probe", session)
	// The spool the transcript already names, written and then stamped back
	// before any possible boot. Stamped rather than grown: an appended byte
	// would make the file look alive, which is the opposite of the fixture.
	mustMkdirAll(t, filepath.Dir(fx.SpoolPath))
	if err := os.WriteFile(fx.SpoolPath, []byte("output from before the reboot\n"), 0o644); err != nil {
		t.Fatalf("write %s: %v", fx.SpoolPath, err)
	}
	if err := os.Chtimes(fx.SpoolPath, preBootStamp, preBootStamp); err != nil {
		t.Fatalf("stamp %s: %v", fx.SpoolPath, err)
	}
	opts := lostOptions(t, fake.Socket, tree)
	// EVERY silence window stays long, so went_silent cannot reach a verdict
	// first and steal the subject. Only the boot sweep can conclude here.
	//
	// THE UNOWNED WINDOW NO LONGER DECIDES ANYTHING HERE. It used to: a rescan
	// that resolved the spool before reading the transcript demoted a claimed
	// spool to residue, which names no run and is owed no terminal. A lapsed
	// hold now leaves the spool unread and claimable, so the launch claims it
	// whichever order discovery takes. The long window is kept as the default
	// this subject was tuned under.
	opts.UnownedSpoolWindow = longWindow

	// Act.
	startSidecar(t, opts)

	// Assert.
	lost := awaitLostTerminalOnTheWire(ctx, t, fake, fx.CallID)
	if lost.GetSweptUp() == nil {
		t.Fatalf("the pre-boot run settled on the arm %q, wanted swept_up", lostArmName(lost))
	}
}

// TestBytesAppendedAfterAWentSilentVerdictStillLand asserts that a verdict is
// not a licence to stop reading.
//
// went_silent's file IS STILL ON DISK, and the reader deliberately keeps its
// tailer for exactly this reason — a run we stopped hearing from may start
// talking again, and dropping the tailer on the verdict would silently discard
// everything it said afterwards. (file_vanished is the opposite case and drops
// its tailer, because a file that is gone will not say anything more.)
func TestBytesAppendedAfterAWentSilentVerdictStillLand(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	session := "e6e6e6e6-e6e6-4e6e-8e6e-e6e6e6e6e6e6"
	fx := seedDetachedShell(t, tree, "/work/lost-resumed-probe", session)
	opts := lostOptions(t, fake.Socket, tree)
	opts.StaleShellSilence = shortSilence

	// Act.
	startSidecar(t, opts)
	awaitCursorInBatches(ctx, t, fake, fx.Parent.Path(), fx.Parent.Offset())
	spool := newGrowingFile(t, fx.SpoolPath)
	spool.AppendRaw([]byte("before the silence\n"))
	awaitCursorInBatches(ctx, t, fake, fx.SpoolPath, spool.Offset())
	// The VERDICT is the signal to append past: waiting on it is what makes
	// the second write genuinely after the conclusion.
	awaitLostConclusion(ctx, t, opts.LogPath, fx.SpoolPath, "went_silent")
	spool.AppendRaw([]byte("and it spoke again after all\n"))

	// Assert: the later bytes reach the store.
	awaitCursorInBatches(ctx, t, fake, fx.SpoolPath, spool.Offset())
	// EXACT EQUALITY of the joined deltas, which is also what proves the earlier
	// bytes were not RE-EMITTED alongside the later ones: a containment check
	// passed just as happily on a delta stream that had replayed the whole file.
	joined := requireLatestTail(t, fx.CallID, bashFramesForRun(fake.Entries(), fx.CallID))
	if want := "before the silence\nand it spoke again after all\n"; joined != want {
		t.Fatalf("the run's deltas joined to %q, wanted exactly the bytes written either side of the verdict, %q", joined, want)
	}
}
