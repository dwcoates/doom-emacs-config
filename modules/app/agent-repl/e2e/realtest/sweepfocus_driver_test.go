//go:build realtest

package realtest

import (
	"context"
	"os"
	"testing"
	"time"
)

// THE SWEEP'S FOCUS, DRIVEN FROM bin/realtest.sh.
//
// `TestSweepFocusTake` runs BEFORE the first realtest and brings Emacs forward
// for the whole run; `TestSweepFocusGiveBack` runs from the sweep's EXIT trap
// and returns focus to the application that had it. sweepfocus.go carries the
// ruling and the reasoning.
//
// NEITHER IS A REALTEST, and neither may be named like one: bin/test-realtest.sh
// asserts that every `func TestRealtest*` here has a row in the runner's world
// table, and docs/REALTEST-PLAN.md is the contract for which realtests exist.
//
// NEITHER FAILS THE SWEEP FOR A DESKTOP THAT WOULD NOT COOPERATE. A declined
// activation is REPORTED — by the take's own note, and by every press's receipt
// after it — and the sweep runs anyway, because refusing to collect a run's
// findings over the window server's mood would throw away the whole point of
// the run. What DOES fail is this harness failing to do its part: a helper that
// would not compile, a token that could not be written, a token the handback
// could not read. Those leave the owner's desktop parked on Emacs with nobody
// holding the handback, and the operator has to be told.

// sweepFocusSettleCeiling bounds one half of the sweep's focus handling.
//
// The helper's own wait is two accessibility messaging timeouts (2s) plus the
// compile that precedes it, which KeyDriver.Build already bounds at two
// minutes. This is the outer bound on the whole call and is deliberately well
// above both: a ceiling that fires here turns a readable "the activation was
// declined" into an unreadable timeout.
const sweepFocusSettleCeiling = 3 * time.Minute

func TestSweepFocusTake(t *testing.T) {
	if os.Getenv(sweepFocusEnv) != sweepFocusTake {
		t.Skipf("the sweep's focus take runs only from bin/realtest.sh, which sets %s=%s",
			sweepFocusEnv, sweepFocusTake)
	}
	ctx, cancel := context.WithTimeout(context.Background(), sweepFocusSettleCeiling)
	defer cancel()

	runDir := os.Getenv(outEnv)
	if runDir == "" {
		t.Fatalf("%s is not set; bin/realtest.sh exports the run directory and the focus token lands in it",
			outEnv)
	}
	driver := &KeyDriver{Scratch: runDir}
	if err := driver.Build(ctx); err != nil {
		t.Fatalf("build the key helper to take the sweep's focus: %v", err)
	}

	// THE EDITOR MAY NOT EXIST YET, and that is the ordinary case: a sweep
	// whose first realtest cold-starts Emacs has nothing to bring forward.
	// Where one IS answering, it comes forward now so the owner watches the
	// run from its first moment.
	pid := sweepFocusEmacsPid(ctx, t, runDir)
	taken, err := driver.TakeSweepFocus(ctx, pid)
	if err != nil {
		t.Fatalf("take focus for the sweep: %v", err)
	}
	path, err := WriteSweepFocusToken(runDir, taken.Previous)
	if err != nil {
		// FATAL, because the handback reads this file and an unwritten token
		// is the owner's desktop left on Emacs after the run.
		t.Fatalf("%v", err)
	}
	t.Logf("%s", sweepFocusTakenNote(taken))
	t.Logf("where focus started is recorded at %s, for the handback the sweep's EXIT trap runs", path)
}

func TestSweepFocusGiveBack(t *testing.T) {
	if os.Getenv(sweepFocusEnv) != sweepFocusGiveBack {
		t.Skipf("the sweep's focus handback runs only from bin/realtest.sh, which sets %s=%s",
			sweepFocusEnv, sweepFocusGiveBack)
	}
	ctx, cancel := context.WithTimeout(context.Background(), sweepFocusSettleCeiling)
	defer cancel()

	runDir := os.Getenv(outEnv)
	if runDir == "" {
		t.Fatalf("%s is not set; bin/realtest.sh exports the run directory and the focus token lives in it",
			outEnv)
	}
	token, err := ReadSweepFocusToken(runDir)
	if err != nil {
		t.Fatalf("%v", err)
	}
	if token == sweepFocusNoPrevious {
		t.Logf("nothing was frontmost when the sweep took focus, so there is nobody to hand it back to")
		return
	}
	driver := &KeyDriver{Scratch: runDir}
	if err := driver.Build(ctx); err != nil {
		t.Fatalf("build the key helper to hand the sweep's focus back: %v", err)
	}
	restored, line, err := driver.GiveBackSweepFocus(ctx, token)
	if err != nil {
		t.Fatalf("hand focus back to %s: %v", token, err)
	}
	note := sweepFocusGaveBackNote(token, restored, line)
	if !restored {
		// A HANDBACK THAT DID NOT LAND IS REPORTED, NOT SWALLOWED. It is not
		// fatal — the application may have quit during the run, and the sweep
		// has already done its work — but the owner is looking at a desktop
		// this run changed and left, and nothing else would say so.
		t.Errorf("%s", note)
		return
	}
	t.Logf("%s", note)
}

// sweepFocusEmacsPid answers which Emacs to bring forward, or zero when none is
// answering.
//
// An editor that is not answering is NOT a failure here: realtests 1, 2, 3 and
// 5 through 8 each start their own, and the sweep's first press against a new
// process takes focus for it.
func sweepFocusEmacsPid(ctx context.Context, t *testing.T, runDir string) int {
	t.Helper()
	socket := os.Getenv(socketEnv)
	if socket == "" {
		t.Logf("%s is not set, so no editor could be addressed; the sweep records where focus started and "+
			"the first press takes it", socketEnv)
		return 0
	}
	client := &Client{Socket: socket, Scratch: runDir}
	pid, err := client.ReadInt(ctx, `(emacs-pid)`)
	if err != nil {
		t.Logf("no Emacs is answering %s (%v), so the sweep records where focus started and brings nothing "+
			"forward; the first press against the editor this run starts takes focus for it", socket, err)
		return 0
	}
	return pid
}
