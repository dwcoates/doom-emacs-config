package server

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"syscall"
	"testing"
	"time"

	"claude-repld/internal/sessionlock"
)

// ---------------------------------------------------------------------------
// THE STOP IS VERIFIED AND ESCALATED. A SIGTERM only asks; a shim that does not
// answer is SIGKILLed, and one that survives that is a typed failure rather
// than a stop reported successful. See StopShim / escalateStopToKill.
// ---------------------------------------------------------------------------

// scriptedWaits stands in for the bounded session-lock waits StopShim performs,
// answering each in order and recording the bound it was given. The stop is
// therefore driven through every rung with no waiting at all.
type scriptedWaits struct {
	results []error
	bounds  []time.Duration
}

func (w *scriptedWaits) wait(_ string, within time.Duration) error {
	w.bounds = append(w.bounds, within)
	if len(w.results) == 0 {
		return nil
	}
	result := w.results[0]
	w.results = w.results[1:]
	return result
}

// signalLog records what was signalled, so a test asserts the escalation
// ladder by the signals it actually delivered.
type signalLog struct {
	sent []syscall.Signal
	pids []int
	errs map[syscall.Signal]error
}

func (l *signalLog) send(pid int, sig syscall.Signal) error {
	l.sent = append(l.sent, sig)
	l.pids = append(l.pids, pid)
	return l.errs[sig]
}

// errLockUnreadable is a probe that could not answer, as distinct from one that
// answered "still held". The two must be treated differently: only the second
// licenses a harder signal.
var errLockUnreadable = errors.New("permission denied reading the lock")

// expiredWait is what the waiter reports when its bound elapsed with the lock
// still held — the one outcome the stop escalates on.
func expiredWait() error { return fmt.Errorf("%w: still held", errStopWaitExpired) }

func TestStopShimEscalationLadder(t *testing.T) {
	const pid = 4242
	tests := []struct {
		name string
		// handle says whether this daemon spawned the shim, which decides
		// whether the first SIGTERM travels by handle or by pid.
		handle      bool
		pid         int32
		waits       []error
		signalErrs  map[syscall.Signal]error
		wantSignals []syscall.Signal
		wantErr     error
		wantErrText string
	}{
		{
			name:        "SIGTERM alone is enough",
			pid:         pid,
			waits:       []error{nil},
			wantSignals: []syscall.Signal{syscall.SIGTERM},
		},
		{
			name:        "an ignored SIGTERM escalates to SIGKILL",
			pid:         pid,
			waits:       []error{expiredWait(), nil},
			wantSignals: []syscall.Signal{syscall.SIGTERM, syscall.SIGKILL},
		},
		{
			name:        "a shim surviving SIGKILL is a typed failure",
			pid:         pid,
			waits:       []error{expiredWait(), expiredWait()},
			wantSignals: []syscall.Signal{syscall.SIGTERM, syscall.SIGKILL},
			wantErr:     ErrShimSurvivedStop,
		},
		{
			name:        "a pid already gone at the SIGKILL is a stop that already happened",
			pid:         pid,
			waits:       []error{expiredWait(), nil},
			signalErrs:  map[syscall.Signal]error{syscall.SIGKILL: os.ErrProcessDone},
			wantSignals: []syscall.Signal{syscall.SIGTERM, syscall.SIGKILL},
		},
		{
			name:    "an ignored SIGTERM with no pid to escalate to is a typed failure",
			pid:     0,
			waits:   []error{expiredWait()},
			wantErr: ErrShimSurvivedStop,
		},
		{
			name:        "an unreadable lock fails the stop without escalating",
			pid:         pid,
			waits:       []error{errLockUnreadable},
			wantSignals: []syscall.Signal{syscall.SIGTERM},
			wantErr:     errLockUnreadable,
		},
		{
			name:        "a daemon-spawned shim escalates by its announced pid",
			handle:      true,
			pid:         pid,
			waits:       []error{expiredWait(), nil},
			wantSignals: []syscall.Signal{syscall.SIGKILL},
		},
		{
			name:        "a pid that names no shim is refused rather than signalled",
			pid:         1,
			waits:       []error{nil},
			wantErrText: "refusing to signal pid 1",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			waits := &scriptedWaits{results: tc.waits}
			signals := &signalLog{errs: tc.signalErrs}
			s := NewShimSpawner(nil, nil, nil, nil, nil)
			s.awaitStopped = waits.wait
			s.signal = signals.send
			s.termGraceOverride = 7 * time.Millisecond
			s.killGraceOverride = 3 * time.Millisecond
			if tc.handle {
				s.handles["s1"] = ShimHandle{Stop: func(ShimStop) error { return nil }}
			}

			// Act
			err := s.StopShim("s1", tc.pid, unitTestStop)

			// Assert
			switch {
			case tc.wantErr != nil:
				if !errors.Is(err, tc.wantErr) {
					t.Fatalf("StopShim = %v, want %v", err, tc.wantErr)
				}
			case tc.wantErrText != "":
				if err == nil || !strings.Contains(err.Error(), tc.wantErrText) {
					t.Fatalf("StopShim = %v, want an error naming %q", err, tc.wantErrText)
				}
			default:
				if err != nil {
					t.Fatalf("StopShim = %v, want the stop to complete", err)
				}
			}
			if got := signals.sent; !equalSignals(got, tc.wantSignals) {
				t.Fatalf("signals = %v, want %v", got, tc.wantSignals)
			}
			for _, p := range signals.pids {
				if p != int(tc.pid) {
					t.Fatalf("signalled pid %d, want the announced %d", p, tc.pid)
				}
			}
		})
	}
}

func equalSignals(got, want []syscall.Signal) bool {
	if len(got) != len(want) {
		return false
	}
	for i := range got {
		if got[i] != want[i] {
			return false
		}
	}
	return true
}

// TestStopShimEscalationBoundsAreDistinct: the SIGKILL wait is its own,
// shorter bound. A SIGKILL cannot be ignored, so waiting the SIGTERM grace
// again would hold the caller for twice as long as anything can justify.
func TestStopShimEscalationBoundsAreDistinct(t *testing.T) {
	// Arrange
	waits := &scriptedWaits{results: []error{expiredWait(), nil}}
	s := NewShimSpawner(nil, nil, nil, nil, nil)
	s.awaitStopped = waits.wait
	s.signal = func(int, syscall.Signal) error { return nil }

	// Act
	if err := s.StopShim("s1", 4242, unitTestStop); err != nil {
		t.Fatalf("StopShim: %v", err)
	}

	// Assert
	if len(waits.bounds) != 2 {
		t.Fatalf("lock waits = %d, want one per rung", len(waits.bounds))
	}
	if waits.bounds[0] != stopTermGrace || waits.bounds[1] != stopKillGrace {
		t.Fatalf("bounds = %v, want [%s %s]", waits.bounds, stopTermGrace, stopKillGrace)
	}
}

// TestStopShimSurvivorLeavesTheParkedTransportAlone: a stop that failed has not
// established process exit, so the only route back to a still-live shim must
// not be cleaned up.
func TestStopShimSurvivorLeavesTheParkedTransportAlone(t *testing.T) {
	// Arrange
	evicted := false
	waits := &scriptedWaits{results: []error{expiredWait(), expiredWait()}}
	s := NewShimSpawner(nil, nil, func(string, string) bool { evicted = true; return true }, nil, nil)
	s.awaitStopped = waits.wait
	s.signal = func(int, syscall.Signal) error { return nil }

	// Act
	err := s.StopShim("s1", 4242, unitTestStop)

	// Assert
	if !errors.Is(err, ErrShimSurvivedStop) {
		t.Fatalf("StopShim = %v, want ErrShimSurvivedStop", err)
	}
	if evicted {
		t.Fatal("the parked transport of a shim that is still alive was evicted")
	}
}

// TestSignallablePIDRefusesPidsThatNameNoShim covers the pids a stop must never
// deliver a signal to, because kill(2) reads them as something other than one
// process.
func TestSignallablePIDRefusesPidsThatNameNoShim(t *testing.T) {
	tests := []struct {
		name string
		pid  int32
		want string
	}{
		{name: "zero is every process in the group", pid: 0, want: "names no shim"},
		{name: "negative is a process group", pid: -1, want: "names no shim"},
		{name: "init", pid: 1, want: "names no shim"},
		{name: "the daemon itself", pid: int32(os.Getpid()), want: "this daemon's own process"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange / Act
			err := signallablePID("s1", tc.pid)

			// Assert
			if err == nil || !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("signallablePID(%d) = %v, want a refusal naming %q", tc.pid, err, tc.want)
			}
		})
	}
}

// ---------------------------------------------------------------------------
// A SUPERSEDE WHOSE STOP DID NOT WIN DOES NOT MINT A REPLACEMENT. Continuing
// past a live writer of the same transcript is the double-writer condition the
// supersede exists to remove. See supersede.go.
// ---------------------------------------------------------------------------

// TestCreateRefusesWhenASupersededShimSurvivesItsStop: the observed defect. The
// stop is fire-and-forget no longer, and a stop that loses stops the create.
func TestCreateRefusesWhenASupersededShimSurvivesItsStop(t *testing.T) {
	// Arrange — a workspace already holding a live session whose shim will not
	// die.
	h := newHarness(t)
	old := createSession(t, h, `{"cwd":"/w"}`)
	h.spawner.mu.Lock()
	h.spawner.stopErr = fmt.Errorf("%w: session %s", ErrShimSurvivedStop, old)
	h.spawner.mu.Unlock()
	before := len(h.reg.All())

	// Act — a newer create claims the same workspace.
	id, err := createSessionErr(t, h, `{"cwd":"/w"}`)

	// Assert — the create is refused with the typed cause, and nothing was
	// minted beside the survivor.
	if !errors.Is(err, ErrShimSurvivedStop) {
		t.Fatalf("CreateSession = (%q, %v), want ErrShimSurvivedStop", id, err)
	}
	if id != "" {
		t.Fatalf("CreateSession returned session %q; no replacement may be minted past a live writer", id)
	}
	if got := len(h.reg.All()); got != before {
		t.Fatalf("registry records went %d -> %d; the refused create wrote one", before, got)
	}
}

// TestCreateProceedsWhenASupersededStopFailsForAnyOtherReason: only a SURVIVING
// shim stops the create. A record whose teardown failed for a bookkeeping
// reason must still be stood down and the create must still land, exactly as it
// does when the stop succeeds.
func TestCreateProceedsWhenASupersededStopFailsForAnyOtherReason(t *testing.T) {
	// Arrange
	h := newHarness(t)
	old := createSession(t, h, `{"cwd":"/w"}`)
	h.spawner.mu.Lock()
	h.spawner.stopErr = errors.New("the turn drain timed out")
	h.spawner.mu.Unlock()

	// Act
	id, err := createSessionErr(t, h, `{"cwd":"/w"}`)

	// Assert
	if err != nil || id == "" {
		t.Fatalf("CreateSession = (%q, %v), want the create to land", id, err)
	}
	if rec, _ := h.reg.Get(old); !rec.Terminal {
		t.Fatalf("old record = %+v, want it stood down", rec)
	}
}

// TestAwaitShimStoppedReportsTheExpiryDistinctly drives the production waiter
// against a genuinely held lock. The bound is the override's, so the expiry is
// observed rather than waited out.
func TestAwaitShimStoppedReportsTheExpiryDistinctly(t *testing.T) {
	// Arrange — a lock held by this process, which is what a live shim looks
	// like to the probe.
	home := t.TempDir()
	t.Setenv("HOME", home)
	path, err := sessionlock.Path("s_held")
	if err != nil {
		t.Fatalf("Path: %v", err)
	}
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	f, err := os.OpenFile(path, os.O_CREATE|os.O_RDWR, 0o644)
	if err != nil {
		t.Fatalf("open: %v", err)
	}
	defer f.Close()
	if err := syscall.Flock(int(f.Fd()), syscall.LOCK_EX|syscall.LOCK_NB); err != nil {
		t.Fatalf("flock: %v", err)
	}

	// Act
	waitErr := awaitShimStopped("s_held", time.Millisecond)

	// Assert — the expiry is the sentinel the escalation keys on, and a free
	// session is not an expiry at all.
	if !errors.Is(waitErr, errStopWaitExpired) {
		t.Fatalf("awaitShimStopped over a held lock = %v, want errStopWaitExpired", waitErr)
	}
	if err := awaitShimStopped("s_free", time.Millisecond); err != nil {
		t.Fatalf("awaitShimStopped over a free session = %v, want nil", err)
	}
}
