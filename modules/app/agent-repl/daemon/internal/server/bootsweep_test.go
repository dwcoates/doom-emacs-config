package server

import (
	"context"
	"errors"
	"fmt"
	"runtime"
	"strings"
	"sync"
	"testing"
	"time"

	"claude-repld/internal/bounceledger"
	"claude-repld/internal/registry"
)

// ---------------------------------------------------------------------------
// BOOT RECONCILIATION (bootsweep.go): claiming the shims that outlived the
// previous daemon.
//
// The classifications are the whole behavior, so each case pins exactly one:
// parked, locked-then-parked, locked-forever, and nothing-there. The re-check
// pass is driven as an EVENT (an injected channel), never a sleep — the delay
// is a redial-window bound in production and has no business being real here.
// ---------------------------------------------------------------------------

// sweepEnsurer records the workspaces it was asked to bring up, and can be
// made to block so a test can observe concurrency.
type sweepEnsurer struct {
	mu      sync.Mutex
	calls   []string
	inside  int
	maxSeen int
	gate    chan struct{}
	err     error
}

func (e *sweepEnsurer) Ensure(workspace string) error {
	e.mu.Lock()
	e.calls = append(e.calls, workspace)
	e.inside++
	if e.inside > e.maxSeen {
		e.maxSeen = e.inside
	}
	e.mu.Unlock()
	if e.gate != nil {
		<-e.gate
	}
	e.mu.Lock()
	e.inside--
	e.mu.Unlock()
	return e.err
}

// EnsureDriveable is never used by the boot sweep — it reattaches, it does not
// send — so it routes to the same recording body rather than pretending to be a
// second, differently-behaved bring-up.
func (e *sweepEnsurer) EnsureDriveable(_ context.Context, workspace string) error {
	return e.Ensure(workspace)
}

func (e *sweepEnsurer) ReviveForMerge(_ context.Context, workspace string) error {
	return e.Ensure(workspace)
}

func (e *sweepEnsurer) ensured() []string {
	e.mu.Lock()
	defer e.mu.Unlock()
	return append([]string(nil), e.calls...)
}

func (e *sweepEnsurer) peak() int {
	e.mu.Lock()
	defer e.mu.Unlock()
	return e.maxSeen
}

// sweepRig builds a sweeper over a temp registry holding one record per cwd.
func sweepRig(t *testing.T, cwds ...string) (*BootSweeper, *sweepEnsurer, *[]string) {
	t.Helper()
	reg := openTestRegistry(t)
	for i, cwd := range cwds {
		if err := reg.Put(registry.Record{
			SessionID: "s_" + cwd,
			CWD:       cwd,
			CreatedAt: "2026-07-25T10:0" + string(rune('0'+i)) + ":00Z",
		}); err != nil {
			t.Fatalf("Put: %v", err)
		}
	}
	ens := &sweepEnsurer{}
	var mu sync.Mutex
	var lines []string
	s := &BootSweeper{
		Reg:       reg,
		Connected: func(string) (bool, error) { return false, nil },
		Held:      func(string) (bool, error) { return false, nil },
		Ensurer:   ens,
		Logf: func(f string, a ...any) {
			mu.Lock()
			defer mu.Unlock()
			lines = append(lines, fmt.Sprintf(f, a...))
		},
		Recheck: make(chan time.Time),
	}
	return s, ens, &lines
}

func logged(lines *[]string, needle string) bool {
	for _, l := range *lines {
		if strings.Contains(l, needle) {
			return true
		}
	}
	return false
}

// A PARKED connection is the whole point: it is already there, and nothing was
// claiming it.
func TestBootSweepReattachesAParkedShim(t *testing.T) {
	// Arrange.
	s, ens, lines := sweepRig(t, "/w")
	s.Connected = func(string) (bool, error) { return true, nil }

	// Act.
	s.Run(context.Background())

	// Assert.
	if got := ens.ensured(); len(got) != 1 || got[0] != "/w" {
		t.Fatalf("ensured = %v, want exactly /w", got)
	}
	if !logged(lines, "REATTACHED") {
		t.Fatalf("the reattach was not logged; lines: %v", *lines)
	}
}

// A shim MID-BACKOFF holds its lock without a connection. Reattaching it on the
// spot would fail (EnsureShim refuses to spawn against a held lock), so it is
// carried to the re-check — and claimed there once the redial lands.
func TestBootSweepReattachesALockedShimOnTheRecheck(t *testing.T) {
	// Arrange — locked and unconnected at the first probe, connected at the
	// next one. The first probe RELEASES the re-check, so the second pass
	// happens because the first finished rather than because time passed.
	s, ens, lines := sweepRig(t, "/w")
	recheck := make(chan time.Time, 1)
	s.Recheck = recheck
	s.Held = func(string) (bool, error) { return true, nil }
	var (
		mu    sync.Mutex
		calls int
	)
	s.Connected = func(string) (bool, error) {
		mu.Lock()
		defer mu.Unlock()
		calls++
		if calls == 1 {
			recheck <- time.Time{}
			return false, nil
		}
		return true, nil
	}

	// Act.
	s.Run(context.Background())

	// Assert.
	if got := ens.ensured(); len(got) != 1 || got[0] != "/w" {
		t.Fatalf("ensured = %v, want exactly /w on the re-check", got)
	}
	if !logged(lines, "deferring to the re-check pass") {
		t.Fatalf("the deferral was not logged; lines: %v", *lines)
	}
}

// A lock still held with nothing dialled in AFTER the redial window is the
// pathological state EnsureShim exists to surface. It is said plainly and NOT
// retried, so a boot can never turn into a spawn loop against a live holder.
func TestBootSweepNeverSpawnsAgainstAStuckLock(t *testing.T) {
	// Arrange.
	s, ens, lines := sweepRig(t, "/w")
	recheck := make(chan time.Time, 1)
	s.Recheck = recheck
	s.Held = func(string) (bool, error) { return true, nil }
	var once sync.Once
	s.Connected = func(string) (bool, error) {
		once.Do(func() { recheck <- time.Time{} })
		return false, nil
	}

	// Act.
	s.Run(context.Background())

	// Assert.
	if got := ens.ensured(); len(got) != 0 {
		t.Fatalf("ensured = %v, want nothing — spawning against a held lock is two writers on one transcript", got)
	}
	if !logged(lines, "STILL holds its lock") {
		t.Fatalf("the stuck lock was not surfaced; lines: %v", *lines)
	}
}

// NEITHER connected nor locked means there is no shim. Booting the daemon is
// not a reason to start every conversation the user has ever had.
func TestBootSweepLeavesAnUnwiredWorkspaceAlone(t *testing.T) {
	// Arrange — the rig's defaults are exactly this case.
	s, ens, lines := sweepRig(t, "/w")

	// Act.
	s.Run(context.Background())

	// Assert.
	if got := ens.ensured(); len(got) != 0 {
		t.Fatalf("ensured = %v, want nothing for a workspace with no shim", got)
	}
	if !logged(lines, "leaving it UNWIRED") {
		t.Fatalf("the unwired classification was not logged; lines: %v", *lines)
	}
}

// A TERMINAL record is not a session to reattach; its conversation ended.
func TestBootSweepSkipsTerminalRecords(t *testing.T) {
	// Arrange.
	s, ens, _ := sweepRig(t)
	if err := s.Reg.Put(registry.Record{SessionID: "s_dead", CWD: "/w", Terminal: true}); err != nil {
		t.Fatalf("Put: %v", err)
	}
	s.Connected = func(string) (bool, error) { return true, nil }

	// Act.
	s.Run(context.Background())

	// Assert.
	if got := ens.ensured(); len(got) != 0 {
		t.Fatalf("ensured = %v, want nothing — a terminal record has no session to drive", got)
	}
}

// A LOCK PROBE THAT FAILED did not say "free", and must never be read as such.
func TestBootSweepDoesNotTreatAnUnreadableLockAsFree(t *testing.T) {
	// Arrange.
	s, ens, lines := sweepRig(t, "/w")
	s.Held = func(string) (bool, error) { return false, errors.New("permission denied") }

	// Act.
	s.Run(context.Background())

	// Assert.
	if got := ens.ensured(); len(got) != 0 {
		t.Fatalf("ensured = %v, want nothing on an unreadable lock", got)
	}
	if !logged(lines, "UNKNOWN") {
		t.Fatalf("the unreadable probe was not surfaced; lines: %v", *lines)
	}
}

func TestBootSweepDoesNotTreatAnUnreadableParkedConnectionAsAbsent(t *testing.T) {
	// Arrange — a failed non-consuming socket probe cannot authorize either a
	// reattach or the no-shim classification.
	s, ens, lines := sweepRig(t, "/w")
	probes := 0
	recheck := make(chan time.Time, 1)
	recheck <- time.Time{}
	s.Recheck = recheck
	s.Connected = func(string) (bool, error) {
		probes++
		return false, errors.New("socket probe failed")
	}

	// Act.
	s.Run(context.Background())

	// Assert.
	if got := ens.ensured(); len(got) != 0 {
		t.Fatalf("ensured = %v, want nothing on an unreadable parked transport", got)
	}
	if probes != 2 {
		t.Fatalf("connection probes = %d, want boot and bounded re-check", probes)
	}
	if !logged(lines, "parked-connection probe FAILED") || !logged(lines, "UNKNOWN") {
		t.Fatalf("the connection probe failure was not surfaced; lines: %v", *lines)
	}
}

func TestBootSweepSurfacesFailedParkedReattachWithoutDeferringIt(t *testing.T) {
	// A proven parked shim whose controller cannot be reconstructed must not be
	// retried as though its transport identity were still uncertain.
	s, ens, lines := sweepRig(t, "/w")
	s.Connected = func(string) (bool, error) { return true, nil }
	ens.err = errors.New("controller construction failed")

	s.Run(context.Background())

	if got := ens.ensured(); len(got) != 1 || got[0] != "/w" {
		t.Fatalf("ensured = %v, want one attempted reattach", got)
	}
	if !logged(lines, "reattach FAILED") {
		t.Fatalf("failed reattach was not logged: %v", *lines)
	}
}

// CONCURRENCY IS BOUNDED. A boot with many registered workspaces must not open
// all of them at the same instant.
func TestBootSweepBoundsItsConcurrency(t *testing.T) {
	// Arrange — six workspaces, all parked, with a gate holding every Ensure
	// open so the peak is observable rather than inferred.
	s, ens, _ := sweepRig(t, "/a", "/b", "/c", "/d", "/e", "/f")
	s.Connected = func(string) (bool, error) { return true, nil }
	s.Parallelism = 2
	ens.gate = make(chan struct{})

	// Act — release the gate only once the cap has demonstrably been reached,
	// so the assertion rests on an event and not on timing.
	done := make(chan struct{})
	go func() { s.Run(context.Background()); close(done) }()
	for ens.peak() < 2 {
		runtime.Gosched()
	}
	close(ens.gate)
	<-done

	// Assert.
	if got := ens.peak(); got > 2 {
		t.Fatalf("peak concurrent reattaches = %d, want at most 2", got)
	}
	if got := ens.ensured(); len(got) != 6 {
		t.Fatalf("ensured %d workspaces, want all 6", len(got))
	}
}

// --- the unwired classification --------------------------------------------

// unwiredCalls records what the classifier was handed, and can be made to fail
// so the error path is a case rather than a hope.
type unwiredCalls struct {
	mu    sync.Mutex
	calls []string
	err   error
}

func (u *unwiredCalls) note(workspace, sessionID, verdict string) error {
	u.mu.Lock()
	defer u.mu.Unlock()
	u.calls = append(u.calls, fmt.Sprintf("%s|%s|%s", workspace, sessionID, verdict))
	return u.err
}

func (u *unwiredCalls) seen() []string {
	u.mu.Lock()
	defer u.mu.Unlock()
	return append([]string(nil), u.calls...)
}

// EVERY BRANCH THAT LEAVES A SESSION UNWIRED HANDS IT OVER, named by the
// verdict that left it that way — one row per conclusion the sweep can reach.
func TestBootSweepClassifiesEveryUnwiredVerdict(t *testing.T) {
	tests := []struct {
		name      string
		connected func(string) (bool, error)
		held      func(string) (bool, error)
		want      string
	}{
		{
			name:      "no live shim at all",
			connected: func(string) (bool, error) { return false, nil },
			held:      func(string) (bool, error) { return false, nil },
			want:      BootSweepUnwiredNoLiveShim,
		},
		{
			name:      "a lock held with nothing dialled in",
			connected: func(string) (bool, error) { return false, nil },
			held:      func(string) (bool, error) { return true, nil },
			want:      BootSweepUnwiredLockHeldWithoutConnection,
		},
		{
			name:      "a parked-connection probe that never answered",
			connected: func(string) (bool, error) { return false, errors.New("listener unreadable") },
			held:      func(string) (bool, error) { return false, nil },
			want:      BootSweepUnwiredProbeFailed,
		},
		{
			name:      "a lock probe that never answered",
			connected: func(string) (bool, error) { return false, nil },
			held:      func(string) (bool, error) { return false, errors.New("lock dir unreadable") },
			want:      BootSweepUnwiredLockProbeFailed,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange — the re-check is released by the first pass, so both
			// passes happen because the first finished rather than on a timer.
			s, _, _ := sweepRig(t, "/w")
			recheck := make(chan time.Time, 1)
			s.Recheck = recheck
			s.Held = tc.held
			var once sync.Once
			s.Connected = func(id string) (bool, error) {
				once.Do(func() { recheck <- time.Time{} })
				return tc.connected(id)
			}
			seen := &unwiredCalls{}
			s.Unwired = seen.note

			// Act.
			s.Run(context.Background())

			// Assert.
			want := "/w|s_/w|" + tc.want
			for _, got := range seen.seen() {
				if got == want {
					return
				}
			}
			t.Fatalf("classified %v, want one entry %q: a conclusion the sweep reached about a session the user owns cannot be a log line only", seen.seen(), want)
		})
	}
}

// A WIRED session is not classified: the sweep did not finish with it, it
// claimed it, and reporting a reattached workspace as unwired would be a
// verdict the sweep never reached.
func TestBootSweepClassifiesNothingForAReattachedSession(t *testing.T) {
	// Arrange.
	s, _, _ := sweepRig(t, "/w")
	s.Connected = func(string) (bool, error) { return true, nil }
	seen := &unwiredCalls{}
	s.Unwired = seen.note

	// Act.
	s.Run(context.Background())

	// Assert.
	if got := seen.seen(); len(got) != 0 {
		t.Fatalf("classified %v, want nothing for a session that was REATTACHED", got)
	}
}

// A CLASSIFIER THAT FAILED IS LOUD. The record never reached the user, which is
// precisely the silence the classification exists to end, so it may not be
// swallowed into the ordinary unwired line.
func TestBootSweepReportsAFailedClassification(t *testing.T) {
	// Arrange.
	s, _, lines := sweepRig(t, "/w")
	seen := &unwiredCalls{err: errors.New("state store closed")}
	s.Unwired = seen.note

	// Act.
	s.Run(context.Background())

	// Assert.
	if !logged(lines, "could NOT be classified") {
		t.Fatalf("a failed classification was not surfaced; lines: %v", *lines)
	}
}

// WITH NO CLASSIFIER WIRED the verdict still says, in its own line, that it
// reached the user nowhere. An unclaimed hook is a known gap, not a quiet one.
func TestBootSweepSaysWhenAVerdictIsNotClassified(t *testing.T) {
	// Arrange — the rig wires no classifier, which is today's daemon.
	s, _, lines := sweepRig(t, "/w")

	// Act.
	s.Run(context.Background())

	// Assert.
	if !logged(lines, "is NOT CLASSIFIED") {
		t.Fatalf("an unclassified verdict was silent; lines: %v", *lines)
	}
}

// ---------------------------------------------------------------------------
// BOUNCE ACCOUNTING (bootsweep.go reportBounce, bounceledger): a bounce is
// scored by pid identity, because counting cannot tell a survivor from a
// replacement.
// ---------------------------------------------------------------------------

func TestBootSweepReportsAPreservedShimByPID(t *testing.T) {
	// Arrange.
	s, _, lines := sweepRig(t, "/w")
	s.Ledger = []bounceledger.Entry{{SessionID: "s_/w", Workspace: "/w", PID: 27494, Disposition: bounceledger.DispositionPreserved}}
	s.Holders = func(string) ([]int, error) { return []int{27494}, nil }

	// Act.
	s.Run(context.Background())

	// Assert.
	if !logged(lines, "session=s_/w ws=\"/w\" shim_pid=27494 verdict=PRESERVED") {
		t.Fatalf("no PRESERVED verdict; got %v", *lines)
	}
}

func TestBootSweepReportsAReplacedShimAsDIED(t *testing.T) {
	// Arrange.
	s, _, lines := sweepRig(t, "/w")
	s.Ledger = []bounceledger.Entry{{SessionID: "s_/w", Workspace: "/w", PID: 27494, Disposition: bounceledger.DispositionPreserved}}
	s.Holders = func(string) ([]int, error) { return []int{51755}, nil }

	// Act.
	s.Run(context.Background())

	// Assert.
	if !logged(lines, "verdict=DIED") || !logged(lines, "FLEET LOSS died=1 of 1") {
		t.Fatalf("no DIED verdict or fleet-loss line; got %v", *lines)
	}
}

func TestBootSweepReportsADeliberateRollAsROLLEDRatherThanPreserved(t *testing.T) {
	// Arrange.
	s, _, lines := sweepRig(t, "/w")
	s.Ledger = []bounceledger.Entry{{
		SessionID: "s_/w", Workspace: "/w", PID: 27494,
		Disposition: bounceledger.DispositionRolled, Reason: "the shim bundle was superseded",
	}}
	s.Holders = func(string) ([]int, error) { return nil, nil }

	// Act.
	s.Run(context.Background())

	// Assert.
	if !logged(lines, "verdict=ROLLED") || logged(lines, "verdict=PRESERVED") {
		t.Fatalf("roll not reported as ROLLED; got %v", *lines)
	}
}

func TestBootSweepSaysItCanMakeNoClaimWithoutAHolderProbe(t *testing.T) {
	// Arrange.
	s, _, lines := sweepRig(t, "/w")
	s.Ledger = []bounceledger.Entry{{SessionID: "s_/w", Workspace: "/w", PID: 27494, Disposition: bounceledger.DispositionPreserved}}

	// Act.
	s.Run(context.Background())

	// Assert.
	if !logged(lines, "bounce accounting SKIPPED") {
		t.Fatalf("no skip line; got %v", *lines)
	}
}
