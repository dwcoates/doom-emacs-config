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
		Connected: func(string) bool { return false },
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
	s.Connected = func(string) bool { return true }

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
	s.Connected = func(string) bool {
		mu.Lock()
		defer mu.Unlock()
		calls++
		if calls == 1 {
			recheck <- time.Time{}
			return false
		}
		return true
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
	s.Connected = func(string) bool {
		once.Do(func() { recheck <- time.Time{} })
		return false
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
	s.Connected = func(string) bool { return true }

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

// CONCURRENCY IS BOUNDED. A boot with many registered workspaces must not open
// all of them at the same instant.
func TestBootSweepBoundsItsConcurrency(t *testing.T) {
	// Arrange — six workspaces, all parked, with a gate holding every Ensure
	// open so the peak is observable rather than inferred.
	s, ens, _ := sweepRig(t, "/a", "/b", "/c", "/d", "/e", "/f")
	s.Connected = func(string) bool { return true }
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
