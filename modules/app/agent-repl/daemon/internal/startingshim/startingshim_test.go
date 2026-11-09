package startingshim

import (
	"context"
	"errors"
	"net"
	"os"
	"path/filepath"
	"sync"
	"testing"
	"time"

	"agent-repld/internal/shimsocket"
)

// boundClock is a Clock whose bound elapses only when the test fires it.
type boundClock struct {
	fired chan time.Time
}

func newBoundClock() *boundClock { return &boundClock{fired: make(chan time.Time, 1)} }

func (c *boundClock) Now() time.Time { return time.Date(2026, 9, 13, 23, 0, 0, 0, time.UTC) }

func (c *boundClock) After(time.Duration) <-chan time.Time { return c.fired }

// fire elapses the bound.
func (c *boundClock) fire() { c.fired <- time.Time{} }

// kernel is the wait's two kernel reports in the test's hands: a change of
// the socket's directory, and the recorded process's exit.
type kernel struct {
	changes chan error
	exit    chan error
}

func newKernel() *kernel {
	return &kernel{changes: make(chan error), exit: make(chan error, 1)}
}

func (k *kernel) follow(context.Context, string) (<-chan error, func()) { return k.changes, func() {} }

func (k *kernel) awaitExit(ctx context.Context, _ int) error {
	select {
	case err := <-k.exit:
		return err
	case <-ctx.Done():
		return ctx.Err()
	}
}

// waiter is a Waiter over K, CLOCK and PROBE.
func waiter(alive func(int) bool, probe *scriptedProbe, clock *boundClock, k *kernel) Waiter {
	return Waiter{Alive: alive, Probe: probe.probe, Clock: clock, Changes: k.follow, Exit: k.awaitExit}
}

// scriptedProbe answers a socket state per path, and switches one path to LIVE
// after a given number of probes — which is exactly a shim binding its socket
// partway through the wait.
type scriptedProbe struct {
	mu sync.Mutex
	// liveAfter is how many probes of base must pass before it reads LIVE. A
	// negative value never goes live.
	liveAfter int
	seen      int
	// livePath, when set, is the path that goes live instead of the base one:
	// a relaunch generation binding rather than the base socket.
	livePath string
}

func (p *scriptedProbe) probe(path string) (shimsocket.State, error) {
	p.mu.Lock()
	defer p.mu.Unlock()
	want := p.livePath
	if want == "" {
		return p.answer(path, path)
	}
	return p.answer(path, want)
}

// answer counts a probe of the awaited path and says whether it has gone live.
func (p *scriptedProbe) answer(path, want string) (shimsocket.State, error) {
	if path != want {
		return shimsocket.StateAbsent, nil
	}
	p.seen++
	if p.liveAfter >= 0 && p.seen > p.liveAfter {
		return shimsocket.StateLive, nil
	}
	return shimsocket.StateAbsent, nil
}

func TestAwait(t *testing.T) {
	base := "/tmp/startingshim-test/ws.sock"
	alivePID := 4242
	tests := []struct {
		name string
		// arrange
		recorded *int
		alive    func(pid int) bool
		probe    *scriptedProbe
		// during runs once the wait has begun.
		during func(clock *boundClock, k *kernel, cancel func())
		// assert
		want     Outcome
		wantPath string
	}{
		{name: "no recorded spawn is no shim at all", alive: func(int) bool { return true },
			probe: &scriptedProbe{liveAfter: -1}, want: OutcomeNoSpawn, wantPath: base},
		{name: "a recorded spawn whose process is gone is no shim either", recorded: &alivePID,
			alive: func(int) bool { return false }, probe: &scriptedProbe{liveAfter: -1}, want: OutcomeSpawnDead, wantPath: base},
		{name: "a live process already listening is announced at once", recorded: &alivePID,
			alive: func(int) bool { return true }, probe: &scriptedProbe{liveAfter: 0}, want: OutcomeAnnounced, wantPath: base},
		{name: "a live process that binds partway through the wait is announced at the change", recorded: &alivePID,
			alive: func(int) bool { return true }, probe: &scriptedProbe{liveAfter: 1}, want: OutcomeAnnounced, wantPath: base,
			during: func(_ *boundClock, k *kernel, _ func()) { k.changes <- nil }},
		{name: "a live process that never binds leaves the answer undetermined at the bound", recorded: &alivePID,
			alive: func(int) bool { return true }, probe: &scriptedProbe{liveAfter: -1}, want: OutcomeUndetermined, wantPath: base,
			during: func(clock *boundClock, _ *kernel, _ func()) { clock.fire() }},
		{name: "a spawn that exits mid-wait is dead at its exit", recorded: &alivePID,
			alive: func(int) bool { return true }, probe: &scriptedProbe{liveAfter: -1}, want: OutcomeSpawnDead, wantPath: base,
			during: func(_ *boundClock, k *kernel, _ func()) { k.exit <- nil }},
		{name: "a spawn that bound just before it exited is announced", recorded: &alivePID,
			alive: func(int) bool { return true }, probe: &scriptedProbe{liveAfter: 1}, want: OutcomeAnnounced, wantPath: base,
			during: func(_ *boundClock, k *kernel, _ func()) { k.exit <- nil }},
		{name: "a watch that fails is undetermined, never a dead spawn", recorded: &alivePID,
			alive: func(int) bool { return true }, probe: &scriptedProbe{liveAfter: -1}, want: OutcomeUndetermined, wantPath: base,
			during: func(_ *boundClock, k *kernel, _ func()) { k.changes <- errors.New("kqueue refused") }},
		{name: "a cancelled context is undetermined, never a dead spawn", recorded: &alivePID,
			alive: func(int) bool { return true }, probe: &scriptedProbe{liveAfter: -1}, want: OutcomeUndetermined, wantPath: base,
			during: func(_ *boundClock, _ *kernel, cancel func()) { cancel() }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			clock, k := newBoundClock(), newKernel()
			w := waiter(tt.alive, tt.probe, clock, k)
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()
			type answer struct {
				path    string
				outcome Outcome
			}
			got := make(chan answer, 1)

			// Act.
			go func() {
				path, outcome := w.Await(ctx, tt.recorded, base, time.Minute)
				got <- answer{path, outcome}
			}()
			if tt.during != nil {
				tt.during(clock, k, cancel)
			}
			a := <-got

			// Assert.
			if a.outcome != tt.want {
				t.Fatalf("Await outcome = %v, want %v", a.outcome, tt.want)
			}
			if a.path != tt.wantPath {
				t.Fatalf("Await path = %q, want %q", a.path, tt.wantPath)
			}
		})
	}
}

// TestAwaitReportsTheGenerationTheStartingShimBound pins that the answered path
// is the one that ANSWERED, not the base: a relaunch's shim binds
// `<base>.nN.sock`, and adopting the base path would dial nothing.
func TestAwaitReportsTheGenerationTheStartingShimBound(t *testing.T) {
	// Arrange.
	dir, err := os.MkdirTemp("", "ssg")
	if err != nil {
		t.Fatalf("temp dir: %v", err)
	}
	t.Cleanup(func() { _ = os.RemoveAll(dir) })
	base := filepath.Join(dir, "ws.sock")
	generation := filepath.Join(dir, "ws.n1.sock")
	if err := os.WriteFile(generation, nil, 0o600); err != nil {
		t.Fatalf("write the generation path: %v", err)
	}
	pid := 4242
	probe := &scriptedProbe{liveAfter: 0, livePath: generation}
	w := waiter(func(int) bool { return true }, probe, newBoundClock(), newKernel())

	// Act.
	path, outcome := w.Await(context.Background(), &pid, base, time.Minute)

	// Assert.
	if outcome != OutcomeAnnounced {
		t.Fatalf("Await outcome = %v, want %v", outcome, OutcomeAnnounced)
	}
	if path != generation {
		t.Fatalf("Await path = %q, want the generation the shim bound, %q", path, generation)
	}
}

func TestAwaitSeesARealSocketLinkedIntoItsDirectory(t *testing.T) {
	// Arrange: the kernel's own reports, and a socket bound under a temporary
	// name, listening, then linked into place as the shim does.
	dir, err := os.MkdirTemp("", "ssr")
	if err != nil {
		t.Fatalf("temp dir: %v", err)
	}
	t.Cleanup(func() { _ = os.RemoveAll(dir) })
	base := filepath.Join(dir, "ws.sock")
	pid := os.Getpid()
	w := Waiter{Alive: func(int) bool { return true }, Probe: shimsocket.Probe, Clock: newBoundClock()}
	type answer struct {
		path    string
		outcome Outcome
	}
	got := make(chan answer, 1)
	go func() {
		path, outcome := w.Await(context.Background(), &pid, base, time.Minute)
		got <- answer{path, outcome}
	}()

	// Act.
	tmp := filepath.Join(dir, ".ws.sock.tmp")
	listener, err := net.Listen("unix", tmp)
	if err != nil {
		t.Fatalf("listen: %v", err)
	}
	t.Cleanup(func() { listener.Close() })
	if err := os.Link(tmp, base); err != nil {
		t.Fatalf("link the socket into place: %v", err)
	}
	a := <-got

	// Assert.
	if a.outcome != OutcomeAnnounced || a.path != base {
		t.Fatalf("Await = (%q, %v), want (%q, announced)", a.path, a.outcome, base)
	}
}

// TestAliveReadsTheKernelsAnswer pins the liveness primitive against two real
// pids: this process, which is alive by construction, and a pid nothing can be
// running under.
func TestAliveReadsTheKernelsAnswer(t *testing.T) {
	tests := []struct {
		name string
		pid  int
		want bool
	}{
		{name: "this very process is alive", pid: os.Getpid(), want: true},
		{name: "pid zero is never a shim: it addresses a process group", pid: 0, want: false},
		{name: "a negative pid is never a shim either", pid: -1, want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange, Act.
			got := Alive(tt.pid)

			// Assert.
			if got != tt.want {
				t.Fatalf("Alive(%d) = %v, want %v", tt.pid, got, tt.want)
			}
		})
	}
}
