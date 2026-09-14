package startingshim

import (
	"context"
	"os"
	"path/filepath"
	"sync"
	"testing"
	"time"

	"claude-repld/internal/shimsocket"
)

// stepClock is a Clock that never sleeps: every After fires at once and
// ADVANCES the clock by the interval it was asked for, so a bounded poll loop
// runs its real number of passes in no wall time at all.
type stepClock struct {
	mu  sync.Mutex
	now time.Time
	// steps counts the After calls, which is the number of polls the wait
	// actually paid for.
	steps int
}

func newStepClock() *stepClock {
	return &stepClock{now: time.Date(2026, 9, 13, 23, 0, 0, 0, time.UTC)}
}

func (c *stepClock) Now() time.Time {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.now
}

func (c *stepClock) After(d time.Duration) <-chan time.Time {
	c.mu.Lock()
	c.now = c.now.Add(d)
	c.steps++
	fired := c.now
	c.mu.Unlock()
	ch := make(chan time.Time, 1)
	ch <- fired
	return ch
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
		cancel   bool
		// assert
		want      Outcome
		wantPath  string
		wantSteps int
	}{
		{
			name:     "no recorded spawn is no shim at all",
			recorded: nil,
			alive:    func(int) bool { return true },
			probe:    &scriptedProbe{liveAfter: -1},
			want:     OutcomeNoSpawn,
			wantPath: base,
		},
		{
			name:     "a recorded spawn whose process is gone is no shim either",
			recorded: &alivePID,
			alive:    func(int) bool { return false },
			probe:    &scriptedProbe{liveAfter: -1},
			want:     OutcomeSpawnDead,
			wantPath: base,
		},
		{
			name:     "a live process already listening is announced without a single poll",
			recorded: &alivePID,
			alive:    func(int) bool { return true },
			probe:    &scriptedProbe{liveAfter: 0},
			want:     OutcomeAnnounced,
			wantPath: base,
		},
		{
			name:     "a live process that binds partway through the wait is announced",
			recorded: &alivePID,
			alive:    func(int) bool { return true },
			probe:    &scriptedProbe{liveAfter: 3},
			want:     OutcomeAnnounced,
			wantPath: base,
			// three probes read absent, so three polls were paid for.
			wantSteps: 3,
		},
		{
			name:     "a live process that never binds leaves the answer undetermined",
			recorded: &alivePID,
			alive:    func(int) bool { return true },
			probe:    &scriptedProbe{liveAfter: -1},
			want:     OutcomeUndetermined,
			wantPath: base,
		},
		{
			name:     "a cancelled context is undetermined, never a dead spawn",
			recorded: &alivePID,
			alive:    func(int) bool { return true },
			probe:    &scriptedProbe{liveAfter: -1},
			cancel:   true,
			want:     OutcomeUndetermined,
			wantPath: base,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			clock := newStepClock()
			w := Waiter{Alive: tt.alive, Probe: tt.probe.probe, Clock: clock, Poll: 20 * time.Millisecond}
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()
			if tt.cancel {
				cancel()
			}

			// Act.
			path, outcome := w.Await(ctx, tt.recorded, base, 200*time.Millisecond)

			// Assert.
			if outcome != tt.want {
				t.Fatalf("Await outcome = %v, want %v", outcome, tt.want)
			}
			if path != tt.wantPath {
				t.Fatalf("Await path = %q, want %q", path, tt.wantPath)
			}
			if tt.wantSteps > 0 && clock.steps != tt.wantSteps {
				t.Fatalf("Await polled %d times, want %d", clock.steps, tt.wantSteps)
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
	w := Waiter{Alive: func(int) bool { return true }, Probe: probe.probe, Clock: newStepClock()}

	// Act.
	path, outcome := w.Await(context.Background(), &pid, base, 200*time.Millisecond)

	// Assert.
	if outcome != OutcomeAnnounced {
		t.Fatalf("Await outcome = %v, want %v", outcome, OutcomeAnnounced)
	}
	if path != generation {
		t.Fatalf("Await path = %q, want the generation the shim bound, %q", path, generation)
	}
}

// TestAwaitStopsWaitingForASpawnThatDiesMidWait pins the re-ask: a process that
// goes away while the wait is running is the ordinary "the shim would not come
// up" case, and holding a boot to the whole bound for it is a listener nobody
// accepts on.
func TestAwaitStopsWaitingForASpawnThatDiesMidWait(t *testing.T) {
	// Arrange.
	var asked int
	alive := func(int) bool {
		asked++
		return asked <= 2
	}
	pid := 4242
	probe := &scriptedProbe{liveAfter: -1}
	clock := newStepClock()
	w := Waiter{Alive: alive, Probe: probe.probe, Clock: clock, Poll: 20 * time.Millisecond}

	// Act.
	_, outcome := w.Await(context.Background(), &pid, "/tmp/startingshim-test/ws.sock", time.Hour)

	// Assert.
	if outcome != OutcomeSpawnDead {
		t.Fatalf("Await outcome = %v, want %v once the recorded process is gone", outcome, OutcomeSpawnDead)
	}
	if clock.steps != 1 {
		t.Fatalf("Await polled %d times after the process died, want 1", clock.steps)
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
