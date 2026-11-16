package runs

import (
	"bytes"
	"context"
	"errors"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"slices"
	"syscall"
	"testing"
	"time"

	sharedlogging "agentrepl/logging"
	"agentrepl/logging/recordlog"
)

// waitBound bounds every wait a test makes on a run: each run here writes a
// line or two and ends within milliseconds (measured 2026-11-16: 10-30ms per
// case), so a wait past it is a hang, said by name rather than outliving
// the suite.
const waitBound = 5 * time.Second

// await waits for CH, failing the test with WHAT past waitBound.
func (h *harness) await(ch <-chan []Event, what string) []Event {
	h.t.Helper()
	select {
	case v := <-ch:
		return v
	case <-time.After(waitBound):
		files, _ := filepath.Glob(filepath.Join(h.root, "*", "*"))
		h.reg.mu.Lock()
		live := len(h.reg.live)
		h.reg.mu.Unlock()
		stacks := make([]byte, 1<<20)
		stacks = stacks[:runtime.Stack(stacks, true)]
		h.t.Fatalf("waited %v for %s; %d runs live, files %v; the registry logged:\n%s\ngoroutines:\n%s", waitBound, what, live, files, h.log.String(), stacks)
		return nil
	}
}

func await[T any](t *testing.T, ch <-chan T, what string) T {
	t.Helper()
	select {
	case v := <-ch:
		return v
	case <-time.After(waitBound):
		t.Fatalf("waited %v for %s", waitBound, what)
		var zero T
		return zero
	}
}

// harness is a registry on a private root, with its log captured.
type harness struct {
	t    *testing.T
	root string
	log  *bytes.Buffer
	reg  *Registry
	now  time.Time
}

func newHarness(t *testing.T) *harness {
	t.Helper()
	h := &harness{t: t, root: t.TempDir(), log: &bytes.Buffer{}, now: time.Date(2026, 11, 16, 12, 0, 0, 0, time.UTC)}
	h.open()
	return h
}

func (h *harness) logger() *recordlog.Logger {
	return recordlog.New("tests", h.log, &bytes.Buffer{}, sharedlogging.FixedWindow(sharedlogging.LevelDebug), func() time.Time { return h.now })
}

func (h *harness) open() {
	h.t.Helper()
	reg, err := Open(h.root, h.logger(), func() time.Time { return h.now })
	if err != nil {
		h.t.Fatalf("Open: %v", err)
	}
	h.reg = reg
}

// fifo makes a named pipe a run blocks on until the test writes it.
func fifo(t *testing.T) string {
	t.Helper()
	path := filepath.Join(t.TempDir(), "gate")
	if err := syscall.Mkfifo(path, 0o600); err != nil {
		t.Fatalf("mkfifo: %v", err)
	}
	return path
}

// release lets a run blocked on PATH go on.
func release(t *testing.T, path string) {
	t.Helper()
	f, err := os.OpenFile(path, os.O_WRONLY, 0)
	if err != nil {
		t.Fatalf("open the gate: %v", err)
	}
	if _, err := f.WriteString("go\n"); err != nil {
		t.Fatalf("write the gate: %v", err)
	}
	_ = f.Close()
}

func (h *harness) start(script string) string {
	h.t.Helper()
	id, err := h.reg.Start(Spec{Dir: h.t.TempDir(), Argv: []string{"sh", "-c", script}, Label: "ws-1 round 1"})
	if err != nil {
		h.t.Fatalf("Start: %v", err)
	}
	return id
}

// watch collects every event of run ID from line FROM.
func (h *harness) watch(ctx context.Context, id string, from uint64) ([]Event, error) {
	var got []Event
	err := h.reg.Watch(ctx, id, from, func(e Event) error {
		got = append(got, e)
		return nil
	})
	return got, err
}

func lines(events []Event) []string {
	var out []string
	for _, e := range events {
		if e.End == nil {
			out = append(out, e.Text)
		}
	}
	return out
}

func last(events []Event) *End {
	if len(events) == 0 {
		return nil
	}
	return events[len(events)-1].End
}

func TestARunIsWatchedWholeToItsExit(t *testing.T) {
	tests := []struct {
		name  string
		from  uint64
		lines []string
	}{
		{name: "from its start", from: 0, lines: []string{"a", "b", "c"}},
		{name: "from a later line", from: 2, lines: []string{"b", "c"}},
		{name: "from past its last line", from: 9, lines: nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			id := h.start("echo a; echo b >&2; echo c; exit 3")

			// Act
			got, err := h.watch(context.Background(), id, tt.from)

			// Assert
			if err != nil || !slices.Equal(lines(got), tt.lines) {
				t.Fatalf("Watch = (%v, %v), want lines %v", lines(got), err, tt.lines)
			}
			if end := last(got); end == nil || end.ExitCode != 3 || end.Lost || end.Stopped {
				t.Fatalf("end = %+v, want exit 3", end)
			}
		})
	}
}

func TestAWatcherOfARunningRunIsToldEachLineAsItIsWritten(t *testing.T) {
	// Arrange: the run writes one line, then waits on the gate.
	h := newHarness(t)
	gate := fifo(t)
	id := h.start("echo before; read x < " + gate + "; echo after")
	firstSeen := make(chan struct{})
	var got []Event
	done := make(chan error, 1)
	go func() {
		done <- h.reg.Watch(context.Background(), id, 0, func(e Event) error {
			got = append(got, e)
			if e.Text == "before" {
				close(firstSeen)
			}
			return nil
		})
	}()
	await(t, firstSeen, "the run's first line")

	// Act
	release(t, gate)
	err := await(t, done, "the watch's end")

	// Assert
	if err != nil || !slices.Equal(lines(got), []string{"before", "after"}) || last(got) == nil || last(got).ExitCode != 0 {
		t.Fatalf("Watch = (%v, %v), want before then after, then exit 0", got, err)
	}
}

func TestAnEndedRunIsReplayedToALaterWatcher(t *testing.T) {
	// Arrange: the run ends before anybody watches again.
	h := newHarness(t)
	id := h.start("echo one; echo two")
	if _, err := h.watch(context.Background(), id, 0); err != nil {
		t.Fatalf("first Watch: %v", err)
	}

	// Act
	got, err := h.watch(context.Background(), id, 0)

	// Assert
	if err != nil || !slices.Equal(lines(got), []string{"one", "two"}) || last(got) == nil || last(got).ExitCode != 0 {
		t.Fatalf("Watch = (%v, %v), want the run replayed and ended", got, err)
	}
}

func TestAStoppedRunEndsStoppedWithItsGroupKilled(t *testing.T) {
	// Arrange: the run and a child of it wait on the gate forever.
	h := newHarness(t)
	gate := fifo(t)
	id := h.start("echo up; (read y < " + gate + ") & read x < " + gate)
	upSeen := make(chan struct{})
	done := make(chan []Event, 1)
	go func() {
		var got []Event
		_ = h.reg.Watch(context.Background(), id, 0, func(e Event) error {
			got = append(got, e)
			if e.Text == "up" {
				close(upSeen)
			}
			return nil
		})
		done <- got
	}()
	await(t, upSeen, "the run's first line")

	// Act
	err := h.reg.Stop(id)

	// Assert
	got := h.await(done, "the stopped run's end")
	if err != nil || last(got) == nil || !last(got).Stopped || last(got).ExitCode != -1 {
		t.Fatalf("Stop = %v, end %+v; want a stopped run killed by a signal", err, last(got))
	}
}

func TestAStopOfAnEndedRunLeavesItAsItEnded(t *testing.T) {
	// Arrange
	h := newHarness(t)
	id := h.start("exit 0")
	if _, err := h.watch(context.Background(), id, 0); err != nil {
		t.Fatalf("Watch: %v", err)
	}

	// Act
	err := h.reg.Stop(id)

	// Assert
	got, _ := h.watch(context.Background(), id, 0)
	if err != nil || last(got) == nil || last(got).Stopped || last(got).ExitCode != 0 {
		t.Fatalf("Stop = %v, end %+v; want success and the exit it really had", err, last(got))
	}
}

func TestAnUnknownRunIsRefused(t *testing.T) {
	tests := []struct {
		name string
		call func(r *Registry) error
	}{
		{name: "a watch", call: func(r *Registry) error {
			return r.Watch(context.Background(), "nope", 0, func(Event) error { return nil })
		}},
		{name: "a stop", call: func(r *Registry) error { return r.Stop("nope") }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			err := tt.call(h.reg)

			// Assert
			if !errors.Is(err, ErrUnknownRun) {
				t.Fatalf("err = %v, want ErrUnknownRun", err)
			}
		})
	}
}

func TestAnUnstartableCommandLeavesNoRun(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	_, err := h.reg.Start(Spec{Dir: t.TempDir(), Argv: []string{filepath.Join(t.TempDir(), "no-such-program")}, Label: "ws-1"})

	// Assert
	var unstartable *UnstartableError
	if !errors.As(err, &unstartable) {
		t.Fatalf("Start = %v, want an UnstartableError", err)
	}
	if entries, _ := os.ReadDir(h.root); len(entries) != 0 {
		t.Fatalf("root holds %d entries, want no run left", len(entries))
	}
}

func TestARunInheritsNoRepositorySelector(t *testing.T) {
	// Arrange
	h := newHarness(t)
	t.Setenv("GIT_DIR", "/elsewhere/.git")
	id, err := h.reg.Start(Spec{Dir: t.TempDir(), Argv: []string{"sh", "-c", `echo "[${GIT_DIR-unset}] [$EXTRA]"`}, Env: []string{"EXTRA=added"}, Label: "ws-1"})
	if err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Act
	got, err := h.watch(context.Background(), id, 0)

	// Assert
	if err != nil || !slices.Equal(lines(got), []string{"[unset] [added]"}) {
		t.Fatalf("Watch = (%v, %v), want GIT_DIR scrubbed and the added variable set", lines(got), err)
	}
}

func TestAWatchEndsWithItsContext(t *testing.T) {
	// Arrange
	h := newHarness(t)
	gate := fifo(t)
	id := h.start("read x < " + gate)
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act
	_, err := h.watch(ctx, id, 0)

	// Assert
	if !errors.Is(err, context.Canceled) {
		t.Fatalf("Watch = %v, want the context's end", err)
	}
	release(t, gate)
}

func TestOpenSettlesARunAPreviousLifeLeftRunningAsLostAndKillsIt(t *testing.T) {
	// Arrange: a group a previous life recorded, still running.
	h := newHarness(t)
	gate := fifo(t)
	leftover := exec.Command("sh", "-c", "read x < "+gate)
	leftover.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}
	if err := leftover.Start(); err != nil {
		t.Fatalf("start the leftover: %v", err)
	}
	if err := os.MkdirAll(filepath.Join(h.root, "old"), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := writeJSON(filepath.Join(h.root, "old", "run.json"), record{PGID: leftover.Process.Pid}); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(h.root, "old", "output.log"), []byte("partial\n"), 0o644); err != nil {
		t.Fatal(err)
	}

	// Act
	h.open()

	// Assert
	waitErr := leftover.Wait()
	var exit *exec.ExitError
	if !errors.As(waitErr, &exit) || exit.ExitCode() != -1 {
		t.Fatalf("the leftover's wait = %v, want it killed by a signal", waitErr)
	}
	got, err := h.watch(context.Background(), "old", 0)
	if err != nil || !slices.Equal(lines(got), []string{"partial"}) || last(got) == nil || !last(got).Lost {
		t.Fatalf("Watch = (%v, %v), want its partial output and a lost end", got, err)
	}
}

func TestOpenPrunesAnEndedRunPastItsRetention(t *testing.T) {
	tests := []struct {
		name string
		age  time.Duration
		kept bool
	}{
		{name: "a run ended within its retention is kept", age: time.Hour, kept: true},
		{name: "a run ended past its retention is pruned", age: Retention + time.Hour},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			id := h.start("exit 0")
			if _, err := h.watch(context.Background(), id, 0); err != nil {
				t.Fatalf("Watch: %v", err)
			}
			ended := h.now.Add(-tt.age)
			if err := os.Chtimes(filepath.Join(h.root, id, "end.json"), ended, ended); err != nil {
				t.Fatal(err)
			}

			// Act
			h.open()

			// Assert
			_, err := os.Stat(filepath.Join(h.root, id))
			if (err == nil) != tt.kept {
				t.Fatalf("run directory stat = %v, want kept %v", err, tt.kept)
			}
		})
	}
}
