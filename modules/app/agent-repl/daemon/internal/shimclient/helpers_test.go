package shimclient

import (
	"bufio"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"os/signal"
	"path/filepath"
	"runtime"
	"strconv"
	"strings"
	"syscall"
	"testing"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
)

// The helper-process switches. A test spawns THIS test binary as the shim, so
// the spawn contract can be asserted from inside the child.
const (
	helperModeEnv = "AGENT_REPL_SHIMCLIENT_HELPER"
	helperExitEnv = "AGENT_REPL_SHIMCLIENT_HELPER_EXIT"
	helperErrEnv  = "AGENT_REPL_SHIMCLIENT_HELPER_STDERR"

	// helperIdle records itself and then lives until it is signaled.
	helperIdle = "idle"
	// helperDie records itself, writes to stderr, and exits.
	helperDie = "die"
	// helperIgnoreTerm ignores SIGTERM, so the kill must escalate.
	helperIgnoreTerm = "ignore-term"
)

// helperRecord is what the child writes to fd 3 — the log sink the parent
// passed it — so the parent can assert the spawn contract exactly.
type helperRecord struct {
	Argv []string          `json:"argv"`
	Env  map[string]string `json:"env"`
	Cwd  string            `json:"cwd"`
	PID  int               `json:"pid"`
	PGID int               `json:"pgid"`
}

// TestMain runs the helper child when the switch is set, and the tests
// otherwise.
func TestMain(m *testing.M) {
	if mode := os.Getenv(helperModeEnv); mode != "" {
		runHelper(mode)
		return
	}
	os.Exit(m.Run())
}

// runHelper is the child: it reports the spawn contract it observed on fd 3,
// then behaves as its mode says.
func runHelper(mode string) {
	if mode == helperIgnoreTerm {
		// Installed BEFORE the record is written, so a parent that has read the
		// record knows the disposition is already in place.
		signal.Ignore(syscall.SIGTERM)
	}

	env := map[string]string{}
	for _, entry := range os.Environ() {
		name, value, ok := strings.Cut(entry, "=")
		if ok {
			env[name] = value
		}
	}
	cwd, _ := os.Getwd()
	pgid, _ := syscall.Getpgid(os.Getpid())
	record := helperRecord{Argv: os.Args, Env: env, Cwd: cwd, PID: os.Getpid(), PGID: pgid}

	sink := os.NewFile(shimLogFD, "shim-log")
	if sink != nil {
		payload, err := json.Marshal(record)
		if err == nil {
			_, _ = sink.Write(append(payload, '\n'))
			_ = sink.Sync()
		}
	}

	switch mode {
	case helperDie:
		if text := os.Getenv(helperErrEnv); text != "" {
			_, _ = fmt.Fprintln(os.Stderr, text)
		}
		code, _ := strconv.Atoi(os.Getenv(helperExitEnv))
		os.Exit(code)
	default:
		blockForever()
	}
}

// blockForever parks the helper in a real read syscall, which is immune to the
// runtime's all-goroutines-asleep detector: the child must stay alive until it
// is signaled, never exit on its own.
func blockForever() {
	r, w, err := os.Pipe()
	if err != nil {
		os.Exit(1)
	}
	defer runtime.KeepAlive(w)
	var b [1]byte
	_, _ = r.Read(b[:])
	os.Exit(0)
}

// testSurfaces is the dlog.Surfaces every test hands the supervisor: one
// capturing logger for every sink.
type testSurfaces struct {
	log *dlog.TestLogger
}

func newTestSurfaces() testSurfaces { return testSurfaces{log: dlog.NewTestLogger()} }

func (s testSurfaces) Global() dlog.Logger { return s.log }

func (s testSurfaces) Workspace(string) (dlog.Logger, error) { return s.log, nil }

func (s testSurfaces) WorkspaceOrCentral(string) dlog.Logger { return s.log }

func (s testSurfaces) ShimSink(string) (dlog.Borrowed, error) {
	return nil, errors.New("testSurfaces: ShimSink is not used by shimclient tests")
}

// BindWorkspaceIDs implements dlog.Surfaces. This double answers its own
// workspace ids, so there is no lookup to install.
func (s testSurfaces) BindWorkspaceIDs(dlog.WorkspaceIDLookup) {}

func (s testSurfaces) ShimRollRequests() <-chan dlog.ShimRollRequest { return nil }

// BindRecordTee implements dlog.Surfaces; this double tees nothing.
func (s testSurfaces) BindRecordTee(dlog.RecordTee) {}

func (s testSurfaces) DetachDir(string) error { return nil }

func (s testSurfaces) AttachDir(string) error { return nil }

func (s testSurfaces) ClientLog(string, dlog.ClientRecord) error { return nil }

func (s testSurfaces) Close() error { return nil }

// testBackoff is the instant-enough redial schedule every test uses: fast, and
// never a hot spin.
func testBackoff() Option { return WithBackoff(time.Millisecond, 5*time.Millisecond, 2) }

// shortDir is a temp directory whose path is short enough for a unix socket:
// macOS caps sun_path at 104 bytes and $TMPDIR is nowhere near short enough.
func shortDir(t *testing.T) string {
	t.Helper()

	dir, err := os.MkdirTemp("/tmp", "sc")
	if err != nil {
		t.Fatalf("MkdirTemp: %v", err)
	}
	t.Cleanup(func() { _ = os.RemoveAll(dir) })
	return dir
}

// helperSink is the read end of the pipe the child's fd 3 writes to. Reading it
// BLOCKS until the child has reported itself, so no test ever polls.
type helperSink struct {
	r *os.File
}

// record blocks for the record the child wrote to fd 3.
func (h *helperSink) record(t *testing.T) helperRecord {
	t.Helper()

	line, err := bufio.NewReader(h.r).ReadString('\n')
	if err != nil {
		t.Fatalf("read the child's fd 3 record: %v", err)
	}
	var record helperRecord
	if err := json.Unmarshal([]byte(strings.TrimSpace(line)), &record); err != nil {
		t.Fatalf("decode helper record %q: %v", line, err)
	}
	return record
}

// newTestSpec builds a valid Spec whose shim is this test binary in the given
// helper mode. fd 3 is a pipe the test reads, which both proves fd 3 was the
// sink the daemon passed and synchronizes on the child having started.
func newTestSpec(t *testing.T, dir, udsPath, mode string) (Spec, *helperSink) {
	t.Helper()

	self, err := os.Executable()
	if err != nil {
		t.Fatalf("os.Executable(): %v", err)
	}
	r, w, err := os.Pipe()
	if err != nil {
		t.Fatalf("os.Pipe(): %v", err)
	}
	t.Cleanup(func() { r.Close(); w.Close() })

	t.Setenv(helperModeEnv, mode)
	spec := Spec{
		WorkspaceID:  ids.WorkspaceID("ws-1"),
		WorkspaceDir: dir,
		UDSPath:      udsPath,
		StoreSocket:  filepath.Join(dir, "store.sock"),
		ConfigDir:    filepath.Join(dir, "account"),
		ShimBuildSHA: "build-sha-1",
		NodeBin:      self,
		MainJS:       filepath.Join(dir, "main.js"),
		Fake:         true,
		LogSink:      w,
		StateDir:     filepath.Join(dir, "state"),
	}
	return spec, &helperSink{r: r}
}

// spawnReady spawns a shim against the in-process fake and returns once the
// client is ready: the fake pushes the healthy diagnostics the readiness rule
// waits for, as soon as the client's session stream opens.
func spawnReady(t *testing.T, f *fakeShim, spec Spec, opts ...Option) Client {
	t.Helper()

	sup := newSupervisor(t, opts...)
	type result struct {
		c   Client
		err error
	}
	done := make(chan result, 1)
	go func() {
		c, err := sup.Spawn(context.Background(), spec)
		done <- result{c: c, err: err}
	}()

	waitForSessionOpen(t, f)
	f.push(healthyUpdate())

	r := <-done
	if r.err != nil {
		t.Fatalf("Spawn() error = %v", r.err)
	}
	t.Cleanup(func() { _ = r.c.Kill(context.Background(), KillAttribution{Actor: "test", Reason: "cleanup"}) })
	return r.c
}

// adoptReady adopts the in-process fake and returns once the client is ready.
func adoptReady(t *testing.T, f *fakeShim, dir, udsPath string, opts ...Option) Client {
	t.Helper()

	sup := newSupervisor(t, opts...)
	type result struct {
		c   Client
		err error
	}
	done := make(chan result, 1)
	go func() {
		c, err := sup.Adopt(context.Background(), ids.WorkspaceID("ws-1"), dir, udsPath)
		done <- result{c: c, err: err}
	}()

	waitForSessionOpen(t, f)
	f.push(healthyUpdate())

	r := <-done
	if r.err != nil {
		t.Fatalf("Adopt() error = %v", r.err)
	}
	t.Cleanup(r.c.Detach)
	return r.c
}

// newSupervisor builds a supervisor with the test backoff plus any extra
// options.
func newSupervisor(t *testing.T, opts ...Option) Supervisor {
	t.Helper()

	sup, _ := newSupervisorLogging(t, opts...)
	return sup
}

// newSupervisorLogging is newSupervisor for a test that reads the records the
// supervisor wrote, rather than only its behavior.
func newSupervisorLogging(t *testing.T, opts ...Option) (Supervisor, testSurfaces) {
	t.Helper()

	surfaces := newTestSurfaces()
	sup, err := NewSupervisor(surfaces, append([]Option{testBackoff()}, opts...)...)
	if err != nil {
		t.Fatalf("NewSupervisor() error = %v", err)
	}
	return sup, surfaces
}

// waitForSessionOpen blocks until the fake has an open WatchSession stream.
func waitForSessionOpen(t *testing.T, f *fakeShim) {
	t.Helper()

	select {
	case <-f.opened:
	case <-time.After(10 * time.Second):
		t.Fatal("no WatchSession stream was opened")
	}
}

// collectStates reads n link states, failing rather than hanging forever.
func collectStates(t *testing.T, c Client, n int) []LinkState {
	t.Helper()

	states := make([]LinkState, 0, n)
	for len(states) < n {
		select {
		case state, ok := <-c.Connectivity():
			if !ok {
				t.Fatalf("connectivity closed after %v, wanted %d states", states, n)
			}
			states = append(states, state)
		case <-time.After(10 * time.Second):
			t.Fatalf("only %v arrived, wanted %d states", states, n)
		}
	}
	return states
}

// alive reports whether a pid is still there.
func alive(pid int) bool { return syscall.Kill(pid, syscall.Signal(0)) == nil }

// Evict satisfies dlog.Surfaces for the merged seam (the bootinfra agent added it).
func (s testSurfaces) Evict(_ string) error { return nil }

// bringUpProbeWindow is the NEGATIVE bound: how long a test waits to be
// satisfied that bring-up has NOT returned. The fake shim is in-process and
// every measured bring-up in this package answers in single-digit
// milliseconds, so 200ms is a wide multiple of the behavior it rules out and
// is paid in full on a green run at exactly one site.
const bringUpProbeWindow = 200 * time.Millisecond

// bringUpAnswerBound is the POSITIVE bound: how long a bring-up that the shim
// has already answered may take to return. Same measured basis, and it is
// paid only by a red test.
const bringUpAnswerBound = 2 * time.Second

// sweepReapBound is the kill grace a sweep test hands its supervisor, and it
// is a FAILURE bound only. StandDownEverySpawn forces its kill, so no SIGTERM
// grace is ever spent: the grace's one remaining role is bounding the wait for
// the reaper's exit decode, and that wait returns the instant the real reap
// lands. At 50ms it was a success-path deadline instead, and a scheduler
// pause between the SIGKILL and the reap (reproduced 2/500 under a 20ms
// SIGSTOP/SIGCONT loop at GOMAXPROCS=1) turned a clean kill into a reported
// overrun. Same measured basis as bringUpAnswerBound; paid only by a red test.
const sweepReapBound = 2 * time.Second

// spawnResult is one asynchronous Spawn's outcome.
type spawnResult struct {
	c   Client
	err error
}

// spawnAsync starts a Spawn on its own goroutine so the test can push the
// frames bring-up is waiting for and then observe whether it returned.
func spawnAsync(t *testing.T, sup Supervisor, spec Spec) <-chan spawnResult {
	t.Helper()

	done := make(chan spawnResult, 1)
	go func() {
		c, err := sup.Spawn(context.Background(), spec)
		done <- spawnResult{c: c, err: err}
	}()
	return done
}

// adoptTestClient fails the test on a spawn error and arms the kill that
// leaves no process behind.
func adoptTestClient(t *testing.T, r spawnResult) Client {
	t.Helper()

	if r.err != nil {
		t.Fatalf("Spawn() error = %v", r.err)
	}
	t.Cleanup(func() { _ = r.c.Kill(context.Background(), KillAttribution{Actor: "test", Reason: "cleanup"}) })
	return r.c
}
