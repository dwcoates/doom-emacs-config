package main

import (
	"bufio"
	"errors"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"syscall"
	"testing"
)

// helperEnv makes the compiled TEST BINARY act as shim-lock: TestMain sees the
// variable and runs the real `run` instead of the suite. Re-executing ourselves
// is what lets these tests exercise REAL child processes and REAL kernel locks
// without a `go build` step or a staged artifact.
const helperEnv = "SHIM_LOCK_TEST_HELPER_ARGS"

// helperNoArgs asks the helper to run with an EMPTY argument list, which no
// non-empty value of helperEnv could express.
const helperNoArgs = "\x00none"

func TestMain(m *testing.M) {
	if spec, ok := os.LookupEnv(helperEnv); ok {
		args := strings.Split(spec, "\x00")
		if spec == helperNoArgs {
			args = nil
		}
		os.Exit(run(args, os.Stdin, os.Stdout, os.Stderr))
	}
	os.Exit(m.Run())
}

// holder is a live shim-lock child plus the pipes the protocol runs over.
type holder struct {
	cmd    *exec.Cmd
	stdin  io.WriteCloser
	stdout *bufio.Reader
	stderr *strings.Builder
	waited error
	done   bool
}

// startHolder spawns the helper against args and returns it running. It does
// NOT wait for the ready line; each test decides what it is synchronizing on.
func startHolder(t *testing.T, args ...string) *holder {
	t.Helper()
	spec := helperNoArgs
	if len(args) > 0 {
		spec = strings.Join(args, "\x00")
	}
	cmd := exec.Command(os.Args[0])
	cmd.Env = append(os.Environ(), helperEnv+"="+spec)
	stdin, err := cmd.StdinPipe()
	if err != nil {
		t.Fatalf("stdin pipe: %v", err)
	}
	stdout, err := cmd.StdoutPipe()
	if err != nil {
		t.Fatalf("stdout pipe: %v", err)
	}
	var stderr strings.Builder
	cmd.Stderr = &stderr
	if err := cmd.Start(); err != nil {
		t.Fatalf("start the holder: %v", err)
	}
	h := &holder{cmd: cmd, stdin: stdin, stdout: bufio.NewReader(stdout), stderr: &stderr}
	t.Cleanup(func() {
		if h.done {
			return
		}
		_ = h.cmd.Process.Kill()
		_ = h.cmd.Wait()
	})
	return h
}

// awaitReady blocks until the holder announces the claim, which is the only
// synchronization point the protocol offers — and the only one these tests use.
func (h *holder) awaitReady(t *testing.T) {
	t.Helper()
	line, err := h.stdout.ReadString('\n')
	if err != nil {
		t.Fatalf("reading the ready line: %v (stderr: %s)", err, h.stderr.String())
	}
	if strings.TrimSpace(line) != ReadyLine {
		t.Fatalf("ready line = %q, want %q", strings.TrimSpace(line), ReadyLine)
	}
}

// awaitExit reaps the holder and answers its exit code.
func (h *holder) awaitExit(t *testing.T) int {
	t.Helper()
	if !h.done {
		h.waited = h.cmd.Wait()
		h.done = true
	}
	var exitErr *exec.ExitError
	if h.waited == nil {
		return 0
	}
	if errors.As(h.waited, &exitErr) {
		return exitErr.ExitCode()
	}
	t.Fatalf("waiting for the holder: %v", h.waited)
	return -1
}

// probeBusy answers what the DAEMON's probe would answer: true when
// flock(LOCK_EX|LOCK_NB) is refused, which is the only thing that makes this
// binary useful. It is the same call daemon/internal/sessionlock.Probe makes.
func probeBusy(t *testing.T, lockPath string) bool {
	t.Helper()
	f, err := os.OpenFile(lockPath, os.O_RDWR|os.O_CREATE, 0o600)
	if err != nil {
		t.Fatalf("probe open %s: %v", lockPath, err)
	}
	defer f.Close()
	err = syscall.Flock(int(f.Fd()), syscall.LOCK_EX|syscall.LOCK_NB)
	if err == nil {
		if err := syscall.Flock(int(f.Fd()), syscall.LOCK_UN); err != nil {
			t.Fatalf("probe unlock %s: %v", lockPath, err)
		}
		return false
	}
	if errors.Is(err, syscall.EWOULDBLOCK) || errors.Is(err, syscall.EAGAIN) {
		return true
	}
	t.Fatalf("probe flock %s: %v", lockPath, err)
	return false
}

func TestHolderTakesAFreeLockAndTheGoProbeSeesItBusy(t *testing.T) {
	// Arrange
	lockPath := filepath.Join(t.TempDir(), "run", "workspace-5c78c72d.lock")

	// Act
	h := startHolder(t, lockPath)
	h.awaitReady(t)

	// Assert: the claim is a REAL kernel flock, so the daemon's probe is refused.
	if !probeBusy(t, lockPath) {
		t.Fatal("the Go probe took the lock while the holder claims to hold it")
	}
}

func TestHolderCreatesTheLockDirectoryItWasPointedAt(t *testing.T) {
	// Arrange: the run directory does not exist yet, which is the state of a
	// machine that has never started a session.
	lockPath := filepath.Join(t.TempDir(), "never", "made", "session-s1.lock")

	// Act
	h := startHolder(t, lockPath)
	h.awaitReady(t)

	// Assert
	if _, err := os.Stat(lockPath); err != nil {
		t.Fatalf("the lock file was not created: %v", err)
	}
}

func TestSecondHolderIsRefusedWithTheDistinctHeldCode(t *testing.T) {
	// Arrange: a first holder owns the lock.
	lockPath := filepath.Join(t.TempDir(), "held.lock")
	first := startHolder(t, lockPath)
	first.awaitReady(t)

	// Act
	second := startHolder(t, lockPath)
	code := second.awaitExit(t)

	// Assert: the code is the one the shim reads as "another shim owns this",
	// and the reason is on stderr rather than only in the code.
	if code != exitHeld {
		t.Fatalf("second holder exit = %d, want %d (stderr: %s)", code, exitHeld, second.stderr.String())
	}
	if !strings.Contains(second.stderr.String(), "already held by another process") {
		t.Fatalf("second holder stderr = %q, want the held diagnostic", second.stderr.String())
	}
}

func TestSecondHolderNeverAnnouncesAClaimItDidNotMake(t *testing.T) {
	// Arrange
	lockPath := filepath.Join(t.TempDir(), "held.lock")
	first := startHolder(t, lockPath)
	first.awaitReady(t)

	// Act: stdout is drained to EOF BEFORE the child is reaped, because Wait
	// closes the pipe under any reader still on it.
	second := startHolder(t, lockPath)
	out, err := io.ReadAll(second.stdout)
	second.awaitExit(t)

	// Assert: stdout is the readiness protocol, so a refused holder must leave
	// it empty — a ready line here would let the shim start over a lock it
	// does not hold.
	if err != nil {
		t.Fatalf("draining the refused holder's stdout: %v", err)
	}
	if len(out) != 0 {
		t.Fatalf("refused holder stdout = %q, want nothing", out)
	}
}

func TestHolderReleasesTheLockOnStdinEOF(t *testing.T) {
	// Arrange
	lockPath := filepath.Join(t.TempDir(), "released.lock")
	h := startHolder(t, lockPath)
	h.awaitReady(t)

	// Act: closing stdin is the shim's deliberate release.
	if err := h.stdin.Close(); err != nil {
		t.Fatalf("closing the holder's stdin: %v", err)
	}
	code := h.awaitExit(t)

	// Assert: a clean exit, and the lock is free for the next shim.
	if code != exitOK {
		t.Fatalf("exit = %d, want %d (stderr: %s)", code, exitOK, h.stderr.String())
	}
	if probeBusy(t, lockPath) {
		t.Fatal("the lock is still held after the holder exited")
	}
}

func TestHolderReleasesTheLockWhenItIsKilled(t *testing.T) {
	// Arrange: SIGKILL is the death the shim cannot clean up after, and the
	// whole reason the claim is a kernel lock rather than a pid file.
	lockPath := filepath.Join(t.TempDir(), "killed.lock")
	h := startHolder(t, lockPath)
	h.awaitReady(t)

	// Act
	if err := h.cmd.Process.Kill(); err != nil {
		t.Fatalf("killing the holder: %v", err)
	}
	h.awaitExit(t)

	// Assert
	if probeBusy(t, lockPath) {
		t.Fatal("the lock survived the holder's death; it is not kernel-released")
	}
}

// THE LOCK OUTLIVES A SIGNAL TO THE SHIM'S PROCESS GROUP: a SIGTERM starts the
// shim's graceful stand-down, during which its session still runs, so the
// holder keeps the lock until the shim's own end closes its stdin.
func TestHolderKeepsTheLockThroughATerminationSignal(t *testing.T) {
	tests := []struct {
		name   string
		signal syscall.Signal
	}{
		{"SIGTERM", syscall.SIGTERM},
		{"SIGINT", syscall.SIGINT},
		{"SIGHUP", syscall.SIGHUP},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			lockPath := filepath.Join(t.TempDir(), "signalled.lock")
			h := startHolder(t, lockPath)
			h.awaitReady(t)

			// Act
			if err := h.cmd.Process.Signal(tt.signal); err != nil {
				t.Fatalf("signalling the holder: %v", err)
			}
			if err := h.stdin.Close(); err != nil {
				t.Fatalf("closing the holder's stdin: %v", err)
			}
			code := h.awaitExit(t)

			// Assert: it was still there to release on EOF, cleanly.
			if code != exitOK {
				t.Fatalf("exit = %d, want %d: the signal ended the holder (stderr: %s)", code, exitOK, h.stderr.String())
			}
		})
	}
}

func TestSecondHolderTakesTheLockTheFirstReleased(t *testing.T) {
	// Arrange
	lockPath := filepath.Join(t.TempDir(), "recycled.lock")
	first := startHolder(t, lockPath)
	first.awaitReady(t)
	if err := first.stdin.Close(); err != nil {
		t.Fatalf("closing the first holder's stdin: %v", err)
	}
	first.awaitExit(t)

	// Act
	second := startHolder(t, lockPath)

	// Assert: the ready line is the proof; it never arrives if the lock is busy.
	second.awaitReady(t)
}

func TestRunRefusesArgumentsThatAreNotOneLockPath(t *testing.T) {
	tests := []struct {
		name string
		args []string
	}{
		{name: "no arguments at all", args: nil},
		{name: "an empty path", args: []string{""}},
		{name: "a blank path", args: []string{"   "}},
		{name: "two paths, which would claim only one", args: []string{"/a.lock", "/b.lock"}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			var stdout, stderr strings.Builder

			// Act
			code := run(tc.args, strings.NewReader(""), &stdout, &stderr)

			// Assert
			if code != exitUsage {
				t.Fatalf("exit = %d, want %d", code, exitUsage)
			}
			if stdout.String() != "" {
				t.Fatalf("stdout = %q, want nothing", stdout.String())
			}
			if !strings.Contains(stderr.String(), "exactly one argument") {
				t.Fatalf("stderr = %q, want the usage diagnostic", stderr.String())
			}
		})
	}
}

func TestRunFailsLoudlyWhenTheLockPathCannotBeOpened(t *testing.T) {
	// Arrange: a regular file standing where the lock DIRECTORY would go, so
	// the directory cannot be created. This must not read as "held".
	root := t.TempDir()
	blocker := filepath.Join(root, "run")
	if err := os.WriteFile(blocker, []byte("not a directory"), 0o600); err != nil {
		t.Fatalf("writing the blocker: %v", err)
	}
	var stdout, stderr strings.Builder

	// Act
	code := run([]string{filepath.Join(blocker, "a.lock")}, strings.NewReader(""), &stdout, &stderr)

	// Assert
	if code != exitError {
		t.Fatalf("exit = %d, want %d", code, exitError)
	}
	if !strings.Contains(stderr.String(), "lock directory could not be created") {
		t.Fatalf("stderr = %q, want the directory diagnostic", stderr.String())
	}
}

func TestRunReportsAStderrSinkThatSwallowedItsRecords(t *testing.T) {
	// Arrange: a stderr that fails every write. The lock is taken and released
	// normally, so the ONLY thing wrong is that the diagnostics went nowhere.
	lockPath := filepath.Join(t.TempDir(), "quiet.lock")
	var stdout strings.Builder

	// Act
	code := run([]string{lockPath}, strings.NewReader(""), &stdout, failingWriter{})

	// Assert: silence is reported, not shrugged off.
	if code != exitError {
		t.Fatalf("exit = %d, want %d", code, exitError)
	}
}

type failingWriter struct{}

func (failingWriter) Write([]byte) (int, error) { return 0, errors.New("sink is gone") }

func TestRunFailsLoudlyWhenTheLockPathIsADirectory(t *testing.T) {
	// Arrange: the lock path itself already exists as a DIRECTORY, so the
	// directory creation succeeds and the OPEN is what fails. That refusal
	// must stay distinct from "another process holds this".
	lockPath := filepath.Join(t.TempDir(), "already-a-dir.lock")
	if err := os.Mkdir(lockPath, 0o755); err != nil {
		t.Fatalf("planting the directory: %v", err)
	}
	var stdout, stderr strings.Builder

	// Act
	code := run([]string{lockPath}, strings.NewReader(""), &stdout, &stderr)

	// Assert
	if code != exitError {
		t.Fatalf("exit = %d, want %d", code, exitError)
	}
	if !strings.Contains(stderr.String(), "lock file could not be opened") {
		t.Fatalf("stderr = %q, want the open diagnostic", stderr.String())
	}
}

func TestRunFailsWhenTheReadyLineCannotBeWritten(t *testing.T) {
	// Arrange: the lock is takeable, but stdout is gone. A claim the shim can
	// never be told about is a failure, not a hold.
	lockPath := filepath.Join(t.TempDir(), "unannounceable.lock")
	var stderr strings.Builder

	// Act
	code := run([]string{lockPath}, strings.NewReader(""), failingWriter{}, &stderr)

	// Assert
	if code != exitError {
		t.Fatalf("exit = %d, want %d", code, exitError)
	}
	if !strings.Contains(stderr.String(), "ready line could not be written") {
		t.Fatalf("stderr = %q, want the ready-line diagnostic", stderr.String())
	}
}

func TestRunFailsWhenTheHoldingReadOfStdinFails(t *testing.T) {
	// Arrange: the lock is held and announced, then the stdin pipe errors
	// rather than reaching EOF. That is not a deliberate release.
	lockPath := filepath.Join(t.TempDir(), "broken-stdin.lock")
	var stdout, stderr strings.Builder

	// Act
	code := run([]string{lockPath}, failingReader{}, &stdout, &stderr)

	// Assert
	if code != exitError {
		t.Fatalf("exit = %d, want %d", code, exitError)
	}
	if strings.TrimSpace(stdout.String()) != ReadyLine {
		t.Fatalf("stdout = %q, want the ready line to have gone out first", stdout.String())
	}
	if !strings.Contains(stderr.String(), "reading stdin failed") {
		t.Fatalf("stderr = %q, want the stdin diagnostic", stderr.String())
	}
}

type failingReader struct{}

func (failingReader) Read([]byte) (int, error) { return 0, errors.New("stdin pipe is gone") }
