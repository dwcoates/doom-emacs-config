package run

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"os/exec"
	"os/signal"
	"path/filepath"
	"strconv"
	"strings"
	"syscall"
	"testing"
	"time"
)

// OSExec's units are this test binary re-executed in a helper mode (the
// standard library's own pattern for testing os/exec): no external program
// runs. Every rendezvous between a test and its helpers is a FIFO, whose open
// and EOF the kernel orders, never a sleep or a poll.
const helperEnv = "TESTRUN_EXEC_HELPER"

func TestMain(m *testing.M) {
	if mode := os.Getenv(helperEnv); mode != "" {
		os.Exit(runHelper(mode))
	}
	os.Exit(m.Run())
}

// runHelper is a unit's whole life in one helper mode.
func runHelper(mode string) int {
	verb, arg, _ := strings.Cut(mode, ":")
	switch verb {
	case "exit":
		fmt.Fprint(os.Stdout, "to stdout\n")
		fmt.Fprint(os.Stderr, "to stderr\n")
		code, _ := strconv.Atoi(arg)
		return code
	case "group":
		// Start a grandchild, say so, then block until killed.
		child := exec.Command(os.Args[0])
		child.Env = append(os.Environ(), helperEnv+"=grandchild")
		if err := child.Start(); err != nil {
			fmt.Fprintln(os.Stderr, err)
			return 2
		}
		announce()
		blockForever()
	case "grandchild":
		// Hold the held FIFO's write end for as long as this process lives.
		held, err := os.OpenFile(os.Getenv("TESTRUN_HELD_FIFO"), os.O_WRONLY, 0)
		if err != nil {
			fmt.Fprintln(os.Stderr, err)
			return 2
		}
		defer held.Close()
		blockForever()
	case "tmpdir":
		// Say where TMPDIR points, and leave a file there as a leaky test would.
		dir := os.Getenv("TMPDIR")
		if err := os.WriteFile(filepath.Join(dir, "left-behind"), []byte("x"), 0o600); err != nil {
			fmt.Fprintln(os.Stderr, err)
			return 2
		}
		fmt.Fprintf(os.Stdout, "TMPDIR=%s\n", dir)
		return 0
	case "unremovable":
		// Leave a directory nobody can list, which RemoveAll cannot descend.
		dir := filepath.Join(os.Getenv("TMPDIR"), "locked")
		if err := os.MkdirAll(filepath.Join(dir, "inner"), 0o700); err != nil {
			fmt.Fprintln(os.Stderr, err)
			return 2
		}
		if err := os.Chmod(dir, 0); err != nil {
			fmt.Fprintln(os.Stderr, err)
			return 2
		}
		return 0
	case "ignore-term":
		signal.Ignore(syscall.SIGTERM)
		announce()
		blockForever()
	}
	fmt.Fprintf(os.Stderr, "unknown helper mode %q\n", mode)
	return 2
}

// announce writes one line to the ready FIFO, which the test is reading.
func announce() {
	f, err := os.OpenFile(os.Getenv("TESTRUN_READY_FIFO"), os.O_WRONLY, 0)
	if err != nil {
		panic(err)
	}
	fmt.Fprintln(f, "ready")
	f.Close()
}

// blockForever opens a FIFO no process ever writes, which blocks in the
// kernel until a signal ends the process.
func blockForever() {
	f, err := os.Open(os.Getenv("TESTRUN_NEVER_FIFO"))
	if err != nil {
		panic(err)
	}
	panic(fmt.Sprintf("the never-written FIFO opened: %v", f.Name()))
}

func mkfifo(t *testing.T, dir, name string) string {
	t.Helper()
	p := filepath.Join(dir, name)
	if err := syscall.Mkfifo(p, 0o600); err != nil {
		t.Fatalf("mkfifo %s: %v", p, err)
	}
	return p
}

type fifos struct{ ready, never, held string }

func newFifos(t *testing.T) fifos {
	dir := t.TempDir()
	return fifos{ready: mkfifo(t, dir, "ready"), never: mkfifo(t, dir, "never"), held: mkfifo(t, dir, "held")}
}

func (f fifos) env(mode string) []string {
	return []string{helperEnv + "=" + mode, "TESTRUN_READY_FIFO=" + f.ready, "TESTRUN_NEVER_FIFO=" + f.never, "TESTRUN_HELD_FIFO=" + f.held}
}

// awaitReady reads the helper's announcement.
func (f fifos) awaitReady(t *testing.T) {
	t.Helper()
	r, err := os.Open(f.ready)
	if err != nil {
		t.Fatalf("open the ready FIFO: %v", err)
	}
	defer r.Close()
	if b, err := io.ReadAll(r); err != nil || string(b) != "ready\n" {
		t.Fatalf("ready FIFO said %q, %v", b, err)
	}
}

func helperSpec(env []string) Spec {
	s := Spec{Argv: []string{os.Args[0]}, Env: env}
	s.ID, s.Suite = "u#00", "s"
	return s
}

func newExecLog() (*Log, *bytes.Buffer) {
	var errs bytes.Buffer
	return &Log{Out: io.Discard, Err: &errs}, &errs
}

// waitBounded is Wait with a failure deadline, so a broken kill fails the
// test instead of hanging the whole run.
func waitBounded(t *testing.T, p Process) (int, error) {
	t.Helper()
	type result struct {
		exit int
		err  error
	}
	ch := make(chan result, 1)
	go func() {
		exit, _, err := p.Wait()
		ch <- result{exit, err}
	}()
	select {
	case r := <-ch:
		return r.exit, r.err
	case <-time.After(30 * time.Second):
		t.Fatal("the unit did not exit")
		return 0, nil
	}
}

func TestOSExecReportsTheExitStatusAndBothStreams(t *testing.T) {
	tests := []struct {
		name string
		code int
	}{
		{"a passing unit", 0},
		{"a failing unit", 3},
		{"a declining unit", ExitDeclined},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			log, errs := newExecLog()
			out := &bytes.Buffer{}

			// Act
			p, err := OSExec{Log: log, Grace: time.Second, TmpParent: t.TempDir()}.Start(helperSpec([]string{helperEnv + "=exit:" + strconv.Itoa(tt.code)}), out)
			if err != nil {
				t.Fatal(err)
			}
			exit, err := waitBounded(t, p)

			// Assert
			if err != nil || exit != tt.code {
				t.Fatalf("Wait = %d, %v; want %d", exit, err, tt.code)
			}
			if got := out.String(); !strings.Contains(got, "to stdout\n") || !strings.Contains(got, "to stderr\n") {
				t.Fatalf("output = %q, want both streams", got)
			}
			if errs.Len() != 0 {
				t.Fatalf("logged %q", errs)
			}
		})
	}
}

func TestOSExecKillStopsTheUnitsWholeProcessGroup(t *testing.T) {
	// Arrange: a unit that has started a grandchild holding the held FIFO.
	f := newFifos(t)
	log, errs := newExecLog()
	p, err := OSExec{Log: log, Grace: KillGrace, TmpParent: t.TempDir()}.Start(helperSpec(f.env("group")), &bytes.Buffer{})
	if err != nil {
		t.Fatal(err)
	}
	f.awaitReady(t)
	held, err := os.Open(f.held) // returns once the grandchild holds the write end
	if err != nil {
		t.Fatalf("open the held FIFO: %v", err)
	}
	defer held.Close()

	// Act
	p.Kill()
	exit, err := waitBounded(t, p)

	// Assert: the unit died of SIGTERM, and so did the grandchild, whose
	// death is the held FIFO's EOF.
	if err != nil || exit != 128+int(syscall.SIGTERM) {
		t.Fatalf("Wait = %d, %v; want SIGTERM's %d", exit, err, 128+int(syscall.SIGTERM))
	}
	eof := make(chan error, 1)
	go func() { _, err := io.ReadAll(held); eof <- err }()
	select {
	case err := <-eof:
		if err != nil {
			t.Fatalf("read the held FIFO: %v", err)
		}
	case <-time.After(30 * time.Second):
		t.Fatal("the grandchild outlived the kill: its process group was not signalled")
	}
	if errs.Len() != 0 {
		t.Fatalf("logged %q", errs)
	}
}

func TestOSExecKillEscalatesToSIGKILLAfterTheGrace(t *testing.T) {
	// Arrange: a unit that ignores SIGTERM.
	f := newFifos(t)
	log, errs := newExecLog()
	p, err := OSExec{Log: log, Grace: 10 * time.Millisecond, TmpParent: t.TempDir()}.Start(helperSpec(f.env("ignore-term")), &bytes.Buffer{})
	if err != nil {
		t.Fatal(err)
	}
	f.awaitReady(t)

	// Act
	p.Kill()
	exit, err := waitBounded(t, p)

	// Assert
	if err != nil || exit != 128+int(syscall.SIGKILL) {
		t.Fatalf("Wait = %d, %v; want SIGKILL's %d", exit, err, 128+int(syscall.SIGKILL))
	}
	if errs.Len() != 0 {
		t.Fatalf("logged %q", errs)
	}
}

func TestOSExecKillOfAnExitedUnitLogsNothing(t *testing.T) {
	// Arrange
	log, errs := newExecLog()
	p, err := OSExec{Log: log, Grace: time.Second, TmpParent: t.TempDir()}.Start(helperSpec([]string{helperEnv + "=exit:0"}), &bytes.Buffer{})
	if err != nil {
		t.Fatal(err)
	}
	if _, err := waitBounded(t, p); err != nil {
		t.Fatal(err)
	}

	// Act
	p.Kill()

	// Assert: the group is gone (ESRCH), which is nothing to report.
	if errs.Len() != 0 {
		t.Fatalf("logged %q", errs)
	}
}

func TestOSExecStartRefusals(t *testing.T) {
	log, _ := newExecLog()
	parent := t.TempDir()
	tests := []struct {
		name    string
		exec    OSExec
		argv    []string
		wantErr string
	}{
		{name: "no log", exec: OSExec{Grace: time.Second, TmpParent: parent}, argv: []string{os.Args[0]}, wantErr: "needs a Log, a positive Grace and a TmpParent"},
		{name: "no grace", exec: OSExec{Log: log, TmpParent: parent}, argv: []string{os.Args[0]}, wantErr: "needs a Log, a positive Grace and a TmpParent"},
		{name: "no tmp parent", exec: OSExec{Log: log, Grace: time.Second}, argv: []string{os.Args[0]}, wantErr: "needs a Log, a positive Grace and a TmpParent"},
		{name: "a missing tmp parent", exec: OSExec{Log: log, Grace: time.Second, TmpParent: filepath.Join(parent, "absent")}, argv: []string{os.Args[0]}, wantErr: "make the temp root of u#00"},
		{name: "no command", exec: OSExec{Log: log, Grace: time.Second, TmpParent: parent}, wantErr: "unit u#00 has no command"},
		{name: "a missing program", exec: OSExec{Log: log, Grace: time.Second, TmpParent: parent}, argv: []string{filepath.Join(t.TempDir(), "absent")}, wantErr: "run: start u#00"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s := helperSpec(nil)
			s.Argv = tt.argv

			// Act
			_, err := tt.exec.Start(s, &bytes.Buffer{})

			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
				t.Fatalf("err = %v, want it to mention %q", err, tt.wantErr)
			}
		})
	}
}

// tmpdirOf reads the TMPDIR the "tmpdir" helper reported.
func tmpdirOf(t *testing.T, out string) string {
	t.Helper()
	for _, line := range strings.Split(out, "\n") {
		if dir, ok := strings.CutPrefix(line, "TMPDIR="); ok {
			return dir
		}
	}
	t.Fatalf("the helper reported no TMPDIR: %q", out)
	return ""
}

func TestOSExecHandsEachUnitAFreshTempRootUnderTheParent(t *testing.T) {
	// Arrange
	log, _ := newExecLog()
	parent := t.TempDir()
	out := &bytes.Buffer{}

	// Act
	p, err := OSExec{Log: log, Grace: time.Second, TmpParent: parent}.Start(helperSpec([]string{helperEnv + "=tmpdir"}), out)
	if err != nil {
		t.Fatal(err)
	}
	if _, err := waitBounded(t, p); err != nil {
		t.Fatal(err)
	}

	// Assert
	if got := filepath.Dir(tmpdirOf(t, out.String())); got != parent {
		t.Fatalf("the unit's TMPDIR is under %s, want %s", got, parent)
	}
}

func TestOSExecRemovesTheTempRootAndWhatTheUnitLeftInIt(t *testing.T) {
	// Arrange
	log, _ := newExecLog()
	parent := t.TempDir()
	p, err := OSExec{Log: log, Grace: time.Second, TmpParent: parent}.Start(helperSpec([]string{helperEnv + "=tmpdir"}), &bytes.Buffer{})
	if err != nil {
		t.Fatal(err)
	}

	// Act
	exit, err := waitBounded(t, p)

	// Assert
	entries, readErr := os.ReadDir(parent)
	if err != nil || exit != 0 || readErr != nil || len(entries) != 0 {
		t.Fatalf("Wait = %d, %v; parent holds %v (%v), want it empty", exit, err, entries, readErr)
	}
}

func TestOSExecFailsAUnitWhoseTempRootCannotBeRemoved(t *testing.T) {
	// Arrange
	log, _ := newExecLog()
	parent := t.TempDir()
	t.Cleanup(func() {
		// Give the permission back so the test's own temp dir can go.
		roots, _ := filepath.Glob(filepath.Join(parent, "tu-*", "locked"))
		for _, r := range roots {
			_ = os.Chmod(r, 0o700)
		}
	})
	p, err := OSExec{Log: log, Grace: time.Second, TmpParent: parent}.Start(helperSpec([]string{helperEnv + "=unremovable"}), &bytes.Buffer{})
	if err != nil {
		t.Fatal(err)
	}

	// Act
	exit, err := waitBounded(t, p)

	// Assert
	if err == nil || exit != -1 || !strings.Contains(err.Error(), "remove the temp root") {
		t.Fatalf("Wait = %d, %v; want -1 and a temp-root removal error", exit, err)
	}
}

func TestOSExecLetsAUnitNameItsOwnTMPDIR(t *testing.T) {
	// Arrange
	log, _ := newExecLog()
	own := t.TempDir()
	out := &bytes.Buffer{}

	// Act
	p, err := OSExec{Log: log, Grace: time.Second, TmpParent: t.TempDir()}.Start(helperSpec([]string{helperEnv + "=tmpdir", "TMPDIR=" + own}), out)
	if err != nil {
		t.Fatal(err)
	}
	if _, err := waitBounded(t, p); err != nil {
		t.Fatal(err)
	}

	// Assert
	if got := tmpdirOf(t, out.String()); got != own {
		t.Fatalf("TMPDIR = %s, want the unit's own %s", got, own)
	}
}

func TestOSExecLeavesNoTempRootForAUnitThatCouldNotStart(t *testing.T) {
	// Arrange
	log, _ := newExecLog()
	parent := t.TempDir()
	s := helperSpec(nil)
	s.Argv = []string{filepath.Join(t.TempDir(), "absent")}

	// Act
	_, err := OSExec{Log: log, Grace: time.Second, TmpParent: parent}.Start(s, &bytes.Buffer{})

	// Assert
	entries, readErr := os.ReadDir(parent)
	if err == nil || readErr != nil || len(entries) != 0 {
		t.Fatalf("Start err = %v; parent holds %v (%v), want it empty", err, entries, readErr)
	}
}
