package stack

import (
	"context"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"syscall"
	"testing"
	"time"

	"agent-repld/integration/grantedclock"
	"agent-repld/integration/harness"
)

// A STAND-IN REPORTS WHAT A TEST WAITS FOR; THE TEST NEVER POLLS FOR IT (the
// agent-repl-tests skill, "Synchronization: events, never time"). A stand-in
// process that must tell its test something -- a child's pid -- writes it to
// a FIFO the test reads: the read blocks until the write, so it is an event,
// and the stand-in's exit ends it at once instead of a bound.

// standInReportBound is the fallback bound on a stand-in's report: one fork
// and one write through /bin/sh, milliseconds healthy.
const standInReportBound = 5 * time.Second

// reportFIFO makes the FIFO a stand-in reports on, in dir.
func reportFIFO(t testing.TB, dir, name string) string {
	t.Helper()
	path := filepath.Join(dir, name)
	if err := syscall.Mkfifo(path, 0o600); err != nil {
		t.Fatalf("make the report fifo %s: %v", path, err)
	}
	return path
}

// readReportedPID reads the pid a stand-in writes to FIFO (reportFIFO). It
// fails at once when EXITED closes first: a stand-in that died never reports.
func readReportedPID(t testing.TB, fifo string, exited <-chan struct{}) int {
	t.Helper()
	type report struct {
		body []byte
		err  error
	}
	read := make(chan report, 1)
	go func() {
		body, err := os.ReadFile(fifo)
		read <- report{body, err}
	}()
	// A read left blocked on a FIFO nobody will write is released by opening
	// its write end, so no goroutine outlives the test.
	defer func() {
		if w, err := os.OpenFile(fifo, os.O_WRONLY|syscall.O_NONBLOCK, 0); err == nil {
			_ = w.Close()
		}
	}()
	bound, cancel := grantedclock.WithBound(context.Background(), standInReportBound)
	defer cancel()
	select {
	case r := <-read:
		if r.err != nil {
			t.Fatalf("read the stand-in's report at %s: %v", fifo, r.err)
		}
		pid, err := strconv.Atoi(strings.TrimSpace(string(r.body)))
		if err != nil || pid <= 0 {
			t.Fatalf("the stand-in reported %q at %s, want a pid", r.body, fifo)
		}
		return pid
	case <-exited:
		t.Fatalf("the stand-in exited before it reported at %s", fifo)
	case <-bound.Done():
		t.Fatalf("the stand-in did not report at %s: %v", fifo, grantedclock.Explain(bound))
	}
	return 0
}

func TestReadReportedPID(t *testing.T) {
	cases := []struct {
		name string
		// script is the stand-in's /bin/sh body; REPORT names its fifo.
		script      func(report string) string
		wantPID     bool
		wantFailure string
	}{
		{name: "a stand-in that reports its pid", wantPID: true, script: func(report string) string {
			return "echo $$ > " + report + "; read _ < /dev/stdin"
		}},
		{name: "a stand-in that exits first fails at once", wantFailure: "exited before it reported", script: func(string) string {
			return "exit 3"
		}},
		{name: "a stand-in that reports no pid fails", wantFailure: "want a pid", script: func(report string) string {
			return "echo nonsense > " + report + "; read _ < /dev/stdin"
		}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			report := reportFIFO(t, t.TempDir(), "report")
			cmd := exec.Command("/bin/sh", "-c", tc.script(report))
			stdin, err := cmd.StdinPipe()
			if err != nil {
				t.Fatalf("stdin: %v", err)
			}
			if err := cmd.Start(); err != nil {
				t.Fatalf("start: %v", err)
			}
			exited := make(chan struct{})
			go func() { _ = cmd.Wait(); close(exited) }()
			t.Cleanup(func() { _ = stdin.Close(); _ = cmd.Process.Kill(); <-exited })
			rec := harness.NewRecordingTB(t, t.Name())

			// Act
			var pid int
			harness.Within(func() { pid = readReportedPID(rec, report, exited) })

			// Assert
			if tc.wantPID && (pid != cmd.Process.Pid || rec.Failed()) {
				t.Fatalf("readReportedPID = %d (failures %q), want the stand-in's pid %d", pid, rec.Failures(), cmd.Process.Pid)
			}
			if tc.wantFailure != "" && !strings.Contains(rec.Failures(), tc.wantFailure) {
				t.Fatalf("failures = %q, want one naming %q", rec.Failures(), tc.wantFailure)
			}
		})
	}
}
