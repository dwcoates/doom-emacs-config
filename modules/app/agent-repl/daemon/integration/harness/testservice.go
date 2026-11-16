package harness

import (
	"context"
	"encoding/json"
	"fmt"
	"net"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"sync"
	"syscall"
	"testing"
	"time"

	"agentrepl/logging/childgroup"

	"agent-repld/integration/grantedclock"
	"agent-repld/internal/testsclient"
)

// EVERY DAEMON'S MERGE GATES RUN ON A REAL TEST SERVICE OF ITS TEST, never
// the owner's (~/.cache/agent-repl/sock/tests.sock): a test daemon pointed at
// the owner's service would run its scripted gates there.
//
// The service is keyed to the STATE ROOT, as the lock directory is
// (LockDirFor): sharing a state root is how this suite spells "the same
// daemon, restarted", and a restarted daemon's resumed merge watches the run
// its predecessor's gate started on the same service.
//
// THE HARNESS BINDS THE SOCKET AND HANDS IT OVER (--listen-fd), so a daemon's
// first connect queues in the socket's backlog until the service accepts:
// there is no readiness to wait for.

// testServiceStopBound bounds the service's exit on SIGTERM: its orderly
// drain of in-flight requests (testservice's shutdownGrace, 5s) and a margin.
const testServiceStopBound = 2 * 5 * time.Second

// testServices is the test service of each state root this process started
// one for.
var testServices sync.Map

// TestsSocketFor is the socket of the test service of the daemons over
// STATEDIR: a sibling of the state root, as the lock directory is.
func TestsSocketFor(stateDir string) string {
	return filepath.Join(filepath.Dir(stateDir), "tests", "tests.sock")
}

// ensureTestService answers the socket of the test service of the daemons
// over STATEDIR, starting it the first time a daemon over STATEDIR starts.
// The service ends with the test that started it, and every run still going
// on it is killed with its group.
func ensureTestService(t testing.TB, stateDir, lockDir string) string {
	t.Helper()
	socket := TestsSocketFor(stateDir)
	if _, started := testServices.Load(stateDir); started {
		return socket
	}
	dir := filepath.Dir(socket)
	runs := filepath.Join(dir, "runs")
	if err := os.MkdirAll(runs, 0o755); err != nil {
		t.Fatalf("harness: mkdir the test service's runs: %v", err)
	}
	ln, err := net.Listen("unix", socket)
	if err != nil {
		t.Fatalf("harness: bind the test service's socket %s: %v", socket, err)
	}
	listener := ln.(*net.UnixListener)
	file, err := listener.File()
	if err != nil {
		t.Fatalf("harness: the test service's listener as a file: %v", err)
	}
	// The child holds the socket from here; the harness's copy closes
	// without unlinking the path the child serves on.
	listener.SetUnlinkOnClose(false)
	_ = listener.Close()
	logPath := filepath.Join(dir, "tests.log")
	cmd := exec.Command(TestServiceBinary(t), "--socket", socket, "--runs", runs, "--log", logPath, "--listen-fd", "3")
	cmd.ExtraFiles = []*os.File{file}
	// Its build report goes to the daemons' lock directory, never the
	// owner's ~/.cache/agent-repl/run.
	cmd.Env = append(cleanGitEnv(os.Environ()), "AGENT_REPL_LOCK_DIR="+lockDir, testsclient.EnvSocket+"="+socket)
	cmd.Stdout, cmd.Stderr = os.Stderr, os.Stderr
	if err := cmd.Start(); err != nil {
		t.Fatalf("harness: start the test service: %v", err)
	}
	_ = file.Close()
	exited := make(chan error, 1)
	go func() { exited <- cmd.Wait() }()
	testServices.Store(stateDir, true)
	t.Cleanup(func() {
		testServices.Delete(stateDir)
		_ = cmd.Process.Signal(syscall.SIGTERM)
		// THE BOUND IS ON THE GRANTED CLOCK: the service is this binary's
		// child, so a host too busy to schedule it never reads as a hang.
		wait, cancel := grantedclock.WithBound(context.Background(), testServiceStopBound)
		defer cancel()
		select {
		case <-exited:
		case <-wait.Done():
			_ = cmd.Process.Kill()
			<-exited
			t.Errorf("harness: the test service did not exit within %s granted of SIGTERM (its drain is 5s); killed: %v", testServiceStopBound, grantedclock.Explain(wait))
		}
		for _, err := range killRunsLeftGoing(runs, childgroup.KillGroup) {
			t.Errorf("harness: %v", err)
		}
	})
	return socket
}

// killRunsLeftGoing kills the process group of every run the test service
// at RUNS left going, answering every run it could not read or kill: the
// service outlives its runs' watchers by design, so its test ends them.
func killRunsLeftGoing(runs string, kill func(pgid int) error) []error {
	entries, err := os.ReadDir(runs)
	if err != nil {
		return []error{fmt.Errorf("read the test service's runs: %w", err)}
	}
	var errs []error
	for _, entry := range entries {
		if _, err := os.Stat(filepath.Join(runs, entry.Name(), "end.json")); err == nil {
			continue
		}
		raw, err := os.ReadFile(filepath.Join(runs, entry.Name(), "run.json"))
		if err != nil {
			errs = append(errs, fmt.Errorf("read the test service's run %s: %w", entry.Name(), err))
			continue
		}
		var run struct {
			PGID int `json:"pgid"`
		}
		if err := json.Unmarshal(raw, &run); err != nil || run.PGID <= 0 {
			errs = append(errs, fmt.Errorf("the test service's run %s names no group (%v): %s", entry.Name(), err, strconv.Quote(string(raw))))
			continue
		}
		if err := kill(run.PGID); err != nil {
			errs = append(errs, fmt.Errorf("kill the test service's run %s (group %d): %w", entry.Name(), run.PGID, err))
		}
	}
	return errs
}
