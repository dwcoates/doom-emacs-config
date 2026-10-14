// Package life ties every process a test run starts to the run's own lifetime,
// through the kernel, so nothing the run started outlives it however it ends.
//
// THREE PROCESSES. `testrun run` is the SENTINEL: it stays in its invoker's
// process group (the group bin/test-all.sh, safe-test-run and a merge gate
// signal), starts the RUNNER in a session of its own, relays the runner's
// output and answers its exit status. The runner leads the run's session and
// starts every unit in it; it starts the WARDEN, in a session of its own too,
// before anything else.
//
// TWO LIFELINES, each a pipe whose write end only one process holds, so the
// kernel closes it the instant that process dies, SIGKILL included:
//
//   - the sentinel holds the runner's lifeline. A merge gate that cuts its run
//     SIGKILLs the sentinel's group, which never reached the units (each runs
//     in a group of its own); the runner reads EOF, takes it as the run's
//     cancellation, reaps its whole session except itself and the warden,
//     releases its disk and exits non-zero.
//   - the runner holds the warden's lifeline. A runner killed outright never
//     releases anything; the warden reads EOF without the runner's "done",
//     reaps the runner's session (and, as the second net, whatever runs out of
//     the run's directories), then releases the run's disk.
//
// Membership is the run's SESSION id, which the kernel keeps for every
// process the run starts unless one leaves the session itself (see
// reap.Members.Dirs for the one that does). Every reap is reap.Reap.
package life

import (
	"errors"
	"os"
	"os/signal"
	"sync"
	"syscall"

	"agentrepl/testrun/internal/run"
)

// EnvRole names the role a testrun process was started in: RoleRunner for
// the sentinel's runner. The sentinel itself is started with none.
const EnvRole = "AGENT_REPL_TESTRUN_ROLE"

// RoleRunner is the runner's EnvRole.
const RoleRunner = "runner"

// lifelineFD is the descriptor a runner and a warden read their lifeline on:
// the first of exec.Cmd.ExtraFiles.
const lifelineFD = 3

// Logger is the run's log: testrun's run.Log.
type Logger interface {
	Infof(format string, args ...any)
	Errorf(format string, args ...any)
}

// FallbackWriter writes to its primary until a write there fails with EPIPE
// (the sentinel relaying it is gone), then appends to Path, the run's log
// file, for good.
//
// A CUT RUN'S LAST WORDS ARE ITS CLEANUP. A runner or warden whose sentinel
// died still logs its reap and its release, and with the relay gone those
// lines would go nowhere; bin/test-all.sh names its log file to every process
// of the run (ramdisk.EnvRunLog), so they land where the run's other lines
// did. With no Path, a write after the break answers the EPIPE.
type FallbackWriter struct {
	Primary *os.File
	Path    string

	mu     sync.Mutex
	broken bool
	file   *os.File
}

// Write implements io.Writer.
func (w *FallbackWriter) Write(p []byte) (int, error) {
	w.mu.Lock()
	defer w.mu.Unlock()
	if !w.broken {
		n, err := w.Primary.Write(p)
		if !errors.Is(err, syscall.EPIPE) {
			return n, err
		}
		w.broken = true
	}
	if w.Path == "" {
		return 0, syscall.EPIPE
	}
	if w.file == nil {
		f, err := os.OpenFile(w.Path, os.O_WRONLY|os.O_APPEND|os.O_CREATE, 0o644)
		if err != nil {
			return 0, err
		}
		w.file = f
	}
	return w.file.Write(p)
}

// CutLog is the log of a runner or a warden: its sentinel's relay on stdout
// and stderr, and the run's log file at path once the relay is gone
// (FallbackWriter). It takes SIGPIPE, so a write to the broken relay answers
// EPIPE instead of ending the process before its cleanup; a handler, unlike
// SIG_IGN, is not inherited by the units.
func CutLog(path string) *run.Log {
	signal.Notify(make(chan os.Signal, 1), syscall.SIGPIPE)
	return &run.Log{Out: &FallbackWriter{Primary: os.Stdout, Path: path}, Err: &FallbackWriter{Primary: os.Stderr, Path: path}}
}
