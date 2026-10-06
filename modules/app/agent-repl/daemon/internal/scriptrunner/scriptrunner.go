// Package scriptrunner runs one shell-invoked script and reports its combined
// output and exit code.
//
// It is the ONE process-spawning script runner in the daemon: the rollout
// controller's deploy chain and the merge orchestrator's test gate both
// consume it through their own copy of the same narrow interface (see
// internal/rollout's Deployer and internal/merge/api.go's ScriptRunner). A
// script that RAN AND FAILED is a non-zero exit code and a NIL error; an
// error is returned only when the run could not be CLASSIFIED at all — an
// empty argv, a script that could not be spawned, or a context that ended
// first. A caller that wants "did it succeed" reads the exit code, not the
// error.
package scriptrunner

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"strings"
	"syscall"

	"claude-repld/internal/dlog"
)

// gitEnvKeys are the repository-selecting variables that must never be
// inherited by a spawned script: a hook-leaked GIT_DIR is a real, previously
// observed source of bogus work-tree errors, and a script running as part of
// the deploy chain or the merge test gate must resolve its own repository
// rather than one a caller's environment happened to leak in. This mirrors
// integration/harness/daemon.go's gitEnvKeys, which scrubs a fake git child's
// environment for the identical reason.
var gitEnvKeys = []string{
	"GIT_DIR", "GIT_WORK_TREE", "GIT_INDEX_FILE", "GIT_COMMON_DIR",
	"GIT_PREFIX", "GIT_OBJECT_DIRECTORY", "GIT_ALTERNATE_OBJECT_DIRECTORIES",
}

// cleanEnv returns env with every repository-selecting git variable dropped.
func cleanEnv(env []string) []string {
	out := make([]string, 0, len(env))
	for _, kv := range env {
		key, _, _ := strings.Cut(kv, "=")
		drop := false
		for _, bad := range gitEnvKeys {
			if key == bad {
				drop = true
				break
			}
		}
		if !drop {
			out = append(out, kv)
		}
	}
	return out
}

// killGroup SIGKILLs the process group the script leads. A group already gone
// is the state the kill was asked to reach.
func killGroup(pid int) error {
	if err := syscall.Kill(-pid, syscall.SIGKILL); err != nil {
		if errors.Is(err, syscall.ESRCH) {
			return os.ErrProcessDone
		}
		return err
	}
	return nil
}

// Runner runs a script in a directory. It is stateless beyond its logger, so
// one instance serves every caller in the daemon.
type Runner struct {
	log dlog.Logger
}

// New builds the runner. log is required; every run is recorded through it,
// because a script the daemon spawns on the deploy chain or the merge test
// gate is exactly the kind of external, hard-to-reproduce step whose outcome
// must survive in the durable log.
func New(log dlog.Logger) (*Runner, error) {
	if log == nil {
		return nil, fmt.Errorf("scriptrunner: a logger is required")
	}
	return &Runner{log: log}, nil
}

// Run executes argv in dir and returns the combined stdout and stderr with
// the process's exit code. It is RunLines with nobody listening.
func (r *Runner) Run(ctx context.Context, dir string, argv []string) (string, int, error) {
	return r.RunLines(ctx, dir, argv, nil)
}

// RunLines executes argv in dir and returns the combined stdout and stderr with
// the process's exit code, handing each line of that output to onLine AS IT IS
// WRITTEN (without its newline), so a caller can follow a long script live. A
// last line with no newline is handed over when the script exits. onLine runs
// on one goroutine at a time; nil hands nothing over.
//
// An empty argv or an empty dir is refused before anything is spawned: every
// caller names both a script and a directory to run it in, so either being
// blank is a caller bug rather than a runtime condition to classify. A
// refusal and a failed spawn are both reported as errors alongside a failed
// run's own non-zero code, per the package doc's RAN-AND-FAILED-versus
// COULD-NOT-BE-CLASSIFIED distinction.
func (r *Runner) RunLines(ctx context.Context, dir string, argv []string, onLine func(string)) (string, int, error) {
	if len(argv) == 0 {
		err := fmt.Errorf("scriptrunner: argv is empty")
		r.log.Error("daemon.scriptrunner.run", "refused an empty argv", dlog.Context{"dir": dir})
		return "", 0, err
	}
	if strings.TrimSpace(dir) == "" {
		err := fmt.Errorf("scriptrunner: dir is empty")
		r.log.Error("daemon.scriptrunner.run", "refused an empty dir", dlog.Context{"script": argv[0]})
		return "", 0, err
	}

	cmd := exec.CommandContext(ctx, argv[0], argv[1:]...)
	cmd.Dir = dir
	// A CANCELLED RUN TAKES ITS CHILDREN WITH IT. The script runs in its own
	// process group and the context's end kills the whole group: killing the
	// script alone left a child it started holding the output pipe open, and
	// Wait waited for that child as long as it lived, so the caller's
	// cancellation was not honored. (exec's WaitDelay is NOT the bound here:
	// it also runs after an ordinary exit, and under load it cut the output of
	// scripts that had finished and answered.)
	cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}
	cmd.Cancel = func() error { return killGroup(cmd.Process.Pid) }
	cmd.Env = cleanEnv(os.Environ())
	// stdout and stderr are combined, in order, into one buffer: a caller
	// painting a test gate's output or a deploy step's log wants what a
	// terminal would have shown, not two streams it must interleave itself.
	out := &lineWriter{onLine: onLine}
	cmd.Stdout = out
	cmd.Stderr = out

	err := cmd.Run()
	out.flush()
	output := out.buf.String()

	// A SCRIPT THE CONTEXT KILLED DID NOT ANSWER. exec reports the kill as an
	// *ExitError (signal: killed, exit code -1), which read below as a script
	// that ran and failed with code -1; it is the context ending, and is
	// classified as that.
	var exitErr *exec.ExitError
	if errors.As(err, &exitErr) && exitErr.ExitCode() == -1 && ctx.Err() != nil {
		err = fmt.Errorf("%w (%v)", ctx.Err(), err)
	}
	if err == nil {
		r.log.Debug("daemon.scriptrunner.run", "script ran to completion", dlog.Context{
			"script": argv[0], "dir": dir, "exit_code": 0,
		})
		return output, 0, nil
	}
	if errors.As(err, &exitErr) {
		// The process ran and answered with a non-zero code: that is a
		// classified, non-error outcome, per the package's RAN-AND-FAILED
		// ruling.
		//
		// THE CALLER OWNS THE LEVEL, so this record is DEBUG. Whether a
		// non-zero exit is a failure depends on what was asked: `launchctl
		// print` of a service that has left its domain exits 113 as the
		// ordinary "stopped" answer, while a build step's non-zero exit is an
		// ERROR. Every caller logs its own judgement of the code (the deploy's
		// builder and services, the merge gate), so a WARN here was a second,
		// context-free verdict that was wrong whenever the exit was expected
		// (2026-09-24, a deploy's sidecar stop).
		code := exitErr.ExitCode()
		r.log.Debug("daemon.scriptrunner.run", "script exited non-zero; its caller judges the exit", dlog.Context{
			"script": argv[0], "dir": dir, "exit_code": code,
		})
		return output, code, nil
	}

	// THE CALLER CANCELLED IT. A context the caller cancelled (a daemon
	// standing down, a client that hung up) ended the run on purpose; that is
	// the caller's decision, not a failure of the script, so it is INFO here
	// and the caller sees the cancellation in the returned error. A DEADLINE
	// is different: the script ran out of the time it was given, which stays
	// the error below.
	if errors.Is(err, context.Canceled) {
		r.log.Info("daemon.scriptrunner.run", "the caller cancelled the script before it finished", dlog.Context{
			"script": argv[0], "dir": dir, "cause": err.Error(),
		})
		return output, 0, fmt.Errorf("scriptrunner: run %s: %w", argv[0], err)
	}

	// The process never produced an exit code at all: it could not be
	// spawned, or its deadline passed first. Either is a failure to CLASSIFY,
	// not an answer, so it is surfaced as an error rather than folded into a
	// fabricated exit code.
	r.log.Error("daemon.scriptrunner.run", "script could not be run", dlog.Context{
		"script": argv[0], "dir": dir, "cause": err.Error(),
	})
	return output, 0, fmt.Errorf("scriptrunner: run %s: %w", argv[0], err)
}

// lineWriter keeps everything written to it and hands each complete line to
// onLine the moment its newline arrives. exec calls Write from one goroutine
// at a time when stdout and stderr share one writer, so it needs no lock.
type lineWriter struct {
	buf     bytes.Buffer
	pending []byte
	onLine  func(string)
}

// Write keeps p and hands over every line p completes.
func (w *lineWriter) Write(p []byte) (int, error) {
	w.buf.Write(p)
	if w.onLine == nil {
		return len(p), nil
	}
	w.pending = append(w.pending, p...)
	for {
		i := bytes.IndexByte(w.pending, '\n')
		if i < 0 {
			return len(p), nil
		}
		w.onLine(string(w.pending[:i]))
		w.pending = w.pending[i+1:]
	}
}

// flush hands over a last line that ended without a newline.
func (w *lineWriter) flush() {
	if w.onLine != nil && len(w.pending) > 0 {
		w.onLine(string(w.pending))
		w.pending = nil
	}
}
