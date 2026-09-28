// run.go is the leaf's ONE spawn point. Every git the daemon ever runs goes
// through invoke below, so the environment hygiene and the evidence-carrying
// failure shape are implemented exactly once and cannot drift per method.
//
// WHY THE HYGIENE IS NOT DEFENSIVE PROGRAMMING. Git exports its
// repository-selecting variables into every child a hook runs, so a daemon
// spawned from inside a hook (or from an Emacs whose own environment carries
// them) inherits a GIT_DIR that names somebody else's repository. `-C dir`
// does not override it: git honors GIT_DIR first and reports work-tree errors
// against a repository the caller never named. This repository has already
// been bitten by that once. Stripping them here makes `-C dir` the ONLY
// repository selector, structurally.
package gitclient

import (
	"context"
	"errors"
	"os"
	"os/exec"
	"strings"
	"syscall"

	"claude-repld/internal/dlog"
)

// terminatingSignal reports the signal that killed the process, when one did.
// A process that exited on its own is not signalled and answers false, so the
// ordinary nonzero exit keeps its status untouched.
func terminatingSignal(state *os.ProcessState) (syscall.Signal, bool) {
	status, ok := state.Sys().(syscall.WaitStatus)
	if !ok || !status.Signaled() {
		return 0, false
	}
	return status.Signal(), true
}

// strippedVars are the environment bindings that select a repository, an
// index, or an object store independently of `-C dir`. Every one of them is
// removed from the inherited environment before git is spawned. Everything
// else the daemon inherited (PATH, HOME, the git identity, the config-file
// overrides) survives untouched.
var strippedVars = []string{
	"GIT_DIR",
	"GIT_WORK_TREE",
	"GIT_INDEX_FILE",
	"GIT_COMMON_DIR",
	"GIT_PREFIX",
	"GIT_OBJECT_DIRECTORY",
	"GIT_ALTERNATE_OBJECT_DIRECTORIES",
	"GIT_NAMESPACE",
	"GIT_CEILING_DIRECTORIES",
}

// MergeQueueMarker is the binding the merge queue's fast-forward carries into
// git, and the ONLY way the repository's reference-transaction hook
// (.githooks/reference-transaction) tells a move of master the queue made from
// one somebody made by hand. OwnerOverride is the owner's escape hatch through
// the same hook. The hook spells both names; TestTheHookSpellsTheMarkers holds
// the two spellings together.
const (
	MergeQueueMarker = "AGENT_REPL_MERGE_QUEUE"
	OwnerOverride    = "AGENT_REPL_OWNER_OVERRIDE"
)

// hookMarkerVars are stripped from the INHERITED environment like the
// repository selectors: a daemon started from a shell that exported either one
// would otherwise vouch for every git it runs. The marker reaches git only
// where a method sets it (FastForward), and the owner override never does,
// because the daemon never acts as the owner.
var hookMarkerVars = []string{MergeQueueMarker, OwnerOverride}

// pinnedVars are the bindings the daemon SETS rather than inherits.
// GIT_TERMINAL_PROMPT=0 makes a git that wants a credential fail instead of
// blocking a daemon that has no terminal; LC_ALL=C pins the message and status
// locale so parsing never depends on the invoking user's language. Both are
// stripped from the inherited environment first, so the pinned value is the
// only one present rather than merely the last one.
var pinnedVars = []string{
	"GIT_TERMINAL_PROMPT=0",
	"LC_ALL=C",
}

// scrubEnv returns env with every repository-selecting binding and every hook
// marker removed, and the pinned bindings appended. It is the whole
// environment contract; a method's own bindings (withEnv) are appended after
// it.
func scrubEnv(env []string) []string {
	kept := make([]string, 0, len(env)+len(pinnedVars))
	for _, entry := range env {
		if isScrubbed(entry) {
			continue
		}
		kept = append(kept, entry)
	}
	return append(kept, pinnedVars...)
}

// isScrubbed reports whether one `NAME=value` binding must not survive into
// git's environment.
func isScrubbed(entry string) bool {
	name, _, ok := strings.Cut(entry, "=")
	if !ok {
		return false
	}
	for _, stripped := range strippedVars {
		if name == stripped {
			return true
		}
	}
	for _, marker := range hookMarkerVars {
		if name == marker {
			return true
		}
	}
	for _, pinned := range pinnedVars {
		if pinnedName, _, _ := strings.Cut(pinned, "="); name == pinnedName {
			return true
		}
	}
	return false
}

// invocation is one completed git run. It is the raw material both run and the
// exit-code-inspecting callers (a conflicted merge, a dirty tree) work from:
// a nonzero exit is DATA here and only becomes an error where the method
// decides it is one.
type invocation struct {
	args     []string
	dir      string
	exitCode int
	stdout   string
	stderr   string
	// signal names the signal that killed git, when a signal did. It is set
	// ONLY for a death nobody in this process asked for: a cancellation is
	// classified before this and carries no exit status at all.
	signal string
	// pid is the git child's process id, zero when git never started. A
	// signalled git names no cause of its own, so the pid is what ties the
	// record to whoever sent the signal: a killer can name the pids it
	// signalled, never the daemon operation it interrupted.
	pid int
}

// fail shapes the invocation as the leaf's evidence-carrying error.
func (in invocation) fail() *Error {
	return &Error{
		Args:     in.args,
		Dir:      in.dir,
		ExitCode: in.exitCode,
		Stdout:   in.stdout,
		Stderr:   in.stderr,
		Signal:   in.signal,
	}
}

// cancelled shapes the invocation as the leaf's cancellation, which is a
// different fact from a failure and carries no exit status.
func (in invocation) cancelled(cause error) *Cancelled {
	return &Cancelled{Args: in.args, Dir: in.dir, Cause: cause}
}

// logContext is the structured context every record about this invocation
// carries.
func (in invocation) logContext() dlog.Context {
	fields := dlog.Context{
		"dir":       in.dir,
		"args":      in.args,
		"exit_code": in.exitCode,
		"stdout":    in.stdout,
		"stderr":    in.stderr,
	}
	// THE SIGNAL IS THE EVIDENCE when there is one, and it is the ONLY
	// evidence: a killed git writes no stderr, so a record without this field
	// says "exit -1" and nothing a person could act on.
	if in.signal != "" {
		fields["signal"] = in.signal
	}
	if in.pid != 0 {
		fields["pid"] = in.pid
	}
	return fields
}

// invoke runs `git -C dir args...` with the scrubbed environment and reports
// what happened. The returned error is non-nil only when git could not be run
// at all — a git that ran and exited nonzero is reported through exitCode, and
// it is the caller that decides whether that is a failure or an answer.
//
// extra are the method's own bindings, appended after the scrub so a marker
// the method sets is the only one present.
func (c *client) invoke(ctx context.Context, dir string, extra []string, args ...string) (invocation, error) {
	full := append([]string{"-C", dir}, args...)
	cmd := exec.CommandContext(ctx, "git", full...)
	cmd.Env = append(scrubEnv(os.Environ()), extra...)

	var stdout, stderr strings.Builder
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr

	in := invocation{args: args, dir: dir}
	err := cmd.Start()
	if err == nil {
		in.pid = cmd.Process.Pid
		err = cmd.Wait()
	}
	in.stdout = stdout.String()
	in.stderr = stderr.String()

	switch {
	case err == nil:
		return in, nil
	case ctx.Err() != nil:
		// WE KILLED IT. exec.CommandContext signals the process when the
		// context ends, and the wait then reports a signalled death whose
		// ExitCode() is -1. Reporting that as "git exited nonzero, exit -1"
		// blamed git for a decision the daemon made, which at shutdown put a
		// false failure in the log for every git still in flight. The
		// cancellation is the fact; there is no exit status to carry.
		in.exitCode = -1
		return in, in.cancelled(ctx.Err())
	case errors.As(err, new(*exec.ExitError)):
		if sig, killed := terminatingSignal(cmd.ProcessState); killed {
			// SOMEBODY ELSE KILLED IT. The context is fine, so this is not our
			// cancellation, and there is no exit status: ExitCode() answers -1
			// for a signalled death and git wrote no stderr on its way out. A
			// caller that reads that as "git exited nonzero, exit -1" is told
			// nothing it can act on and may even read the -1 as an ANSWER (a
			// conflict, a dirty tree). The signal is the fact, it is carried as
			// the evidence, and the invocation is a failure rather than a
			// judgeable exit code.
			in.signal = sig.String()
			in.exitCode = -1
			return in, in.fail()
		}
		in.exitCode = cmd.ProcessState.ExitCode()
		return in, nil
	default:
		// git never started: no exit status exists, so -1 marks the absence
		// rather than pretending to a status, and the reason git could not be
		// spawned becomes the evidence in place of git's own stderr.
		in.exitCode = -1
		if in.stderr == "" {
			in.stderr = err.Error()
		}
		return in, in.fail()
	}
}

// run is the ordinary path: it invokes git, treats any nonzero exit as a
// failure, logs the outcome once, and returns trimmed stdout.
func (c *client) run(ctx context.Context, operation, dir string, args ...string) (string, error) {
	return c.runWithEnv(ctx, operation, dir, nil, args...)
}

// runWithEnv is run with the method's own bindings added to git's environment.
func (c *client) runWithEnv(ctx context.Context, operation, dir string, extra []string, args ...string) (string, error) {
	in, err := c.runRawWithEnv(ctx, operation, dir, extra, args...)
	if err != nil {
		return "", err
	}
	if in.exitCode != 0 {
		failure := in.fail()
		c.log.Global().Error(operation, "git exited nonzero", in.logContext())
		return "", failure
	}
	return strings.TrimRight(in.stdout, "\n"), nil
}

// runRaw invokes git and logs the ordinary path, leaving a nonzero exit for
// the caller to judge. It is what the methods whose ANSWER is an exit code
// (MergeNoFF's conflict, IsClean's dirty tree) use.
func (c *client) runRaw(ctx context.Context, operation, dir string, args ...string) (invocation, error) {
	return c.runRawWithEnv(ctx, operation, dir, nil, args...)
}

// runRawWithEnv is runRaw with the method's own bindings added to git's
// environment.
func (c *client) runRawWithEnv(ctx context.Context, operation, dir string, extra []string, args ...string) (invocation, error) {
	in, err := c.invoke(ctx, dir, extra, args...)
	if err != nil {
		var cancelled *Cancelled
		if errors.As(err, &cancelled) {
			// At most INFO: a cancelled git is the daemon exiting or an
			// operation being called off, not a fault. The error is still
			// returned unchanged, so nothing is swallowed.
			fields := dlog.Context{
				"dir":        in.dir,
				"args":       in.args,
				"subcommand": cancelled.Subcommand(),
				"stdout":     in.stdout,
				"stderr":     in.stderr,
				"cause":      cancelled.Cause.Error(),
			}
			// A context that ended before the spawn leaves no process to name.
			if in.pid != 0 {
				fields["pid"] = in.pid
			}
			c.log.Global().Info(operation, "git was cancelled before it finished", fields)
			return in, err
		}
		if in.signal != "" {
			c.log.Global().Error(operation, "git was killed by a signal", in.logContext())
			return in, err
		}
		c.log.Global().Error(operation, "git could not be run", in.logContext())
		return in, err
	}
	c.log.Global().Debug(operation, "git ran", in.logContext())
	return in, nil
}
