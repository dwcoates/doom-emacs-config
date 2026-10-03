// Command shim-lock HOLDS one kernel file lock on behalf of a parent that
// cannot take one itself.
//
// # Why this binary exists
//
// The shim's session and workspace claims are `flock(2)` locks, because the
// daemon PROBES them with Go's `syscall.Flock(LOCK_EX|LOCK_NB)` and only a real
// kernel lock on the same path answers that probe. Node has no flock: the shim
// used to take one through `open(2)`'s `O_EXLOCK`, which is macOS/BSD only, so
// on Linux the shim refused to start ANY session and every Linux deployment was
// dead in the water.
//
// A tiny Go child closes that gap with ONE code path on both platforms. The
// shim spawns `shim-lock <lock-path>`, keeps the child's stdin pipe, and reads
// one line of stdout; the child holds the lock for as long as it lives.
//
// # The two properties the shim's lock module documents, preserved exactly
//
//   - KERNEL-ENFORCED. The lock is a real `flock`, taken on the same path the
//     daemon probes, so exclusion is arbitration rather than bookkeeping.
//   - RELEASED ON DEATH, HOWEVER DEATH COMES. The parent holds the write end of
//     this process's stdin. When the shim exits — cleanly, on a crash, on
//     SIGKILL — the kernel closes that pipe, this process reads EOF and exits,
//     and the kernel drops the lock with its last file descriptor. There is no
//     stale lock to reap and no PID-reuse hazard, exactly as before.
//
// # The protocol, which the shim's locks.ts is the only speaker of
//
//	argv:    exactly one argument, the absolute lock file path
//	stdout:  the single line "locked" once the lock is HELD, and nothing else
//	stdin:   read and discarded; EOF means release
//	stderr:  structured JSON diagnostic records (internal/logging)
//	exit 0:  stdin reached EOF and the lock was released
//	exit 2:  the arguments were not one lock path
//	exit 3:  the lock is HELD BY ANOTHER PROCESS (EWOULDBLOCK)
//	exit 1:  anything else failed
//
// Exit 3 is spelled distinctly because the shim must tell "another shim owns
// this conversation" (a typed `conversation_owned` refusal) apart from "the
// claim could not be attempted" (a hard failure). Collapsing them would make an
// unwritable lock directory look like a live duplicate.
package main

import (
	"errors"
	"fmt"
	"io"
	"os"
	"os/signal"
	"path/filepath"
	"strings"
	"syscall"

	"agentrepl/shim-lock/internal/logging"
)

// The exit codes the protocol above fixes. They are the shim's only structured
// channel for WHY a claim did not happen, so nothing here may be reordered.
const (
	exitOK    = 0
	exitError = 1
	exitUsage = 2
	exitHeld  = 3
)

// ReadyLine is the one line stdout ever carries, written only once the lock is
// actually held. The shim treats its arrival as the claim and nothing else.
const ReadyLine = "locked"

func main() {
	os.Exit(run(os.Args[1:], os.Stdin, os.Stdout, os.Stderr))
}

// run is main with its process boundaries injected, so the suite drives every
// branch without reading the real argv or the real standard streams.
func run(args []string, stdin io.Reader, stdout, stderr io.Writer) int {
	log := logging.New(stderr)

	if len(args) != 1 || strings.TrimSpace(args[0]) == "" {
		log.Error("shim-lock.usage",
			"shim-lock takes exactly one argument, the lock file path",
			logging.Context{"args": args})
		return withSinkStatus(log, exitUsage)
	}
	lockPath := args[0]
	ctx := logging.Context{"lock_path": lockPath}

	// The lock DIRECTORY is created here rather than assumed. A holder that
	// refused for want of a directory would be indistinguishable, to a reader,
	// from one that refused because a live shim owns the workspace.
	if err := os.MkdirAll(filepath.Dir(lockPath), 0o755); err != nil {
		log.Error("shim-lock.acquire", "the lock directory could not be created: "+err.Error(), ctx)
		return withSinkStatus(log, exitError)
	}

	file, err := os.OpenFile(lockPath, os.O_RDWR|os.O_CREATE, 0o600)
	if err != nil {
		log.Error("shim-lock.acquire", "the lock file could not be opened: "+err.Error(), ctx)
		return withSinkStatus(log, exitError)
	}
	// Held for the whole of this process's life; the close is what drops the
	// flock on the deliberate-release path, and the kernel does it on every
	// other path.
	defer file.Close()

	// LOCK_NB so an already-held lock answers immediately instead of parking
	// this process behind whichever shim owns the claim — the shim turns a
	// refusal into a typed StartSession answer and stays serving.
	if err := syscall.Flock(int(file.Fd()), syscall.LOCK_EX|syscall.LOCK_NB); err != nil {
		if errors.Is(err, syscall.EWOULDBLOCK) || errors.Is(err, syscall.EAGAIN) {
			log.Error("shim-lock.acquire", "the lock is already held by another process", ctx)
			return withSinkStatus(log, exitHeld)
		}
		log.Error("shim-lock.acquire", "flock failed: "+err.Error(), ctx)
		return withSinkStatus(log, exitError)
	}

	// THE READY LINE COMES AFTER THE FLOCK, NEVER BEFORE IT. The shim treats
	// this line as proof the claim is made; announcing intent would let it
	// start a session over a lock it does not hold.
	// THE LOCK OUTLIVES A SIGNAL TO ITS PROCESS GROUP. This process is in the
	// shim's group (the shim leads one of its own), so a stop the daemon sends
	// the group -- SIGTERM, which starts the shim's GRACEFUL stand-down -- used
	// to end this holder at once: the lock was released while the shim went on
	// serving its session for seconds, and a daemon booting in that window read
	// "lock free, socket live" and adopted a running session as an inert shim
	// (live bounce 2026-10-03 14:11, shim pid 26015). The lock is the shim's,
	// so only the shim's end ends it: stdin's EOF (its exit or its deliberate
	// release). SIGKILL, which cannot be ignored, still releases it with the
	// process, as the kernel lock always did.
	signal.Ignore(syscall.SIGTERM, syscall.SIGINT, syscall.SIGHUP)

	if _, err := fmt.Fprintln(stdout, ReadyLine); err != nil {
		log.Error("shim-lock.acquire", "the ready line could not be written: "+err.Error(), ctx)
		return withSinkStatus(log, exitError)
	}
	log.Info("shim-lock.hold", "holding the lock until stdin closes", ctx)

	// The hold. Nothing is ever sent on stdin; the read exists solely so that
	// the parent's death — which closes the pipe — ends this process and
	// therefore the lock.
	if _, err := io.Copy(io.Discard, stdin); err != nil {
		log.Error("shim-lock.hold", "reading stdin failed: "+err.Error(), ctx)
		return withSinkStatus(log, exitError)
	}

	log.Info("shim-lock.release", "stdin reached EOF; releasing the lock", ctx)
	return withSinkStatus(log, exitOK)
}

// withSinkStatus answers code, or exitError when a diagnostic never reached
// stderr. A holder whose records went nowhere has no other way to say so.
func withSinkStatus(log *logging.Logger, code int) int {
	if code == exitOK && log.SinkFailed() {
		return exitError
	}
	return code
}
