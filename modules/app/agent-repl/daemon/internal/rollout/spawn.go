package rollout

import (
	"context"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"syscall"
	"time"
)

// JoiningFlag is the successor's binding argv spelling. Go's flag package
// accepts one dash or two; this is the documented form.
const JoiningFlag = "--joining"

// JoiningAddrFile is the file the SUCCESSOR writes to report its own address
// back to the incumbent that spawned it.
//
// THIS IS NOT A DAEMON-TO-DAEMON CHANNEL. It is a one-shot, write-once report
// in the shared state root, in the same family as the intent manifest: the
// successor writes it the moment its listener is bound, the incumbent reads it
// once, and neither ever writes to the other again. A pipe or a socket between
// the two would be a channel and is ruled out.
const JoiningAddrFile = "joining.addr"

// JoiningAddrPath is where a successor reports its address under a state root.
func JoiningAddrPath(stateDir string) string {
	return filepath.Join(stateDir, JoiningAddrFile)
}

// successorArgv is the incumbent's OWN command line with the joining flag
// re-pointed at it.
//
// The successor inherits the incumbent's ARGV as well as its environment, for
// the reason the environment is inherited whole: a successor assembled from a
// curated list would differ from its incumbent in exactly the ways nobody
// thought to list. Emacs launches the daemon with no argv at all, so in
// production this is just the joining flag -- but every flag the operator or a
// test did pass (the account roots, the shim entry, the webapp dist, the
// prompts directory) is the incumbent's configuration, and a successor
// without it is a different daemon.
func successorArgv(incumbent []string, address string) []string {
	out := make([]string, 0, len(incumbent)+2)
	for i := 0; i < len(incumbent); i++ {
		arg := incumbent[i]
		bare := strings.TrimLeft(arg, "-")
		name, _, hasValue := strings.Cut(bare, "=")
		if name != joiningName {
			out = append(out, arg)
			continue
		}
		// An existing joining flag is DROPPED, value and all: this successor
		// joins the daemon that spawned it, not the one its parent joined.
		if !hasValue && i+1 < len(incumbent) {
			i++
		}
	}
	return append(out, JoiningFlag, address)
}

// joiningName is JoiningFlag without its dashes, for argv matching.
const joiningName = "joining"

// ReportJoiningAddr is the SUCCESSOR's half: write this daemon's bound address
// where the incumbent that spawned it is waiting. It writes atomically, because
// a half-written address would be dialed as a real one.
func ReportJoiningAddr(stateDir, address string) error {
	if strings.TrimSpace(address) == "" {
		return fmt.Errorf("rollout: a joining daemon reports a non-blank address")
	}
	path := JoiningAddrPath(stateDir)
	if err := os.MkdirAll(stateDir, 0o755); err != nil {
		return fmt.Errorf("rollout: create the state root %s: %w", stateDir, err)
	}
	tmp, err := os.CreateTemp(stateDir, "joining-*.addr")
	if err != nil {
		return fmt.Errorf("rollout: create the joining address report: %w", err)
	}
	if _, err := tmp.WriteString(address + "\n"); err != nil {
		return fmt.Errorf("rollout: write the joining address report: %w", errors.Join(err, tmp.Close(), os.Remove(tmp.Name())))
	}
	if err := tmp.Close(); err != nil {
		return fmt.Errorf("rollout: close the joining address report: %w", errors.Join(err, os.Remove(tmp.Name())))
	}
	if err := os.Rename(tmp.Name(), path); err != nil {
		return fmt.Errorf("rollout: install the joining address report: %w", errors.Join(err, os.Remove(tmp.Name())))
	}
	return nil
}

// ReadJoiningAddr reads a reported address. The bool is false when none has
// been reported yet, which is the ordinary state while the successor is still
// binding.
func ReadJoiningAddr(stateDir string) (string, bool, error) {
	body, err := os.ReadFile(JoiningAddrPath(stateDir))
	if os.IsNotExist(err) {
		return "", false, nil
	}
	if err != nil {
		return "", false, fmt.Errorf("rollout: read the joining address report: %w", err)
	}
	address := strings.TrimSpace(string(body))
	if address == "" {
		return "", false, nil
	}
	return address, true, nil
}

// ProcessSpawner is the production SuccessorSpawner: it starts the daemon
// binary again with --joining and waits for the child to report its address.
type ProcessSpawner struct {
	// Exe is the daemon binary to start.
	Exe string
	// StateDir is the shared state root the report travels through.
	StateDir string
	// Poll is how often the report file is checked. It is a field so a test
	// drives it, and it is a POLL rather than a watch because the report is a
	// single file written once: a filesystem watch would be more machinery for
	// the same answer.
	Poll time.Duration
	// Timeout bounds the wait. A successor that never reports is a failed
	// handover, and nothing is announced.
	Timeout time.Duration
}

// NewProcessSpawner builds the production spawner.
func NewProcessSpawner(exe, stateDir string) *ProcessSpawner {
	return &ProcessSpawner{Exe: exe, StateDir: stateDir, Poll: 50 * time.Millisecond, Timeout: 30 * time.Second}
}

// Spawn starts the successor and waits for its address.
//
// The child inherits THIS PROCESS'S ENVIRONMENT WHOLE — the state root, the
// store socket, the vendor guard, everything — because a successor assembled
// from a curated allowlist would differ from its incumbent in exactly the ways
// nobody thought to list.
func (s *ProcessSpawner) Spawn(ctx context.Context, incumbentAddress string) (string, error) {
	if strings.TrimSpace(s.Exe) == "" {
		return "", fmt.Errorf("rollout: no daemon binary to spawn the successor from")
	}
	if strings.TrimSpace(incumbentAddress) == "" {
		return "", fmt.Errorf("rollout: the successor is told the incumbent's address, never left to infer it")
	}
	// A report left by an earlier handover would be read as this one's answer.
	if err := os.Remove(JoiningAddrPath(s.StateDir)); err != nil && !os.IsNotExist(err) {
		return "", fmt.Errorf("rollout: clear the stale joining address report: %w", err)
	}

	// NOT exec.CommandContext, AND THAT IS THE WHOLE POINT. CommandContext
	// kills the child when ctx is done, and ctx here is the incumbent's own
	// serving lifetime -- the very thing the handover ends. Bound that way the
	// successor was SIGKILLed the instant the outgoing daemon finished its
	// orderly exit: it served for the two seconds the exit takes, then died
	// without a shutdown record, and Emacs -- which had already adopted every
	// workspace onto it and promoted it to primary -- found the address it had
	// just been handed refusing connections. ctx still bounds the WAIT below,
	// which is this call's own work; it must not bound the process this call
	// exists to leave running.
	cmd := exec.Command(s.Exe, successorArgv(os.Args[1:], incumbentAddress)...)
	cmd.Env = os.Environ()
	cmd.Stdout, cmd.Stderr = os.Stdout, os.Stderr
	// ITS OWN SESSION, WHICH IS WHAT "OUTLIVES" ACTUALLY TAKES.
	//
	// Emacs spawns the incumbent through `make-process' with the default
	// connection type, so the daemon runs on a PTY that Emacs owns. A child
	// started plainly inherits that controlling terminal and the incumbent's
	// process group -- so when the incumbent exited and Emacs closed the pty
	// master, the kernel sent SIGHUP to the whole foreground group and took
	// the successor down with it. The successor died with no shutdown record,
	// two seconds after a handover that had already moved every workspace onto
	// it, and Emacs found the address it had just been promoted to refusing
	// connections.
	//
	// Setsid makes the successor a session leader with NO controlling
	// terminal, which is the structural form of the guarantee: no signal aimed
	// at the incumbent's terminal, session or process group can reach it. It
	// is not a matter of the incumbent exiting politely enough.
	cmd.SysProcAttr = &syscall.SysProcAttr{Setsid: true}
	if err := cmd.Start(); err != nil {
		return "", fmt.Errorf("rollout: start the successor %s: %w", s.Exe, err)
	}
	// The successor OUTLIVES this process by design, so its exit is never
	// waited on: releasing the handle is all that is owed.
	go func() { _ = cmd.Wait() }()

	deadline := time.NewTimer(s.Timeout)
	defer deadline.Stop()
	poll := time.NewTicker(s.Poll)
	defer poll.Stop()
	for {
		address, reported, err := ReadJoiningAddr(s.StateDir)
		if err != nil {
			return "", err
		}
		if reported {
			return address, nil
		}
		select {
		case <-ctx.Done():
			return "", ctx.Err()
		case <-deadline.C:
			return "", fmt.Errorf("rollout: the successor did not report an address within %s", s.Timeout)
		case <-poll.C:
		}
	}
}
