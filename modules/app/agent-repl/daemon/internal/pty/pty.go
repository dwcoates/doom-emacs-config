// Package pty opens a pseudo-terminal and starts a command on its slave
// side.
//
// The daemon needs one because the Claude login is a full-screen TUI: it
// only renders, and only reads keystrokes, when it believes it is talking
// to a terminal. Handed a pipe it degrades or refuses outright, which is
// the whole reason the login used to be exiled to an Emacs vterm. A PTY
// the daemon owns puts that terminal back under the daemon's control, so
// the webapp can render it.
//
// Stdlib only, deliberately. github.com/creack/pty is the canonical
// library, but it would be a second dependency for ~60 lines of ioctl in
// a daemon whose dependency set is one package wide. The platform halves
// live in pty_darwin.go and pty_linux.go.
package pty

import (
	"fmt"
	"os"
	"os/exec"
	"strings"
	"syscall"
	"unsafe"
)

// winsize mirrors `struct winsize` from <sys/ioctl.h>.
type winsize struct {
	rows   uint16
	cols   uint16
	xpixel uint16
	ypixel uint16
}

// Start opens a PTY, attaches CMD to its slave side, and starts it.
//
// Returns the master: read it for screen output, write it for keystrokes.
// Closing the master is the caller's job, and does NOT reap the child —
// the caller owns cmd.Wait too.
//
// Setsid plus Setctty is what makes the slave the child's CONTROLLING
// terminal. Wiring the three fds alone is not enough: without a
// controlling terminal the TUI still sees no tty and falls back to its
// non-interactive path, which is exactly the failure this package exists
// to avoid.
func Start(cmd *exec.Cmd) (*os.File, error) {
	master, slaveName, err := open()
	if err != nil {
		return nil, err
	}
	// O_NOCTTY: the PARENT must not adopt the slave as its controlling
	// terminal — only the child does, via Setctty below.
	slave, err := os.OpenFile(slaveName, os.O_RDWR|syscall.O_NOCTTY, 0)
	if err != nil {
		_ = master.Close()
		return nil, fmt.Errorf("pty: open slave %s: %w", slaveName, err)
	}
	// The child dups the slave fd across the fork, so the parent's copy is
	// dead weight the moment Start returns. Holding it open would also keep
	// the master's read side from ever seeing EOF when the child exits.
	defer func() { _ = slave.Close() }()

	cmd.Stdin, cmd.Stdout, cmd.Stderr = slave, slave, slave
	cmd.Env = withoutInheritedSize(cmd.Env)
	if cmd.SysProcAttr == nil {
		cmd.SysProcAttr = &syscall.SysProcAttr{}
	}
	cmd.SysProcAttr.Setsid = true
	cmd.SysProcAttr.Setctty = true

	if err := cmd.Start(); err != nil {
		_ = master.Close()
		return nil, fmt.Errorf("pty: start %s: %w", cmd.Path, err)
	}
	return master, nil
}

// withoutInheritedSize returns ENV with COLUMNS and LINES stripped,
// defaulting to the process environment when ENV is nil (cmd.Env's own
// "inherit" convention).
//
// The PTY is the ONLY authority on its child's size, and COLUMNS/LINES
// inherited from the parent are a competing one. Programs that consult
// them — `tput`, and terminal-size libraries generally — prefer them
// OVER the TIOCSWINSZ ioctl, so a stale COLUMNS in the daemon's own
// environment silently overrides Setsize inside the child.
//
// On the login path that is not cosmetic. It is precisely what re-wraps
// the ~350-character OAuth URL that the 400-column pty (login.DefaultCols)
// exists to keep on one line: the daemon sets 400, the child reads
// COLUMNS=134 from the environment its parent was launched with, and the
// URL shreds across three lines exactly as it did before the pty existed.
func withoutInheritedSize(env []string) []string {
	if env == nil {
		env = os.Environ()
	}
	out := make([]string, 0, len(env))
	for _, kv := range env {
		if strings.HasPrefix(kv, "COLUMNS=") || strings.HasPrefix(kv, "LINES=") {
			continue
		}
		out = append(out, kv)
	}
	return out
}

// Setsize sets the window size on F, a PTY master.
//
// Size is not cosmetic on this path. The login TUI hard-wraps its output
// at the column count, and the OAuth URL is ~350 characters, so an
// 80-column terminal shreds it across five lines. The webapp reports its
// real geometry and this keeps the child in step with it.
func Setsize(f *os.File, rows, cols uint16) error {
	ws := winsize{rows: rows, cols: cols}
	if _, _, errno := syscall.Syscall(
		syscall.SYS_IOCTL,
		f.Fd(),
		uintptr(syscall.TIOCSWINSZ),
		uintptr(unsafe.Pointer(&ws)),
	); errno != 0 {
		return fmt.Errorf("pty: set winsize %dx%d: %w", rows, cols, errno)
	}
	return nil
}
