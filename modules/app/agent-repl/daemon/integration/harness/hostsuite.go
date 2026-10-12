package harness

import (
	"errors"
	"fmt"
	"io"
	"os"
	"strings"
	"syscall"
	"time"
)

// ONE DAEMON SUITE RUNS ON A HOST AT A TIME.
//
// The live-daemon cap (slots.go) bounds the load ONE suite process makes, and
// DefaultTimeout is measured against exactly that load. Nothing bounded the
// load SEVERAL suites make together: parallel sessions each running the
// integration or e2e suite multiplied the cap, and at a load average of 50-80
// a freshly exec'd daemon spent its whole DefaultTimeout still inside Go's
// package initialization (2026-10-12: seven tests timed out at once with every
// SIGQUIT dump in runtime.doInit), while which tests drew the short straw
// changed run to run. That is not a bound too tight; it is load nothing
// bounded, one level up.
//
// So the host is held by an EXCLUSIVE flock on HostSuiteLockPath for the
// whole of a daemon suite, taken before its builds and released by the kernel
// when the process ends, however it ends: a crashed or killed suite can never
// leave the host held. A second suite WAITS in the kernel's queue for the
// lock, before testing.M.Run, so its -timeout is not spent waiting.

// HostSuiteLockPath is the host's one daemon-suite lock. It is a fixed path
// under /tmp, never under TMPDIR, because sessions run with different TMPDIRs
// and the lock must be one file for the whole host.
const HostSuiteLockPath = "/tmp/agent-repl-daemon-suite.lock"

// holdHost takes path's exclusive flock for this process's lifetime and
// answers the open file that IS the lock. When another suite holds it, the
// holder's record is written to wait (one line), and the call blocks in the
// kernel until the lock is free. The holder's record -- this process's pid
// and start instant -- is written into the file once held, so a waiting suite
// can say whom it waits for.
func holdHost(path string, wait io.Writer) (*os.File, error) {
	f, err := os.OpenFile(path, os.O_CREATE|os.O_RDWR, 0o666)
	if err != nil {
		return nil, fmt.Errorf("harness: open the host suite lock %s: %w", path, err)
	}
	err = syscall.Flock(int(f.Fd()), syscall.LOCK_EX|syscall.LOCK_NB)
	if errors.Is(err, syscall.EWOULDBLOCK) {
		holder, _ := os.ReadFile(path)
		fmt.Fprintf(wait, "harness: another daemon suite holds this host (%s); waiting for it to end\n",
			strings.TrimSpace(string(holder)))
		err = syscall.Flock(int(f.Fd()), syscall.LOCK_EX)
	}
	if err != nil {
		f.Close()
		return nil, fmt.Errorf("harness: take the host suite lock %s: %w", path, err)
	}
	if err := f.Truncate(0); err != nil {
		f.Close()
		return nil, fmt.Errorf("harness: record the host suite lock's holder: %w", err)
	}
	if _, err := fmt.Fprintf(f, "pid %d since %s", os.Getpid(), time.Now().Format(time.RFC3339)); err != nil {
		f.Close()
		return nil, fmt.Errorf("harness: record the host suite lock's holder: %w", err)
	}
	return f, nil
}
