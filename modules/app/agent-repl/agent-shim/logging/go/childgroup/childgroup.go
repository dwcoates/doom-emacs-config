// Package childgroup is what every agent-repl process that spawns a child
// shares about that child: the environment it must not inherit, and the
// kill of the process group it leads.
package childgroup

import (
	"errors"
	"os"
	"strings"
	"syscall"
)

// RepositorySelectors are the environment bindings that select a
// repository, an index, or an object store independently of a child's
// working directory or `git -C dir`. A hook-leaked GIT_DIR is a real,
// previously observed source of bogus work-tree errors, so no child an
// agent-repl process spawns inherits any of them. This is the ONE list: the
// daemon's git client, its script runner, its test harness and the test
// service all scrub through it.
var RepositorySelectors = []string{
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

// SelectsRepository reports whether one `NAME=value` binding is a
// repository selector.
func SelectsRepository(entry string) bool {
	name, _, ok := strings.Cut(entry, "=")
	if !ok {
		return false
	}
	for _, selector := range RepositorySelectors {
		if name == selector {
			return true
		}
	}
	return false
}

// ScrubRepositorySelectors returns ENV with every repository selector
// dropped, everything else in its order.
func ScrubRepositorySelectors(env []string) []string {
	out := make([]string, 0, len(env))
	for _, entry := range env {
		if !SelectsRepository(entry) {
			out = append(out, entry)
		}
	}
	return out
}

// KillGroup SIGKILLs the process group PID leads. A group already gone is
// the state the kill was asked to reach, answered as os.ErrProcessDone.
func KillGroup(pid int) error { return KillGroupWith(syscall.Kill, pid) }

// KillGroupWith is KillGroup over an injected kill, so the answers a kernel
// gives for a finished group can be driven in a test.
//
// TWO ANSWERS MEAN THE GROUP HAS FINISHED. ESRCH is a group with no members
// at all. EPERM is what darwin answers for a group whose members have all
// exited and wait to be reaped: the child finished just as its caller
// cancelled it. The group is the caller's own child's, created with Setpgid
// and running as this user, so EPERM cannot mean a live member this process
// may not signal. Returning either as an error made exec report "canceling
// Cmd: operation not permitted" for a run that had in fact ended (2026-10-06,
// a daemon exiting during its boot's service check).
func KillGroupWith(kill func(int, syscall.Signal) error, pid int) error {
	if err := kill(-pid, syscall.SIGKILL); err != nil {
		if errors.Is(err, syscall.ESRCH) || errors.Is(err, syscall.EPERM) {
			return os.ErrProcessDone
		}
		return err
	}
	return nil
}
