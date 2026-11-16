// Package childgroup is what every agent-repl process that spawns a child
// shares about that child: the environment it must not inherit, and the
// kill of the process group it leads.
package childgroup

import (
	"fmt"
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

// KillGroup SIGKILLs every member of the process group PID leads until no
// live member is left. It answers nil once it killed the group, and
// os.ErrProcessDone when the group held no live member to begin with.
func KillGroup(pid int) error { return killUntilEmpty(pid, syscall.Kill, killableMembers) }

// maxKillPasses bounds killUntilEmpty's passes. Each pass kills every live
// member the group holds, and a killed member forks no more, so only a fork
// already in flight when a pass lands can add one; a group still holding
// members after this many passes is forking faster than it is killed.
const maxKillPasses = 64

// killableMembers answers the members of process group PGID that a kill can
// still reach: neither exited nor exiting (Exiting), so a member SIGKILLed
// by an earlier pass is never counted again while its exit completes.
func killableMembers(pgid int) ([]Member, error) {
	live, err := LiveMembers(pgid)
	if err != nil {
		return nil, err
	}
	var out []Member
	for _, m := range live {
		exiting, err := Exiting(m.Pid)
		if err != nil {
			return nil, err
		}
		if !exiting {
			out = append(out, m)
		}
	}
	return out, nil
}

// killUntilEmpty is KillGroup over its kill and its reading of the group's
// killable members, so a test can stand in for the kernel.
//
// ONE KILL IS NOT ENOUGH. A member caught mid-fork by the kill can finish the
// fork after the group was signalled, and the child joins the group
// unsignalled; and on darwin a group whose leader is an unreaped zombie
// answers the group kill with EPERM without signalling the live child beside
// it (SignalMembers). A shell's background subshell did both, outliving its
// stopped run and holding the run's output pipe open forever (2026-11-16).
// So the group's live members are read after every pass, and the group is
// killed again until none is left.
func killUntilEmpty(pgid int, kill func(int, syscall.Signal) error, members func(int) ([]Member, error)) error {
	for pass := 0; pass < maxKillPasses; pass++ {
		live, err := members(pgid)
		if err != nil {
			return fmt.Errorf("childgroup: read the members of process group %d: %w", pgid, err)
		}
		if len(live) == 0 {
			if pass == 0 {
				return os.ErrProcessDone
			}
			return nil
		}
		if err := SignalMembers(pgid, syscall.SIGKILL, kill, func(int) ([]Member, error) { return live, nil }); err != nil {
			return err
		}
	}
	return fmt.Errorf("childgroup: process group %d still had live members after %d kills", pgid, maxKillPasses)
}
