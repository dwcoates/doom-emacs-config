// Package childproc builds every child process the daemon starts, each with
// the working directory it runs in named by its caller.
//
// A CHILD NEVER INHERITS THE DAEMON'S DIRECTORY. A child started without a
// directory of its own runs wherever the daemon happens to stand, and the
// daemon once stood in a workspace's worktree for a whole chain of rollouts:
// Emacs launched it from a workspace buffer, every successor inherited that
// directory, and when the worktree was removed each `claude -p` the daemon
// started refused with "The current working directory was deleted", so every
// create that needed a name failed (2026-10-18). The daemon now moves to
// Stable at boot, and every child names its directory here as well, so no
// child's directory depends on where some earlier process was launched.
// TestEveryChildIsBuiltHere holds every exec.Command site to this package.
package childproc

import (
	"context"
	"fmt"
	"os/exec"
	"path/filepath"
)

// Stable is the directory a daemon, and a child with no directory of its own,
// runs in. `/` always exists and no workspace's teardown removes it.
const Stable = "/"

// Command is exec.Command run in dir.
func Command(dir, name string, args ...string) *exec.Cmd {
	return inDir(exec.Command(name, args...), dir)
}

// CommandContext is exec.CommandContext run in dir.
func CommandContext(ctx context.Context, dir, name string, args ...string) *exec.Cmd {
	return inDir(exec.CommandContext(ctx, name, args...), dir)
}

// inDir sets cmd's directory. A dir that is not absolute is a caller's bug:
// an empty one would inherit the daemon's directory, and a relative one would
// resolve against it. Such a cmd never starts: Start answers the refusal
// through the caller's own error path.
func inDir(cmd *exec.Cmd, dir string) *exec.Cmd {
	if !filepath.IsAbs(dir) {
		cmd.Err = fmt.Errorf("childproc: %s must run in an absolute directory, not %q", cmd.Path, dir)
		return cmd
	}
	cmd.Dir = dir
	return cmd
}
