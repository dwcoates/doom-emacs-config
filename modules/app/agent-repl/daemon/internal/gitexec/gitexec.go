// Package gitexec builds the daemon's git invocations.
//
// Every git command the daemon runs selects its repository with `-C dir` and
// with NOTHING else. This package is the single place that guarantee is
// implemented: see AGENTS.md for why an inherited GIT_DIR is a live hazard
// rather than a hypothetical one.
package gitexec

import (
	"context"
	"os"
	"os/exec"
	"strings"
)

// StrippedVars are the environment bindings that select a repository
// independently of `-C dir`. Git exports every one of them into the children a
// hook runs, so every one of them is removed before the daemon spawns git.
var StrippedVars = []string{
	"GIT_DIR",
	"GIT_INDEX_FILE",
	"GIT_WORK_TREE",
	"GIT_OBJECT_DIRECTORY",
	"GIT_COMMON_DIR",
	"GIT_PREFIX",
}

// Command returns `git -C dir args...` with the inherited repository bindings
// stripped from its environment, so `-C dir` is the ONLY repository selector.
// Every other inherited variable (PATH, HOME, the git identity, ...) survives.
func Command(ctx context.Context, dir string, args ...string) *exec.Cmd {
	full := append([]string{"-C", dir}, args...)
	cmd := exec.CommandContext(ctx, "git", full...)
	cmd.Env = StripEnv(os.Environ())
	return cmd
}

// StripEnv returns env with every StrippedVars binding removed. It is exported
// so a caller assembling its own exec.Cmd (a test fixture driver, say) can
// reuse the exact same boundary rather than re-deriving it.
func StripEnv(env []string) []string {
	kept := make([]string, 0, len(env))
	for _, entry := range env {
		if stripped(entry) {
			continue
		}
		kept = append(kept, entry)
	}
	return kept
}

func stripped(entry string) bool {
	for _, name := range StrippedVars {
		if strings.HasPrefix(entry, name+"=") {
			return true
		}
	}
	return false
}
