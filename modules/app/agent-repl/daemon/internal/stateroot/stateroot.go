// Package stateroot resolves the agent-repl state directory root that the
// Emacs side, the hook scripts, and the managed skills all agree on:
// $AGENT_REPL_STATE_DIR, falling back to ~/.claude-emacs.
//
// The root is a cross-process contract, not a daemon-private path: Emacs
// watches directories under it and the skills write into them, so a
// daemon that resolved it differently would drop messages on the floor
// silently. Keeping the resolution in one place is what makes "the same
// root" checkable rather than a coincidence of three copies agreeing.
package stateroot

import (
	"fmt"
	"os"
	"path/filepath"
)

// EnvVar overrides the root when set.
const EnvVar = "AGENT_REPL_STATE_DIR"

// DefaultDirName is the root's name under the home directory when EnvVar
// is unset.
const DefaultDirName = ".claude-emacs"

// Root returns $AGENT_REPL_STATE_DIR, or ~/.claude-emacs when that is
// unset or empty. An unresolvable home directory is an error rather than
// a guess: writing state under the wrong root is worse than not writing
// it, because the reader would wait forever on a path nobody writes.
func Root() (string, error) {
	if root := os.Getenv(EnvVar); root != "" {
		return root, nil
	}
	home, err := os.UserHomeDir()
	if err != nil {
		return "", fmt.Errorf("stateroot: resolve home dir: %w", err)
	}
	return filepath.Join(home, DefaultDirName), nil
}
