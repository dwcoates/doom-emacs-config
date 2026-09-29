// Package dirpath is the ONE spelling of a directory wherever a directory
// becomes identity: the workspace registry's key, the repository's, and the
// log surfaces' sink key.
//
// THE SPELLING IS THE ON-DISK ONE, CASE INCLUDED. The default macOS volume is
// case-insensitive, so `/Users/x/ChessCom/w` and `/Users/x/chesscom/w` open the
// same directory, and `filepath.EvalSymlinks` answers each in the case it was
// given. Keyed by that answer, the registry minted two workspaces for one
// worktree (223c4ec7b27f4f5a and b58a6f10703e493f, 2026-09-15), and because
// both shared one `.claude/emacs/daemon.log` link on disk, each one's records
// landed in the other's sink (realtest 7, 2026-09-24, attribution-conflict).
// Canonical asks the volume for the path it stores (F_GETPATH on darwin), so
// every spelling of one directory is one string. It does NOT list parent
// directories to find each name's case: the temporary root alone held 310,699
// entries on the owner's box, and a listing per component made every
// registration and every resolve pay for it.
package dirpath

import (
	"errors"
	"fmt"
	"path/filepath"
)

// Canonical returns the one spelling of dir: absolute, cleaned, symlinks
// resolved, and every EXISTING component in the case the volume stores it.
//
// dir MUST ALREADY BE ABSOLUTE. A relative one is refused rather than
// resolved against the daemon's working directory, which is wherever launchd
// started it: that guess turned a producer's `~/.config/doom` into
// `/Users/me/~/.config/doom` (2026-09-28). A producer's path is made absolute
// at its boundary, by Absolute.
//
// A path that does not exist cannot have its symlinks resolved or its case
// read; the deepest existing ancestor is canonicalized instead and the
// remainder appended as given, so a directory under a symlinked or case-folded
// parent still normalizes the same way once it appears.
func Canonical(dir string) (string, error) {
	return osResolver.canonical(dir)
}

// resolver is Canonical with its two filesystem reads injected, so the case
// rule is testable on any volume.
type resolver struct {
	// evalSymlinks resolves symlinks in an existing path, and is how the walk
	// finds the deepest existing ancestor.
	evalSymlinks func(string) (string, error)
	// onDiskPath answers an EXISTING path as the volume stores it, case
	// included (onDiskPath in dirpath_darwin.go and dirpath_other.go).
	onDiskPath func(string) (string, error)
}

// osResolver is the production resolver.
var osResolver = resolver{evalSymlinks: filepath.EvalSymlinks, onDiskPath: onDiskPath}

func (r resolver) canonical(dir string) (string, error) {
	if dir == "" {
		return "", errors.New("dirpath: empty directory")
	}
	if !filepath.IsAbs(dir) {
		return "", fmt.Errorf("dirpath: %q is not an absolute directory, and a relative one names nothing a daemon can resolve", dir)
	}
	abs := filepath.Clean(dir)
	// Walk up to the deepest existing ancestor, resolve that, and re-join the
	// tail so the answer is stable once the leaf is created.
	rest := ""
	head := abs
	for {
		if resolved, err := r.evalSymlinks(head); err == nil {
			stored, err := r.onDiskPath(filepath.Clean(resolved))
			if err != nil {
				return "", fmt.Errorf("dirpath: read the on-disk spelling of %q: %w", resolved, err)
			}
			return filepath.Join(filepath.Clean(stored), rest), nil
		}
		parent := filepath.Dir(head)
		if parent == head {
			return abs, nil
		}
		rest = filepath.Join(filepath.Base(head), rest)
		head = parent
	}
}
