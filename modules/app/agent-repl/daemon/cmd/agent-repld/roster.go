package main

import (
	"fmt"
	"io"
	"path/filepath"
)

// What the commands that call the daemon share: a directory's canonical
// spelling, and the one writer.

// canonicalDir resolves a directory's symlinks through its longest existing
// prefix, so a path that no longer exists (a landed workspace's removed
// worktree) still compares equal to the spelling it had.
func canonicalDir(path string) string {
	clean := filepath.Clean(path)
	if resolved, err := filepath.EvalSymlinks(clean); err == nil {
		return resolved
	}
	parent := filepath.Dir(clean)
	if parent == clean {
		return clean
	}
	return filepath.Join(canonicalDir(parent), filepath.Base(clean))
}

// say is the verb's one writer. The verb is a CLI whose product is its output:
// every line goes to the injected stdout or stderr, and the daemon logs every
// decision it makes itself.
func say(to io.Writer, format string, a ...any) {
	fmt.Fprintf(to, format, a...)
}
