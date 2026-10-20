package main

import (
	"fmt"
	"io"
	"path/filepath"
	"strings"

	frontendv1 "agentrepl/proto/frontend/v1"
	"agentrepl/protohelpers/rosterwalk"
)

// The roster reads the commands that call the daemon share: which workspace
// a directory is, and the one writer.

// callingRow finds the requesting workspace's row: the one whose worktree
// contains the caller's directory, the deepest when worktrees nest.
func callingRow(roster *frontendv1.WorkspaceRoster, cwd string) *frontendv1.RosterRow {
	var best *frontendv1.RosterRow
	bestLen := -1
	for _, row := range rosterwalk.AllRows(roster) {
		dir := canonicalDir(row.GetWorkspace().GetWorkspace().GetDir())
		if row.GetWorkspace().GetWorkspace().GetDir() == "" || !within(cwd, dir) {
			continue
		}
		if len(dir) > bestLen {
			best, bestLen = row, len(dir)
		}
	}
	return best
}

// findRosterRow finds a workspace's row by its canonical directory, in every
// grouping the roster carries, nested (child) rows included: a child is a
// workspace in its own right, so it can ask for its own merge, which lands in
// its parent.
func findRosterRow(roster *frontendv1.WorkspaceRoster, dir string) *frontendv1.RosterRow {
	for _, row := range rosterwalk.AllRows(roster) {
		if canonicalDir(row.GetWorkspace().GetWorkspace().GetDir()) == dir {
			return row
		}
	}
	return nil
}

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

// within reports whether path is dir or lies beneath it.
func within(path, dir string) bool {
	return path == dir || strings.HasPrefix(path, dir+string(filepath.Separator))
}

// say is the verb's one writer. The verb is a CLI whose product is its output:
// every line goes to the injected stdout or stderr, and the daemon logs every
// decision it makes itself.
func say(to io.Writer, format string, a ...any) {
	fmt.Fprintf(to, format, a...)
}
