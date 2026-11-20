package main

import (
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"

	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/shimclient"
)

// Which workspace a command's `here` names: the one whose own worktree the
// caller runs in (cliCall.callingRef), found from the caller's worktree root.

// nestedWorktreeError refuses `here` for a caller in a separate git worktree
// nested under a workspace's directory (an Agent-tool worktree under the
// main checkout) whose session names no workspace: the enclosing workspace
// did not ask, so it must never be named.
type nestedWorktreeError struct {
	// worktree is the caller's own git worktree.
	worktree string
	// enclosing is the workspace whose directory the worktree is nested under.
	enclosing *workspacev1.WorkspaceRef
}

func (e *nestedWorktreeError) Error() string {
	return fmt.Sprintf("%s is a separate git worktree nested under the workspace at %s, not that workspace's own, and this session names no workspace (%s is unset); run this from a workspace's own worktree, or pass -as-user when the user asks",
		e.worktree, e.enclosing.GetDir(), shimclient.EnvWorkspaceDir)
}

// unservedSessionError refuses `here` for a caller whose session names a
// workspace the daemon does not serve: the daemon stamps every session it
// spawns with its own workspace's directory, so a directory it does not
// serve is a stale or foreign session, never a workspace to guess at.
type unservedSessionError struct {
	// dir is the directory the session's AGENT_REPL_WORKSPACE_DIR names.
	dir string
}

func (e *unservedSessionError) Error() string {
	return fmt.Sprintf("this session's %s names %s, which is no workspace the daemon serves", shimclient.EnvWorkspaceDir, e.dir)
}

// worktreeRoot answers the root of the git worktree holding DIR: the nearest
// directory at or above DIR that ISROOT marks, empty when none is. Every
// workspace's directory is a worktree's root, so a caller's own worktree is
// the only one that can be its workspace's.
func worktreeRoot(dir string, isRoot func(string) (bool, error)) (string, error) {
	for {
		root, err := isRoot(dir)
		if err != nil {
			return "", err
		}
		if root {
			return dir, nil
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			return "", nil
		}
		dir = parent
	}
}

// isWorktreeRoot reports whether DIR is the root of a git worktree or
// repository: it holds a `.git` entry, a directory in a main checkout and a
// file in a linked worktree, the same mark git itself finds a worktree's top
// level by. A `.git` entry it cannot read is an error, never a guess.
func isWorktreeRoot(dir string) (bool, error) {
	_, err := os.Lstat(filepath.Join(dir, ".git"))
	switch {
	case err == nil:
		return true, nil
	case errors.Is(err, fs.ErrNotExist):
		return false, nil
	}
	return false, fmt.Errorf("tell whether %s is a git worktree's root: %w", dir, err)
}
