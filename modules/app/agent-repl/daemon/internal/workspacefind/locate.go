package workspacefind

import (
	"path/filepath"

	frontendv1 "agentrepl/proto/frontend/v1"
	"agentrepl/protohelpers/rosterwalk"

	"agent-repld/internal/dirpath"
	"agent-repld/internal/wsm"
)

// Locating answers agentrepl.v1.LocateWorkspace: which workspace a name, a
// directory, or a directory inside a worktree names.
//
// THE REGISTRY ANSWERS, NOT THE ROSTER. A workspace is locatable when it is
// open, hidden merge workspaces and undrawn children included, when the
// roster's streams draw it (a closed row), or when it is merged: closed with
// its merge landed, which no stream carries (its row is paged by
// agentrepl.v1.ListMergedWorkspaces), so the registry says so. The agent
// CLI resolved its caller off the roster alone until 2026-11-14, so a merge
// workspace's fixing turn, and every open workspace the sidebar did not draw,
// was refused "inside no workspace the daemon serves" from its own worktree.
// Threads share their workspace's directory and are never located.

// LocateBy is what a locate matches on: exactly one of its fields is set.
type LocateBy struct {
	// Name matches every locatable workspace of this name.
	Name string
	// At matches the locatable workspace whose worktree is this directory.
	At string
	// Containing matches the locatable workspace whose worktree contains
	// this directory, the deepest when worktrees nest.
	Containing string
}

// Locate answers the locatable workspaces of WORKSPACES that BY matches, in
// registry order; PUSH is the push the sidebar publishes now, every stream
// set.
func Locate(workspaces []wsm.Workspace, push *frontendv1.RosterPush, by LocateBy) []wsm.Workspace {
	drawn := map[wsm.WorkspaceID]bool{}
	for _, row := range rosterwalk.AllRows(push) {
		drawn[wsm.WorkspaceID(row.GetWorkspace().GetWorkspace().GetId())] = true
	}
	var out []wsm.Workspace
	var deepest *wsm.Workspace
	for _, ws := range workspaces {
		if ws.IsThread() || ws.Dir == "" || (ws.Closed && !drawn[ws.ID] && !ws.InMergedBand()) {
			continue
		}
		dir := filepath.Clean(ws.Dir)
		switch {
		case by.Name != "":
			if ws.Name == by.Name {
				out = append(out, ws)
			}
		case by.At != "":
			if dir == filepath.Clean(by.At) {
				out = append(out, ws)
			}
		case by.Containing != "":
			if dirpath.Within(filepath.Clean(by.Containing), dir) && deeper(dir, ws, deepest) {
				deepest = &ws
			}
		default:
			panic("workspacefind: a locate names no name, directory or containing directory")
		}
	}
	if deepest != nil {
		out = append(out, *deepest)
	}
	return out
}

// deeper reports whether WS, at DIR, is a better containing match than BEST:
// a deeper worktree, or the same one open where BEST is closed.
func deeper(dir string, ws wsm.Workspace, best *wsm.Workspace) bool {
	if best == nil {
		return true
	}
	bestDir := filepath.Clean(best.Dir)
	return len(dir) > len(bestDir) || (dir == bestDir && best.Closed && !ws.Closed)
}
