package workspacefind

import (
	"context"
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

// HeadReader is the registry read a locate's candidates come from
// (wsm.DB's head reads).
type HeadReader interface {
	WorkspaceHeadsAt(ctx context.Context, dirs []string) ([]wsm.WorkspaceHead, error)
	WorkspaceHeadsNamed(ctx context.Context, name string) ([]wsm.WorkspaceHead, error)
}

// Candidates reads, off REGISTRY, the heads of every workspace BY could
// match, in registry order: those of its name, those at its directory, or
// those at its containing directory or any ancestor of it. Locate picks the
// matches among them.
//
// A LOCATE NEVER READS THE WHOLE REGISTRY. Every arm names the rows it could
// match up front, so the read is an index probe per directory however many
// workspaces the registry holds; reading every whole row held the state
// database's one connection for hundreds of milliseconds a locate, and the
// sidecar's bounded locate ran past its deadline behind it (2026-11-25).
func Candidates(ctx context.Context, registry HeadReader, by LocateBy) ([]wsm.WorkspaceHead, error) {
	switch {
	case by.Name != "":
		return registry.WorkspaceHeadsNamed(ctx, by.Name)
	case by.At != "":
		return registry.WorkspaceHeadsAt(ctx, []string{filepath.Clean(by.At)})
	case by.Containing != "":
		return registry.WorkspaceHeadsAt(ctx, selfAndAncestors(filepath.Clean(by.Containing)))
	default:
		panic("workspacefind: a locate names no name, directory or containing directory")
	}
}

// selfAndAncestors answers DIR and every directory above it, up to the root:
// every worktree that could contain DIR.
func selfAndAncestors(dir string) []string {
	out := []string{dir}
	for {
		parent := filepath.Dir(dir)
		if parent == dir {
			return out
		}
		out = append(out, parent)
		dir = parent
	}
}

// Locate answers the locatable workspaces of WORKSPACES that BY matches, in
// registry order; PUSH is the push the sidebar publishes now, every stream
// set.
func Locate(workspaces []wsm.WorkspaceHead, push *frontendv1.RosterPush, by LocateBy) []wsm.WorkspaceHead {
	drawn := map[wsm.WorkspaceID]bool{}
	for _, row := range rosterwalk.AllRows(push) {
		drawn[wsm.WorkspaceID(row.GetWorkspace().GetWorkspace().GetId())] = true
	}
	var out []wsm.WorkspaceHead
	var deepest *wsm.WorkspaceHead
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
func deeper(dir string, ws wsm.WorkspaceHead, best *wsm.WorkspaceHead) bool {
	if best == nil {
		return true
	}
	bestDir := filepath.Clean(best.Dir)
	return len(dir) > len(bestDir) || (dir == bestDir && best.Closed && !ws.Closed)
}
