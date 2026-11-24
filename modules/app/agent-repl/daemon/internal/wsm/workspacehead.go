package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"

	"agent-repld/internal/dlog"
	workspacev1 "agentrepl/proto/workspace/v1"
)

// WorkspaceHead is the part of a workspace's row a request is resolved by:
// its own columns, read by primary key, with none of the facts a full
// Workspace derives from other tables (its turns, its merge queue, its
// creation job).
//
// A REQUEST'S RESOLUTION READS THE HEAD, NEVER THE WHOLE ROW. The whole row
// derives a dozen facts by subquery, most of them over the workspace's every
// turn; on the 2026-11-23 19:29 handover the server read it twice per
// request, 371-512 times a second for the webapp's ClientLog burst, and those
// reads held the state database's one connection 400-800ms of every second
// while every other operation of the daemon queued behind them. A handler
// that needs a derived fact reads the whole row itself.
type WorkspaceHead struct {
	ID WorkspaceID
	// Kind is what the workspace is (Workspace.Kind).
	Kind WorkspaceKind
	// Repo is the repository the worktree belongs to.
	Repo RepoID
	// Dir is the normalized worktree directory.
	Dir string
	// Name is the workspace's display name.
	Name string
	// Closed reports a closed workspace.
	Closed bool
}

// Head is the workspace's head: the columns WorkspaceHead reads, as this
// full row holds them.
func (ws Workspace) Head() WorkspaceHead {
	return WorkspaceHead{ID: ws.ID, Kind: ws.Kind, Repo: ws.Repo, Dir: ws.Dir, Name: ws.Name, Closed: ws.Closed}
}

// IsThread reports a zettel chat thread (Workspace.IsThread).
func (h WorkspaceHead) IsThread() bool { return h.Kind == WorkspaceKindThread }

// Ref is the workspace as a WorkspaceRef names it (Workspace.Ref).
func (h WorkspaceHead) Ref() *workspacev1.WorkspaceRef {
	return FamilyRef{ID: h.ID, Dir: h.Dir, Name: h.Name}.Ref()
}

// workspaceHeadColumns are the columns WorkspaceHead reads, in scan order.
const workspaceHeadColumns = `id, kind, repo_id, dir, name, closed`

// WorkspaceHead loads one workspace's head by id.
func (s *store) WorkspaceHead(ctx context.Context, id WorkspaceID) (WorkspaceHead, error) {
	var out WorkspaceHead
	err := s.read(ctx, "daemon.wsm.workspace_head", dlog.Context{"workspace": string(id)}, func(ctx context.Context) error {
		var kind string
		err := s.q(ctx).QueryRowContext(ctx, `SELECT `+workspaceHeadColumns+` FROM workspaces WHERE id = ?`, id).
			Scan(&out.ID, &kind, &out.Repo, &out.Dir, &out.Name, &out.Closed)
		if errors.Is(err, sql.ErrNoRows) {
			return fmt.Errorf("wsm: workspace %s: %w", id, ErrNotFound)
		}
		if err != nil {
			return err
		}
		out.Kind, err = decodeWorkspaceKind(out.ID, kind)
		return err
	})
	return out, err
}

// decodeWorkspaceKind decodes the kind column of workspace ID's row: the one
// decoding both the head and the full row go through.
func decodeWorkspaceKind(id WorkspaceID, kind string) (WorkspaceKind, error) {
	switch WorkspaceKind(kind) {
	case WorkspaceKindRepository, WorkspaceKindThread:
		return WorkspaceKind(kind), nil
	default:
		return "", &DecodeError{Table: "workspaces", Row: string(id), Field: "kind", Err: fmt.Errorf("unknown workspace kind %q", kind)}
	}
}
