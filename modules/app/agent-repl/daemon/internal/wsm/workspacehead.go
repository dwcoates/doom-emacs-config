package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"strings"

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
	// Merged reports that the workspace's merge landed: its merged_at is
	// recorded (Workspace.MergedAt). The instant itself is left to the full
	// row, so a head stays comparable.
	Merged bool
}

// Head is the workspace's head: the columns WorkspaceHead reads, as this
// full row holds them.
func (ws Workspace) Head() WorkspaceHead {
	return WorkspaceHead{ID: ws.ID, Kind: ws.Kind, Repo: ws.Repo, Dir: ws.Dir, Name: ws.Name, Closed: ws.Closed, Merged: ws.MergedAt != nil}
}

// MergeSettled reports whether the workspace's merge has settled
// (Workspace.MergeSettled): it is closed and its merge landed. It is the ONE
// spelling, which the full row's answer is read through.
func (h WorkspaceHead) MergeSettled() bool { return h.Closed && h.Merged }

// InMergedBand reports whether the workspace is in the sidebar's merged set
// (Workspace.InMergedBand): its merge has settled and it is no chat thread.
// It is the ONE spelling, which the full row's answer is read through.
func (h WorkspaceHead) InMergedBand() bool { return !h.IsThread() && h.MergeSettled() }

// IsThread reports a zettel chat thread (Workspace.IsThread).
func (h WorkspaceHead) IsThread() bool { return h.Kind == WorkspaceKindThread }

// Ref is the workspace as a WorkspaceRef names it (Workspace.Ref).
func (h WorkspaceHead) Ref() *workspacev1.WorkspaceRef {
	return FamilyRef{ID: h.ID, Dir: h.Dir, Name: h.Name}.Ref()
}

// workspaceHeadColumns are the columns WorkspaceHead reads, in scan order.
const workspaceHeadColumns = `id, kind, repo_id, dir, name, closed, merged_at IS NOT NULL`

// scanWorkspaceHead reads one WorkspaceHead from a row of
// workspaceHeadColumns: the ONE decoding every head read shares.
func scanWorkspaceHead(row interface{ Scan(...any) error }) (WorkspaceHead, error) {
	var out WorkspaceHead
	var kind string
	if err := row.Scan(&out.ID, &kind, &out.Repo, &out.Dir, &out.Name, &out.Closed, &out.Merged); err != nil {
		return WorkspaceHead{}, err
	}
	var err error
	out.Kind, err = decodeWorkspaceKind(out.ID, kind)
	if err != nil {
		return WorkspaceHead{}, err
	}
	return out, nil
}

// WorkspaceHead loads one workspace's head by id.
func (s *store) WorkspaceHead(ctx context.Context, id WorkspaceID) (WorkspaceHead, error) {
	var out WorkspaceHead
	err := s.read(ctx, "daemon.wsm.workspace_head", dlog.Context{"workspace": string(id)}, func(ctx context.Context) error {
		head, err := scanWorkspaceHead(s.q(ctx).QueryRowContext(ctx, `SELECT `+workspaceHeadColumns+` FROM workspaces WHERE id = ?`, id))
		if errors.Is(err, sql.ErrNoRows) {
			return fmt.Errorf("wsm: workspace %s: %w", id, ErrNotFound)
		}
		out = head
		return err
	})
	return out, err
}

// WorkspaceHeadsAt loads the heads of the workspaces whose worktree is one of
// DIRS, compared as given, in registry order (created_at, id). No dirs is no
// heads, with nothing read.
//
// A LOCATE READS THE HEADS OF ITS CANDIDATES, NEVER THE WHOLE REGISTRY. The
// directory column is unique, so this is one index probe per dir. The
// daemon's locate read every whole row instead (ListWorkspaces), each
// deriving a dozen facts by subquery; on the ~1,700-workspace registry that
// held the state database's one connection ~330ms a read (median of the
// over-budget holds, 2026-11-25), and the 0.573.5 handover's sidecar locate
// ran past its 400ms bound inside it.
func (s *store) WorkspaceHeadsAt(ctx context.Context, dirs []string) ([]WorkspaceHead, error) {
	if len(dirs) == 0 {
		return nil, nil
	}
	args := make([]any, len(dirs))
	for i, dir := range dirs {
		args[i] = dir
	}
	marks := strings.TrimSuffix(strings.Repeat("?, ", len(dirs)), ", ")
	return s.workspaceHeads(ctx, "daemon.wsm.workspace_heads_at", dlog.Context{"dirs": dirs},
		`SELECT `+workspaceHeadColumns+` FROM workspaces WHERE dir IN (`+marks+`) ORDER BY created_at, id`, args...)
}

// WorkspaceHeadsNamed loads the heads of every workspace named NAME, in
// registry order (created_at, id).
func (s *store) WorkspaceHeadsNamed(ctx context.Context, name string) ([]WorkspaceHead, error) {
	return s.workspaceHeads(ctx, "daemon.wsm.workspace_heads_named", dlog.Context{"name": name},
		`SELECT `+workspaceHeadColumns+` FROM workspaces WHERE name = ? ORDER BY created_at, id`, name)
}

// workspaceHeads is the one read of a set of heads, all-or-nothing.
func (s *store) workspaceHeads(ctx context.Context, op string, fields dlog.Context, query string, args ...any) ([]WorkspaceHead, error) {
	var out []WorkspaceHead
	err := s.read(ctx, op, fields, func(ctx context.Context) error {
		heads, err := listRows(ctx, s.q(ctx), query, scanWorkspaceHead, args...)
		out = heads
		return err
	})
	if err != nil {
		return nil, err
	}
	return out, nil
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
