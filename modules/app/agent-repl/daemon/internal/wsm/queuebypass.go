package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"strings"
	"time"

	"agent-repld/internal/dlog"
)

// queueBypassDDL is the layout-113 addition: which workspace holds its
// repository's queue bypass mode (agentrepl.v1.UpdateQueueBypassMode), and,
// per workspace, the conversation it is NOT running: the ordinary one while
// it holds the mode, the bypass one while it does not. IT IS ADDITIVE: two new
// tables the previous build never names.
//
// ONE HOLDER PER REPOSITORY IS THE TABLE'S KEY, not a check: the holder row is
// keyed by repository, so a second workspace's take is a constraint the
// database refuses, whatever two callers race. The holder column is UNIQUE
// too, so one workspace never holds two repositories' modes.
//
// THE PARKED CONVERSATION IS ONE ROW PER WORKSPACE. A switch swaps it with
// the session record's binding, so the conversation the workspace stepped out
// of is kept, with its transcript and its warm cache, for the next switch.
const queueBypassDDL = `
CREATE TABLE queue_bypass_holders (
  repo_id      TEXT PRIMARY KEY REFERENCES repositories(id) ON DELETE CASCADE,
  workspace_id TEXT NOT NULL UNIQUE REFERENCES workspaces(id) ON DELETE CASCADE,
  held_since   INTEGER NOT NULL
);
CREATE TABLE workspace_parked_conversations (
  workspace_id      TEXT PRIMARY KEY REFERENCES workspaces(id) ON DELETE CASCADE,
  vendor_session_id TEXT NOT NULL
);
`

// ErrQueueBypassHeld refuses a take while another workspace of the same
// repository holds the mode. The error wraps it beside the holder's id.
var ErrQueueBypassHeld = errors.New("wsm: another workspace of the repository holds the queue bypass mode")

// QueueBypass is one workspace's standing toward its repository's queue
// bypass mode.
type QueueBypass struct {
	// Holds reports that this workspace holds the mode.
	Holds bool
	// HeldBy is the workspace holding the mode when it is another one of the
	// same repository; empty when the mode is free or this workspace holds it.
	HeldBy WorkspaceID
	// RepoID is the workspace's repository, whose queue the mode stops.
	RepoID RepoID
	// ParkedVendorSessionID is the conversation the workspace is not running,
	// kept for the next switch; empty when it has none yet.
	ParkedVendorSessionID string
}

// QueueBypass answers workspace ID's standing toward its repository's queue
// bypass mode. A workspace the registry does not hold is ErrNotFound.
func (s *store) QueueBypass(ctx context.Context, id WorkspaceID) (QueueBypass, error) {
	var out QueueBypass
	err := s.read(ctx, "daemon.wsm.queue_bypass", dlog.Context{"workspace": string(id)}, func(ctx context.Context) error {
		var err error
		out, err = readQueueBypass(ctx, s.q(ctx), id)
		return err
	})
	return out, err
}

// TakeQueueBypass records workspace ID as its repository's queue bypass
// holder, refusing with ErrQueueBypassHeld while another workspace holds it.
// A workspace that already holds it is left as it is.
func (s *store) TakeQueueBypass(ctx context.Context, id WorkspaceID, at time.Time) error {
	const op = "daemon.wsm.take_queue_bypass"
	fields := dlog.Context{"workspace": string(id)}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		standing, err := readQueueBypass(ctx, tx, id)
		if err != nil {
			return err
		}
		switch {
		case standing.Holds:
			return nil
		case standing.HeldBy != "":
			return fmt.Errorf("wsm: workspace %s holds repository %s's queue bypass mode: %w", standing.HeldBy, standing.RepoID, ErrQueueBypassHeld)
		}
		_, err = tx.ExecContext(ctx,
			`INSERT INTO queue_bypass_holders (repo_id, workspace_id, held_since) VALUES (?, ?, ?)`,
			standing.RepoID, id, at.UnixNano())
		return err
	})
}

// ReleaseQueueBypass clears workspace ID's hold on its repository's queue
// bypass mode. A workspace that holds none is left as it is.
func (s *store) ReleaseQueueBypass(ctx context.Context, id WorkspaceID) error {
	const op = "daemon.wsm.release_queue_bypass"
	return s.write(ctx, op, dlog.Context{"workspace": string(id)}, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx, `DELETE FROM queue_bypass_holders WHERE workspace_id = ?`, id)
		return err
	})
}

// SetParkedConversation records VENDORSESSIONID as the conversation workspace
// ID is not running. A blank id is a bug in the caller, refused loudly: a
// switch always parks the conversation it stepped out of.
func (s *store) SetParkedConversation(ctx context.Context, id WorkspaceID, vendorSessionID string) error {
	const op = "daemon.wsm.set_parked_conversation"
	fields := dlog.Context{"workspace": string(id), "vendor_session_id": vendorSessionID}
	if strings.TrimSpace(vendorSessionID) == "" {
		err := fmt.Errorf("wsm: a parked conversation names its conversation (workspace %s)", id)
		s.log.Error(op, "refused a blank parked conversation", withError(fields, err))
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx,
			`INSERT INTO workspace_parked_conversations (workspace_id, vendor_session_id) VALUES (?, ?)
			 ON CONFLICT(workspace_id) DO UPDATE SET vendor_session_id = excluded.vendor_session_id`, id, vendorSessionID)
		return err
	})
}

// readQueueBypass reads workspace ID's standing on Q.
func readQueueBypass(ctx context.Context, q remoteControlQuerier, id WorkspaceID) (QueueBypass, error) {
	var (
		repo   string
		holder sql.NullString
		parked sql.NullString
	)
	err := q.QueryRowContext(ctx, `SELECT w.repo_id, h.workspace_id, p.vendor_session_id
		FROM workspaces AS w
		LEFT JOIN queue_bypass_holders AS h ON h.repo_id = w.repo_id
		LEFT JOIN workspace_parked_conversations AS p ON p.workspace_id = w.id
		WHERE w.id = ?`, id).Scan(&repo, &holder, &parked)
	if errors.Is(err, sql.ErrNoRows) {
		return QueueBypass{}, fmt.Errorf("wsm: workspace %s: %w", id, ErrNotFound)
	}
	if err != nil {
		return QueueBypass{}, err
	}
	out := QueueBypass{RepoID: RepoID(repo), ParkedVendorSessionID: parked.String}
	if holder.Valid {
		if WorkspaceID(holder.String) == id {
			out.Holds = true
		} else {
			out.HeldBy = WorkspaceID(holder.String)
		}
	}
	return out, nil
}
