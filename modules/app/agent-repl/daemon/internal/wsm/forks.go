package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"

	"agent-repld/internal/dlog"
)

// A FORK IS A POINTER, NEVER A COPY (docs/protobuf-design/fork-lineage.md).
//
// A forked workspace's vendor conversation is the vendor's own fork of its
// parent's (shim.v1 ForkSession), and its feed reads the conversation it
// inherited from the PARENT's book, bounded to the fork point. Two records hold
// that, and both are the daemon's own:
//
//   - pending_forks: a fork the create recorded and the bring-up has not made
//     yet. It names only the parent; the bring-up asks the parent's shim for
//     the fork point and the child's shim for the fork, and resolves it.
//   - fork_lineage (+ fork_uninherited_turns): a fork that was made — the
//     parent, the parent's book, the instant bounding the inherited rows, and
//     the parent's turns that were open at the fork, whose rows are not the
//     fork's however they are placed.
//
// THE PARENT IS NAMED, NOT REFERENCED. A forgotten parent's workspace row goes,
// but its book stays in the store, and the fork's inherited past is still read
// from it; a foreign key would cascade the fork's lineage away with the parent.

// forkLineageDDL is the layout-47 addition. Kept apart from the rest of the
// schema because two paths write it: a fresh file as part of schemaDDL, and a
// layout-42 file from the 42 -> 43 migration.
const forkLineageDDL = `
CREATE TABLE pending_forks (
  workspace_id        TEXT PRIMARY KEY REFERENCES workspaces(id) ON DELETE CASCADE,
  parent_workspace_id TEXT NOT NULL
);

CREATE TABLE fork_lineage (
  workspace_id        TEXT PRIMARY KEY REFERENCES workspaces(id) ON DELETE CASCADE,
  parent_workspace_id TEXT NOT NULL,
  parent_book         TEXT NOT NULL,
  through_ms          INTEGER NOT NULL
);

CREATE TABLE fork_uninherited_turns (
  workspace_id TEXT NOT NULL REFERENCES workspaces(id) ON DELETE CASCADE,
  turn_id      TEXT NOT NULL,
  PRIMARY KEY (workspace_id, turn_id)
);
`

// ownForkDDL is the layout-114 addition: the conversation a fork of the
// workspace's OWN conversation copies (the first switch into the queue bypass
// mode). A fork of another workspace leaves it NULL and forks whatever that
// workspace runs at the bring-up; an own fork names the conversation because
// the bind that owes it has just stepped the workspace out of it.
const ownForkDDL = `
ALTER TABLE pending_forks ADD COLUMN parent_vendor_session_id TEXT;
`

// forkLineageByConversationDDL is the layout-141 step: a lineage describes a
// CONVERSATION, so it is keyed by the workspace and the conversation the fork
// continues (vendor_session_id), and a bind to another conversation no longer
// forgets it. A queue-bypass switch back onto a parked conversation that was a
// fork drew no inherited past once the switch away had deleted its lineage
// (2026-11-10). An own fork also records the conversation it copied
// (parent_vendor_session_id), so the walk continues through that
// conversation's own lineage; a fork of another workspace leaves it NULL and
// continues through the conversation that workspace runs.
//
// A row is carried to the conversation its workspace's session names: every
// bind of the build before this step deleted the lineage it left, so a
// standing row is the bound conversation's. An own fork's copied conversation
// is the one parked beside it, when one is. A row whose workspace names no
// conversation breaks that invariant and fails the step (vendor_session_id is
// NOT NULL) rather than being dropped.
//
// A fresh file runs the same statements over its empty tables. IT IS
// BREAKING: the build before it inserts lineage rows naming no conversation.
const forkLineageByConversationDDL = `
CREATE TABLE fork_lineage_by_conversation (
  workspace_id             TEXT NOT NULL REFERENCES workspaces(id) ON DELETE CASCADE,
  vendor_session_id        TEXT NOT NULL,
  parent_workspace_id      TEXT NOT NULL,
  parent_vendor_session_id TEXT,
  parent_book              TEXT NOT NULL,
  through_ms               INTEGER NOT NULL,
  PRIMARY KEY (workspace_id, vendor_session_id)
);
INSERT INTO fork_lineage_by_conversation
  (workspace_id, vendor_session_id, parent_workspace_id, parent_vendor_session_id, parent_book, through_ms)
SELECT l.workspace_id, NULLIF(s.vendor_session_id, ''), l.parent_workspace_id,
  CASE WHEN l.parent_workspace_id = l.workspace_id THEN NULLIF(p.vendor_session_id, '') END,
  l.parent_book, l.through_ms
FROM fork_lineage AS l
LEFT JOIN sessions AS s ON s.workspace_id = l.workspace_id
LEFT JOIN workspace_parked_conversations AS p ON p.workspace_id = l.workspace_id;
CREATE TABLE fork_uninherited_turns_by_conversation (
  workspace_id      TEXT NOT NULL REFERENCES workspaces(id) ON DELETE CASCADE,
  vendor_session_id TEXT NOT NULL,
  turn_id           TEXT NOT NULL,
  PRIMARY KEY (workspace_id, vendor_session_id, turn_id)
);
INSERT INTO fork_uninherited_turns_by_conversation (workspace_id, vendor_session_id, turn_id)
SELECT u.workspace_id, NULLIF(s.vendor_session_id, ''), u.turn_id
FROM fork_uninherited_turns AS u
LEFT JOIN sessions AS s ON s.workspace_id = u.workspace_id;
DROP TABLE fork_uninherited_turns;
DROP TABLE fork_lineage;
ALTER TABLE fork_lineage_by_conversation RENAME TO fork_lineage;
ALTER TABLE fork_uninherited_turns_by_conversation RENAME TO fork_uninherited_turns;
`

// PendingFork is a fork the create recorded and the bring-up has not made.
type PendingFork struct {
	// Workspace is the fork.
	Workspace WorkspaceID
	// Parent is the workspace whose conversation it forks.
	Parent WorkspaceID
	// ParentVendorSessionID is the conversation an OWN fork copies (Parent is
	// Workspace), and empty for a fork of another workspace, which forks the
	// conversation that workspace runs.
	ParentVendorSessionID string
}

// Own reports whether FORK copies one of its workspace's own conversations.
func (fork PendingFork) Own() bool {
	return fork.ParentVendorSessionID != ""
}

// validPendingFork refuses a pending fork whose parts disagree: an own fork
// names its workspace as its parent and the conversation it copies, and a fork
// of another workspace names a different parent and no conversation.
func validPendingFork(fork PendingFork) error {
	switch {
	case fork.Workspace == "" || fork.Parent == "":
		return errors.New("wsm: a pending fork names a workspace and a parent")
	case fork.Own() && fork.Workspace != fork.Parent:
		return errors.New("wsm: a pending fork naming the conversation it copies is a fork of its own workspace")
	case !fork.Own() && fork.Workspace == fork.Parent:
		return errors.New("wsm: a pending fork of its own workspace names the conversation it copies")
	}
	return nil
}

// insertPendingFork writes FORK inside the caller's transaction.
func insertPendingFork(ctx context.Context, tx *sql.Tx, fork PendingFork) error {
	var parentVendor any
	if fork.Own() {
		parentVendor = fork.ParentVendorSessionID
	}
	_, err := tx.ExecContext(ctx,
		`INSERT INTO pending_forks (workspace_id, parent_workspace_id, parent_vendor_session_id) VALUES (?, ?, ?)`,
		fork.Workspace, fork.Parent, parentVendor)
	return err
}

// ForkLineage is one made fork: where its inherited past is read from.
type ForkLineage struct {
	// Workspace is the fork.
	Workspace WorkspaceID
	// Parent is the workspace it was forked from.
	Parent WorkspaceID
	// ParentVendorSessionID is the conversation an OWN fork copied, which the
	// walk continues through; empty for a fork of another workspace, whose
	// walk continues through the conversation that workspace runs. Recorded
	// by ResolveFork from the pending fork, never set by its caller.
	ParentVendorSessionID string
	// ParentBook is the agent the parent's stored history is filed under.
	ParentBook string
	// ThroughMs is the inclusive upper bound, epoch ms, on the places of the
	// parent book's inherited rows.
	ThroughMs int64
	// Uninherited are the parent's turns open at the fork: their rows are not
	// the fork's, wherever they are placed.
	Uninherited []TurnID
}

// PutPendingFork records that a workspace is to be forked from its parent.
func (s *store) PutPendingFork(ctx context.Context, fork PendingFork) error {
	const op = "daemon.wsm.put_pending_fork"
	fields := dlog.Context{"workspace": string(fork.Workspace), "parent": string(fork.Parent), "parent_vendor_session_id": fork.ParentVendorSessionID}
	if err := validPendingFork(fork); err != nil {
		s.log.Error(op, "refused a pending fork whose workspace, parent and conversation disagree", withError(fields, err))
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		return insertPendingFork(ctx, tx, fork)
	})
}

// PendingFork answers the fork a workspace still owes, and whether it owes one.
func (s *store) PendingFork(ctx context.Context, id WorkspaceID) (PendingFork, bool, error) {
	var (
		out   PendingFork
		found bool
	)
	err := s.read(ctx, "daemon.wsm.pending_fork", dlog.Context{"workspace": string(id)}, func(ctx context.Context) error {
		var (
			parent       string
			parentVendor sql.NullString
		)
		err := s.q(ctx).QueryRowContext(ctx,
			`SELECT parent_workspace_id, parent_vendor_session_id FROM pending_forks WHERE workspace_id = ?`, id).Scan(&parent, &parentVendor)
		if errors.Is(err, sql.ErrNoRows) {
			return nil
		}
		if err != nil {
			return err
		}
		out, found = PendingFork{Workspace: id, Parent: WorkspaceID(parent), ParentVendorSessionID: parentVendor.String}, true
		return nil
	})
	return out, found, err
}

// ResolveFork records a made fork in ONE transaction: the session's vendor id
// becomes the fork's, the lineage is written, and the pending fork is retired.
// All or nothing: a fork recorded as made without its vendor id would resume
// nothing, and one whose id is recorded without its lineage would draw no past.
func (s *store) ResolveFork(ctx context.Context, vendorSessionID string, lineage ForkLineage) error {
	const op = "daemon.wsm.resolve_fork"
	fields := dlog.Context{
		"workspace": string(lineage.Workspace), "parent": string(lineage.Parent),
		"vendor_session_id": vendorSessionID, "parent_book": lineage.ParentBook,
		"through_ms": lineage.ThroughMs, "uninherited_turns": len(lineage.Uninherited),
	}
	if vendorSessionID == "" || lineage.Workspace == "" || lineage.Parent == "" || lineage.ParentBook == "" || lineage.ThroughMs <= 0 {
		err := errors.New("wsm: a made fork names its vendor session, workspace, parent, parent book and a positive bound")
		s.log.Error(op, "refused to record a fork missing part of its lineage", withError(fields, err))
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		// THE PENDING FORK NAMES THE CONVERSATION AN OWN FORK COPIED, which
		// its lineage records so the walk continues through it.
		var copied sql.NullString
		err := tx.QueryRowContext(ctx, `SELECT parent_vendor_session_id FROM pending_forks WHERE workspace_id = ? AND parent_workspace_id = ?`,
			lineage.Workspace, lineage.Parent).Scan(&copied)
		if errors.Is(err, sql.ErrNoRows) {
			return fmt.Errorf("wsm: workspace %q owes no fork from %q", lineage.Workspace, lineage.Parent)
		}
		if err != nil {
			return err
		}
		if _, err := tx.ExecContext(ctx, `DELETE FROM pending_forks WHERE workspace_id = ? AND parent_workspace_id = ?`,
			lineage.Workspace, lineage.Parent); err != nil {
			return err
		}
		res, err := tx.ExecContext(ctx, `UPDATE sessions SET vendor_session_id = ? WHERE workspace_id = ?`,
			vendorSessionID, lineage.Workspace)
		if err != nil {
			return err
		}
		if n, err := res.RowsAffected(); err != nil {
			return err
		} else if n != 1 {
			return fmt.Errorf("wsm: workspace %q has no session row to record its fork on", lineage.Workspace)
		}
		if _, err := tx.ExecContext(ctx,
			`INSERT INTO fork_lineage (workspace_id, vendor_session_id, parent_workspace_id, parent_vendor_session_id, parent_book, through_ms) VALUES (?, ?, ?, ?, ?, ?)`,
			lineage.Workspace, vendorSessionID, lineage.Parent, copied, lineage.ParentBook, lineage.ThroughMs); err != nil {
			return err
		}
		for _, turn := range lineage.Uninherited {
			if _, err := tx.ExecContext(ctx,
				`INSERT INTO fork_uninherited_turns (workspace_id, vendor_session_id, turn_id) VALUES (?, ?, ?) ON CONFLICT DO NOTHING`,
				lineage.Workspace, vendorSessionID, turn); err != nil {
				return err
			}
		}
		return nil
	})
}

// maxForkDepth bounds a lineage walk. A chain longer than this is a cycle in
// the records, which no create can write (a fork's parent exists before it).
const maxForkDepth = 256

// Lineage answers where a workspace's inherited past is read from, nearest
// first: the lineage of the conversation its session is bound to, then the
// lineage of the conversation that one was forked from, and so on to the first
// conversation that was not forked. Empty for a workspace bound to no forked
// conversation, and for one bound to none at all.
//
// THE WALK IS BY CONVERSATION. An own fork continues through the conversation
// it copied (ParentVendorSessionID); a fork of another workspace through the
// conversation that workspace is bound to. An own fork that records no copied
// conversation was made by a build that forgot the lineage behind it at the
// bind, so nothing lies past it.
func (s *store) Lineage(ctx context.Context, id WorkspaceID) ([]ForkLineage, error) {
	var out []ForkLineage
	err := s.read(ctx, "daemon.wsm.fork_lineage", dlog.Context{"workspace": string(id)}, func(ctx context.Context) error {
		type key struct {
			ws   WorkspaceID
			conv string
		}
		conv, err := s.boundConversation(ctx, id)
		if err != nil || conv == "" {
			return err
		}
		seen := map[key]bool{}
		for at := (key{ws: id, conv: conv}); ; {
			if seen[at] || len(out) == maxForkDepth {
				return fmt.Errorf("wsm: the lineage of %q loops back to %q (conversation %q)", id, at.ws, at.conv)
			}
			seen[at] = true
			var (
				link   ForkLineage
				parent string
				copied sql.NullString
			)
			err := s.q(ctx).QueryRowContext(ctx,
				`SELECT parent_workspace_id, parent_vendor_session_id, parent_book, through_ms FROM fork_lineage WHERE workspace_id = ? AND vendor_session_id = ?`, at.ws, at.conv).
				Scan(&parent, &copied, &link.ParentBook, &link.ThroughMs)
			if errors.Is(err, sql.ErrNoRows) {
				return nil
			}
			if err != nil {
				return err
			}
			link.Workspace, link.Parent, link.ParentVendorSessionID = at.ws, WorkspaceID(parent), copied.String
			if link.Uninherited, err = s.uninheritedTurns(ctx, at.ws, at.conv); err != nil {
				return err
			}
			out = append(out, link)
			next := key{ws: link.Parent, conv: link.ParentVendorSessionID}
			switch {
			case next.conv != "":
			case link.Parent == at.ws:
				return nil
			default:
				if next.conv, err = s.boundConversation(ctx, link.Parent); err != nil || next.conv == "" {
					return err
				}
			}
			at = next
		}
	})
	if err != nil {
		return nil, err
	}
	return out, nil
}

// boundConversation answers the conversation workspace ID's session is bound
// to, empty when it has no session or is bound to none. Called inside a read.
func (s *store) boundConversation(ctx context.Context, id WorkspaceID) (string, error) {
	var conv sql.NullString
	err := s.q(ctx).QueryRowContext(ctx, `SELECT vendor_session_id FROM sessions WHERE workspace_id = ?`, id).Scan(&conv)
	if errors.Is(err, sql.ErrNoRows) {
		return "", nil
	}
	return conv.String, err
}

// uninheritedTurns reads one fork's uninherited turns: workspace ID's
// conversation CONV's. Called inside a read.
func (s *store) uninheritedTurns(ctx context.Context, id WorkspaceID, conv string) ([]TurnID, error) {
	rows, err := s.q(ctx).QueryContext(ctx,
		`SELECT turn_id FROM fork_uninherited_turns WHERE workspace_id = ? AND vendor_session_id = ? ORDER BY turn_id`, id, conv)
	if err != nil {
		return nil, err
	}
	defer rows.Close()
	var out []TurnID
	for rows.Next() {
		var turn string
		if err := rows.Scan(&turn); err != nil {
			return nil, err
		}
		out = append(out, TurnID(turn))
	}
	return out, rows.Err()
}
