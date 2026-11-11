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
	// ParkedClockAt is when the parked conversation's cache was last written;
	// nil when it never was or none is parked.
	ParkedClockAt *time.Time
	// ParkedCold reports the parked conversation's cache known lapsed.
	ParkedCold bool
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

// parkedClockDDL is the layout-116 addition: each conversation of a
// queue-bypass workspace keeps its OWN cache clock (owner request,
// 2026-11-10: the two are kept warm in tandem). The parked row carries the
// parked conversation's clock and whether its cache is known cold, and the
// workspace row stamps when its running conversation was bound, so turns
// from before the binding (the other conversation's) time nothing.
const parkedClockDDL = `
ALTER TABLE workspace_parked_conversations ADD COLUMN clock_at INTEGER;
ALTER TABLE workspace_parked_conversations ADD COLUMN cold INTEGER NOT NULL DEFAULT 0;
ALTER TABLE workspaces ADD COLUMN conversation_bound_at INTEGER;
`

// SwapParkedConversation parks STEPPEDOUT, the conversation workspace ID is
// stepping out of, and swaps the two conversations' cache clocks, in ONE
// transaction, AT being the switch: the parked row takes STEPPEDOUT with the
// clock and cold mark the workspace row held for it, and the workspace row
// takes the clock and cold mark the parked row held for the conversation now
// bound, stamped bound at AT. A conversation bound with no clock of its own
// (the bypass conversation's first binding, a fork whose new system prompt
// reads no cache) is timed from its first turn. RESUMESPARKED says whether
// the conversation bound is the parked one: false for a fork that REPLACES a
// parked bypass conversation, whose clock then goes with the binding it
// discards rather than to the fork. A blank STEPPEDOUT is a bug in the
// caller, refused loudly: a switch always parks the conversation it stepped
// out of.
func (s *store) SwapParkedConversation(ctx context.Context, id WorkspaceID, steppedOut string, at time.Time, resumesParked bool) error {
	const op = "daemon.wsm.swap_parked_conversation"
	fields := dlog.Context{"workspace": string(id), "vendor_session_id": steppedOut, "at": at, "resumes_parked": resumesParked}
	if strings.TrimSpace(steppedOut) == "" {
		err := fmt.Errorf("wsm: a parked conversation names its conversation (workspace %s)", id)
		s.log.Error(op, "refused a blank parked conversation", withError(fields, err))
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		var (
			ended, keepalive, bound, coldTurn sql.NullInt64
			parkedClock                       sql.NullInt64
			parkedCold                        sql.NullBool
		)
		if err := tx.QueryRowContext(ctx, `SELECT
			(SELECT MAX(turns.closed_at) FROM turns WHERE turns.workspace_id = w.id),
			w.last_keepalive_at, w.conversation_bound_at, w.cache_cold_turn_at, p.clock_at, p.cold
			FROM workspaces AS w
			LEFT JOIN workspace_parked_conversations AS p ON p.workspace_id = w.id
			WHERE w.id = ?`, id).Scan(&ended, &keepalive, &bound, &coldTurn, &parkedClock, &parkedCold); err != nil {
			if errors.Is(err, sql.ErrNoRows) {
				return fmt.Errorf("wsm: workspace %s: %w", id, ErrNotFound)
			}
			return err
		}
		out := Workspace{LastTurnEndedAt: optTime(ended), LastKeepaliveAt: optTime(keepalive), ConversationBoundAt: optTime(bound), CacheColdTurnAt: optTime(coldTurn)}
		var outClock any
		if clock := out.CacheClock(); clock != nil {
			outClock = nanos(*clock)
		}
		if _, err := tx.ExecContext(ctx,
			`INSERT INTO workspace_parked_conversations (workspace_id, vendor_session_id, clock_at, cold) VALUES (?, ?, ?, ?)
			 ON CONFLICT(workspace_id) DO UPDATE SET vendor_session_id = excluded.vendor_session_id, clock_at = excluded.clock_at, cold = excluded.cold`,
			id, steppedOut, outClock, out.CacheKnownCold()); err != nil {
			return err
		}
		// THE BOUND-IN CONVERSATION'S CLOCK. Cold, it is timed from nothing
		// and marked cold at the turn clock as it stands, so no beat goes out
		// until a turn of its own closes.
		var inClock, inColdTurn any
		switch {
		case !resumesParked:
		case parkedCold.Valid && parkedCold.Bool:
			if ended.Valid {
				inColdTurn = ended.Int64
			}
		case parkedClock.Valid:
			inClock = parkedClock.Int64
		}
		_, err := tx.ExecContext(ctx,
			`UPDATE workspaces SET last_keepalive_at = ?, cache_cold_turn_at = ?, conversation_bound_at = ? WHERE id = ?`,
			inClock, inColdTurn, nanos(at), id)
		return err
	})
}

// SetParkedKeepalive records AT as when workspace ID's parked conversation's
// keep-alive was answered: its clock moves, and its cache is warm.
func (s *store) SetParkedKeepalive(ctx context.Context, id WorkspaceID, at time.Time) error {
	return s.setParkedClock(ctx, "daemon.wsm.set_parked_keepalive", id, `clock_at = ?, cold = 0`, nanos(at))
}

// MarkParkedCold records that workspace ID's parked conversation's cache has
// lapsed: it is beaten no more until it runs again.
func (s *store) MarkParkedCold(ctx context.Context, id WorkspaceID) error {
	return s.setParkedClock(ctx, "daemon.wsm.mark_parked_cold", id, `cold = 1`)
}

// setParkedClock writes SET on workspace ID's parked row, which must stand:
// a parked conversation's clock moves only for a conversation that is parked.
func (s *store) setParkedClock(ctx context.Context, op string, id WorkspaceID, set string, args ...any) error {
	fields := dlog.Context{"workspace": string(id)}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `UPDATE workspace_parked_conversations SET `+set+` WHERE workspace_id = ?`, append(args, id)...)
		if err != nil {
			return err
		}
		if n, err := res.RowsAffected(); err != nil {
			return err
		} else if n != 1 {
			return fmt.Errorf("wsm: workspace %s parks no conversation", id)
		}
		return nil
	})
}

// readQueueBypass reads workspace ID's standing on Q.
func readQueueBypass(ctx context.Context, q remoteControlQuerier, id WorkspaceID) (QueueBypass, error) {
	var (
		repo   string
		holder sql.NullString
		parked sql.NullString
		clock  sql.NullInt64
		cold   sql.NullBool
	)
	err := q.QueryRowContext(ctx, `SELECT w.repo_id, h.workspace_id, p.vendor_session_id, p.clock_at, p.cold
		FROM workspaces AS w
		LEFT JOIN queue_bypass_holders AS h ON h.repo_id = w.repo_id
		LEFT JOIN workspace_parked_conversations AS p ON p.workspace_id = w.id
		WHERE w.id = ?`, id).Scan(&repo, &holder, &parked, &clock, &cold)
	if errors.Is(err, sql.ErrNoRows) {
		return QueueBypass{}, fmt.Errorf("wsm: workspace %s: %w", id, ErrNotFound)
	}
	if err != nil {
		return QueueBypass{}, err
	}
	out := QueueBypass{RepoID: RepoID(repo), ParkedVendorSessionID: parked.String, ParkedClockAt: optTime(clock), ParkedCold: cold.Valid && cold.Bool}
	if holder.Valid {
		if WorkspaceID(holder.String) == id {
			out.Holds = true
		} else {
			out.HeldBy = WorkspaceID(holder.String)
		}
	}
	return out, nil
}
