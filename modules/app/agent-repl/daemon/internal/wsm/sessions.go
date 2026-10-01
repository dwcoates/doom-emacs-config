package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"time"

	"claude-repld/internal/dlog"
)

// scanSession decodes one session row all-or-nothing. The terminal is a whole:
// a row with a terminal instant but no kind (or the reverse) is corrupt, and a
// corrupt row fails the read rather than presenting a live session that died.
func scanSession(row interface{ Scan(...any) error }) (Session, error) {
	var (
		s          Session
		started    int64
		engagement int64
		pid        sql.NullInt64
		kind       sql.NullString
		detail     sql.NullString
		at         sql.NullInt64
	)
	if err := row.Scan(&s.Workspace, &s.HostSessionID, &s.VendorSessionID, &s.ConfigDir, &s.SelectedConfigDir, &s.Model, &s.PermissionMode, &started, &engagement, &pid, &kind, &detail, &at); err != nil {
		return Session{}, err
	}
	s.StartedAt = fromNanos(started)
	s.LastEngagementAt = fromNanos(engagement)
	if pid.Valid {
		if pid.Int64 <= 0 {
			return Session{}, &DecodeError{Table: "sessions", Row: string(s.Workspace), Field: "shim_pid", Err: errors.New("a recorded shim pid is positive")}
		}
		recorded := int(pid.Int64)
		s.ShimPID = &recorded
	}
	switch {
	case !kind.Valid && !at.Valid && !detail.Valid:
	case kind.Valid && at.Valid && detail.Valid:
		s.Terminal = &SessionTerminal{Kind: kind.String, Detail: detail.String, At: fromNanos(at.Int64)}
	default:
		return Session{}, &DecodeError{Table: "sessions", Row: string(s.Workspace), Field: "terminal", Err: errors.New("a session terminal is stored whole or not at all")}
	}
	return s, nil
}

// terminalDeleted is the terminal kind that refuses resurrection.
const terminalDeleted = "deleted"

// PutSession records a workspace's session binding and spawn identity. A
// workspace whose session was DELETED refuses a new binding: resurrection is
// unrepresentable, not merely discouraged.
//
// A ROW WITH NO HOST SESSION IDENTITY IS REFUSED AT THE WRITE. Every session
// row is composed into the host view, and the view is WITHHELD when the row
// names no identity — so a row written without one is not a transient gap but
// a permanent one, re-failing every compose for as long as the row stands.
// This is the single chokepoint every writer passes through, so refusing here
// is what makes the missing identity unrepresentable rather than merely
// unlikely.
func (s *store) PutSession(ctx context.Context, sess Session) error {
	const op = "daemon.wsm.put_session"
	if sess.HostSessionID == "" {
		return fmt.Errorf("wsm: workspace %s: %w", sess.Workspace, ErrSessionIdentityMissing)
	}
	fields := dlog.Context{"workspace": string(sess.Workspace), "vendor_session": sess.VendorSessionID, "config_dir": sess.ConfigDir}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		var kind sql.NullString
		err := tx.QueryRowContext(ctx, `SELECT terminal_kind FROM sessions WHERE workspace_id = ?`, sess.Workspace).Scan(&kind)
		if err != nil && !errors.Is(err, sql.ErrNoRows) {
			return err
		}
		if kind.Valid && kind.String == terminalDeleted {
			return fmt.Errorf("wsm: workspace %s: %w", sess.Workspace, ErrSessionDeleted)
		}
		var (
			tKind, tDetail any
			tAt            any
			pid            any
		)
		if sess.Terminal != nil {
			tKind, tDetail, tAt = sess.Terminal.Kind, sess.Terminal.Detail, nanos(sess.Terminal.At)
		}
		if sess.ShimPID != nil {
			if *sess.ShimPID <= 0 {
				return fmt.Errorf("wsm: workspace %s: a recorded shim pid is positive, got %d", sess.Workspace, *sess.ShimPID)
			}
			pid = int64(*sess.ShimPID)
		}
		_, err = tx.ExecContext(ctx,
			`INSERT INTO sessions (workspace_id, host_session_id, vendor_session_id, config_dir, selected_config_dir, model, permission_mode, started_at, last_engagement_at, shim_pid, terminal_kind, terminal_detail, terminal_at)
			 VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
			 ON CONFLICT(workspace_id) DO UPDATE SET
			   host_session_id = excluded.host_session_id,
			   vendor_session_id = excluded.vendor_session_id, config_dir = excluded.config_dir,
			   selected_config_dir = excluded.selected_config_dir, model = excluded.model,
			   permission_mode = excluded.permission_mode, started_at = excluded.started_at,
			   last_engagement_at = excluded.last_engagement_at, shim_pid = excluded.shim_pid,
			   terminal_kind = excluded.terminal_kind,
			   terminal_detail = excluded.terminal_detail, terminal_at = excluded.terminal_at`,
			sess.Workspace, sess.HostSessionID, sess.VendorSessionID, sess.ConfigDir, sess.SelectedConfigDir, sess.Model, sess.PermissionMode,
			nanos(sess.StartedAt), nanos(sess.LastEngagementAt), pid, tKind, tDetail, tAt)
		return err
	})
}

// Session loads one workspace's session; the bool reports existence.
func (s *store) Session(ctx context.Context, id WorkspaceID) (Session, bool, error) {
	var (
		out   Session
		found bool
	)
	err := s.read(ctx, "daemon.wsm.session", dlog.Context{"workspace": string(id)}, func(ctx context.Context) error {
		sess, err := scanSession(s.db().QueryRowContext(ctx,
			`SELECT workspace_id, host_session_id, vendor_session_id, config_dir, selected_config_dir, model, permission_mode, started_at, last_engagement_at, shim_pid, terminal_kind, terminal_detail, terminal_at
			 FROM sessions WHERE workspace_id = ?`, id))
		if errors.Is(err, sql.ErrNoRows) {
			out, found = Session{}, false
			return nil
		}
		if err != nil {
			return err
		}
		out, found = sess, true
		return nil
	})
	if err != nil {
		return Session{}, false, err
	}
	return out, found, nil
}

// SetSessionTerminal records a session's death with its cause. A session
// already deleted takes no further terminal — its cause of death is final.
func (s *store) SetSessionTerminal(ctx context.Context, id WorkspaceID, t SessionTerminal) error {
	const op = "daemon.wsm.set_session_terminal"
	fields := dlog.Context{"workspace": string(id), "terminal_kind": t.Kind, "terminal_at": t.At}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		var kind sql.NullString
		err := tx.QueryRowContext(ctx, `SELECT terminal_kind FROM sessions WHERE workspace_id = ?`, id).Scan(&kind)
		if errors.Is(err, sql.ErrNoRows) {
			return fmt.Errorf("wsm: session for workspace %s: %w", id, ErrNotFound)
		}
		if err != nil {
			return err
		}
		if kind.Valid && kind.String == terminalDeleted {
			return fmt.Errorf("wsm: workspace %s: %w", id, ErrSessionDeleted)
		}
		res, err := tx.ExecContext(ctx, `UPDATE sessions SET terminal_kind = ?, terminal_detail = ?, terminal_at = ? WHERE workspace_id = ?`,
			t.Kind, t.Detail, nanos(t.At), id)
		if err != nil {
			return err
		}
		return requireOneRow(res, fmt.Sprintf("wsm: session for workspace %s", id))
	})
}

// ClearSessionTerminal retires a workspace's terminal session record, and it
// is the write behind THE LIVE-SHIM INVARIANT: a workspace whose shim is live
// in the fleet carries no terminal session record. Nothing else retired one —
// a killed session's cause of death stood in the row until some later
// PutSession happened to compose a row with no terminal — so a workspace whose
// shim came back up without re-recording its facts (a bring-up parked at a
// cold gate does exactly that) went on reading KILLED to every surface that
// composes off the record, and the roster receded a row whose shim was serving.
//
// It is stated as a POSTCONDITION, not as an edit: what it guarantees is that
// the workspace carries no terminal afterwards. A workspace with no session row
// at all already satisfies that, so it is not a refusal — there is no record to
// retire, and the caller asked for none to stand.
//
// A DELETED SESSION IS NOT RESURRECTED. Deletion is final everywhere else a
// terminal is written (PutSession, SetSessionTerminal), and retirement is no
// exception: the refusal is ErrSessionDeleted and the row keeps its cause.
func (s *store) ClearSessionTerminal(ctx context.Context, id WorkspaceID) error {
	const op = "daemon.wsm.clear_session_terminal"
	fields := dlog.Context{"workspace": string(id)}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		var kind sql.NullString
		err := tx.QueryRowContext(ctx, `SELECT terminal_kind FROM sessions WHERE workspace_id = ?`, id).Scan(&kind)
		if errors.Is(err, sql.ErrNoRows) {
			return nil
		}
		if err != nil {
			return err
		}
		if kind.Valid && kind.String == terminalDeleted {
			return fmt.Errorf("wsm: workspace %s: %w", id, ErrSessionDeleted)
		}
		res, err := tx.ExecContext(ctx, `UPDATE sessions SET terminal_kind = NULL, terminal_detail = NULL, terminal_at = NULL WHERE workspace_id = ?`, id)
		if err != nil {
			return err
		}
		return requireOneRow(res, fmt.Sprintf("wsm: session for workspace %s", id))
	})
}

// SetVendorSessionID records the vendor session id a later resume names, and
// answers the one it replaced. The vendor ROTATES the id (a /clear starts a new
// conversation under a new id), and the shim states the id in force on every
// rotation and on every re-announced start; a record left naming the start's
// id resumed nothing after a restart, and the session came up FRESH
// (2026-09-30). Recording the id the row already names is no change.
func (s *store) SetVendorSessionID(ctx context.Context, id WorkspaceID, vendorSessionID string) (string, error) {
	const op = "daemon.wsm.set_vendor_session_id"
	fields := dlog.Context{"workspace": string(id), "vendor_session": vendorSessionID}
	if vendorSessionID == "" {
		err := fmt.Errorf("wsm: workspace %s: a recorded vendor session id is never empty", id)
		s.log.Error(op, "refused an empty vendor session id", withError(fields, err))
		return "", err
	}
	var previous string
	err := s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		err := tx.QueryRowContext(ctx, `SELECT vendor_session_id FROM sessions WHERE workspace_id = ?`, id).Scan(&previous)
		if errors.Is(err, sql.ErrNoRows) {
			return fmt.Errorf("wsm: session for workspace %s: %w", id, ErrNotFound)
		}
		if err != nil {
			return err
		}
		res, err := tx.ExecContext(ctx, `UPDATE sessions SET vendor_session_id = ? WHERE workspace_id = ?`, vendorSessionID, id)
		if err != nil {
			return err
		}
		return requireOneRow(res, fmt.Sprintf("wsm: session for workspace %s", id))
	})
	if err != nil {
		return "", err
	}
	return previous, nil
}

// TouchEngagement records last engagement — the idle sweep's input.
func (s *store) TouchEngagement(ctx context.Context, id WorkspaceID, at time.Time) error {
	return s.write(ctx, "daemon.wsm.touch_engagement", dlog.Context{"workspace": string(id), "at": at}, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `UPDATE sessions SET last_engagement_at = ? WHERE workspace_id = ?`, nanos(at), id)
		if err != nil {
			return err
		}
		return requireOneRow(res, fmt.Sprintf("wsm: session for workspace %s", id))
	})
}

// SetShimPID records (or clears, with nil) the pid of the shim process serving
// a workspace's session. The stand-down INTENT MANIFEST names it per session
// and the incoming daemon reconciles it against the shim-held kernel lock, so
// a stale pid would make a dead session read as preserved: the pid is cleared
// at every stand-down rather than left behind.
func (s *store) SetShimPID(ctx context.Context, id WorkspaceID, pid *int) error {
	const op = "daemon.wsm.set_shim_pid"
	fields := dlog.Context{"workspace": string(id)}
	var stored any
	if pid != nil {
		if *pid <= 0 {
			err := fmt.Errorf("wsm: workspace %s: a recorded shim pid is positive, got %d", id, *pid)
			s.log.Error(op, "refused a non-positive shim pid", withError(fields, err))
			return err
		}
		fields["shim_pid"] = *pid
		stored = int64(*pid)
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `UPDATE sessions SET shim_pid = ? WHERE workspace_id = ?`, stored, id)
		if err != nil {
			return err
		}
		return requireOneRow(res, fmt.Sprintf("wsm: session for workspace %s", id))
	})
}
