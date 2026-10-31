package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"strings"

	"agent-repld/internal/dlog"
)

// remoteControlDDL is the layout-91 addition: each workspace's remote-control
// mode (agentrepl.v1.UpdateRemoteControlMode), and the claude.ai session its
// last shim's bridge answered (shim.v1.SetSessionRemoteControlSuccess
// .bridge_session_id), so the next shim re-binds that session rather than
// listing a new one. IT IS ADDITIVE: a new table the previous build never
// names.
//
// THE MODE IS RECORDED ONLY ONCE IT IS SET (owner ruling, 2026-10-31). A NULL
// `enabled` is a mode nobody set, which reads as the DEFAULT: on for a
// repository's main worktree, off for every other workspace
// (RemoteControl.On). A row can carry a bridge session with no mode, because a
// main worktree's default-on bridge answers one too.
const remoteControlDDL = `
CREATE TABLE workspace_remote_control (
  workspace_id      TEXT PRIMARY KEY REFERENCES workspaces(id) ON DELETE CASCADE,
  enabled           INTEGER,
  bridge_session_id TEXT
);
`

// RemoteControl is one workspace's remote-control standing.
type RemoteControl struct {
	// On is the EFFECTIVE mode: the one recorded, else the default (on for a
	// repository's main worktree, IsMainWorktree; off otherwise).
	On bool
	// Recorded reports whether a mode was ever set explicitly, which is what
	// makes On the recorded mode rather than the default.
	Recorded bool
	// BridgeSessionID is the claude.ai session the workspace's last bridge
	// answered, empty when none ever did.
	BridgeSessionID string
}

// RemoteControlChange is what UpdateRemoteControlMode asks of the mode.
type RemoteControlChange int

const (
	// RemoteControlOn turns the mode on.
	RemoteControlOn RemoteControlChange = iota + 1
	// RemoteControlOff turns the mode off.
	RemoteControlOff
	// RemoteControlToggle turns the mode over from the effective mode read in
	// the same transaction that records the new one.
	RemoteControlToggle
)

// String spells the change for the record.
func (c RemoteControlChange) String() string {
	switch c {
	case RemoteControlOn:
		return "on"
	case RemoteControlOff:
		return "off"
	case RemoteControlToggle:
		return "toggle"
	}
	return fmt.Sprintf("RemoteControlChange(%d)", int(c))
}

// RemoteControl answers workspace ID's remote-control standing. A workspace
// the registry does not hold is ErrNotFound.
func (s *store) RemoteControl(ctx context.Context, id WorkspaceID) (RemoteControl, error) {
	var out RemoteControl
	err := s.read(ctx, "daemon.wsm.remote_control", dlog.Context{"workspace": string(id)}, func(ctx context.Context) error {
		var err error
		out, err = readRemoteControl(ctx, s.q(ctx), id)
		return err
	})
	return out, err
}

// SetRemoteControlMode records the mode CHANGE leaves on workspace ID and
// answers it. A toggle reads the effective mode in the SAME transaction that
// records the new one, so two toggles racing each turn the mode over once and
// never both read the same mode. The bridge session is kept: a later on
// re-binds the claude.ai session the last bridge answered.
func (s *store) SetRemoteControlMode(ctx context.Context, id WorkspaceID, change RemoteControlChange) (bool, error) {
	const op = "daemon.wsm.set_remote_control_mode"
	fields := dlog.Context{"workspace": string(id), "change": change.String()}
	if change < RemoteControlOn || change > RemoteControlToggle {
		err := fmt.Errorf("wsm: %s is no remote-control change (workspace %s)", change, id)
		s.log.Error(op, "refused a remote-control change that names no mode", withError(fields, err))
		return false, err
	}
	var on bool
	err := s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		current, err := readRemoteControl(ctx, tx, id)
		if err != nil {
			return err
		}
		switch change {
		case RemoteControlOn:
			on = true
		case RemoteControlOff:
			on = false
		case RemoteControlToggle:
			on = !current.On
		}
		_, err = tx.ExecContext(ctx,
			`INSERT INTO workspace_remote_control (workspace_id, enabled) VALUES (?, ?)
			 ON CONFLICT(workspace_id) DO UPDATE SET enabled = excluded.enabled`, id, on)
		return err
	})
	return on, err
}

// SetRemoteControlBridgeSession records BRIDGE as the claude.ai session
// workspace ID's bridge answered, keeping its mode. A blank session is a bug
// in the caller, refused loudly.
func (s *store) SetRemoteControlBridgeSession(ctx context.Context, id WorkspaceID, bridge string) error {
	const op = "daemon.wsm.set_remote_control_bridge_session"
	fields := dlog.Context{"workspace": string(id), "bridge_session_id": bridge}
	if strings.TrimSpace(bridge) == "" {
		err := fmt.Errorf("wsm: a remote-control bridge session names its session (workspace %s)", id)
		s.log.Error(op, "refused a blank bridge session", withError(fields, err))
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx,
			`INSERT INTO workspace_remote_control (workspace_id, bridge_session_id) VALUES (?, ?)
			 ON CONFLICT(workspace_id) DO UPDATE SET bridge_session_id = excluded.bridge_session_id`, id, bridge)
		return err
	})
}

// remoteControlQuerier is what a remote-control read runs on: the store's own
// handle, or a write's transaction.
type remoteControlQuerier interface {
	QueryRowContext(ctx context.Context, query string, args ...any) *sql.Row
}

// readRemoteControl reads workspace ID's standing on Q, resolving the default
// from the workspace's own directory and its repository's: the ONE spelling
// of the main-worktree rule (IsMainWorktree) decides it.
func readRemoteControl(ctx context.Context, q remoteControlQuerier, id WorkspaceID) (RemoteControl, error) {
	var (
		dir, repoDir string
		enabled      sql.NullBool
		bridge       sql.NullString
	)
	err := q.QueryRowContext(ctx, `SELECT w.dir, r.dir, rc.enabled, rc.bridge_session_id
		FROM workspaces AS w
		JOIN repositories AS r ON r.id = w.repo_id
		LEFT JOIN workspace_remote_control AS rc ON rc.workspace_id = w.id
		WHERE w.id = ?`, id).Scan(&dir, &repoDir, &enabled, &bridge)
	if errors.Is(err, sql.ErrNoRows) {
		return RemoteControl{}, fmt.Errorf("wsm: workspace %s: %w", id, ErrNotFound)
	}
	if err != nil {
		return RemoteControl{}, err
	}
	out := RemoteControl{On: IsMainWorktree(dir, repoDir), BridgeSessionID: bridge.String}
	if enabled.Valid {
		out.On, out.Recorded = enabled.Bool, true
	}
	return out, nil
}
