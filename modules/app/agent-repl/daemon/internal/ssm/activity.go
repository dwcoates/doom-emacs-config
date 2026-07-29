package ssm

import (
	"database/sql"
	"fmt"
)

// LastActivityMs returns the wall-clock millis of the newest row on a
// workspace's state log, with ok=false when the workspace has no rows at all.
//
// THE LOG IS ALREADY THE ACTIVITY RECORD, which is why this needs no new
// bookkeeping and no periodic stamp. Every row is appended by something that
// genuinely happened to the workspace: a turn starting or ending, a bring-up, a
// permission opening, a task moving. Nothing appends on a timer, and
// applyWiredLocked and its siblings append only when an axis actually MOVES, so
// the newest row's timestamp is exactly "the last time anything happened here".
//
// UNKNOWN IS NOT IDLE. ok=false means the workspace has no state history
// whatsoever, which a caller deciding whether to tear something down must treat
// as "not idle" rather than as "idle since the epoch". Reading an absent answer
// as a very old one is how a sweeper reaps the workspace it knows least about.
func (m *Manager) LastActivityMs(workspace string) (int64, bool, error) {
	if workspace == "" {
		return 0, false, fmt.Errorf("ssm: LastActivityMs got an empty workspace")
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	var at sql.NullInt64
	if err := m.db.QueryRow(
		`SELECT MAX(at) FROM workspace_state WHERE workspace = ?`, workspace).Scan(&at); err != nil {
		return 0, false, fmt.Errorf("ssm: last-activity read for workspace %q: %w", workspace, err)
	}
	if !at.Valid {
		return 0, false, nil
	}
	return at.Int64, true, nil
}
