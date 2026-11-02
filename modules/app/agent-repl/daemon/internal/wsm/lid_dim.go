package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"math"

	"agent-repld/internal/dlog"
)

// persistentWifiLidDimDDL is the layout-99 addition: the built-in display's
// brightness the moment the lid closed with persistent wifi mode on, which
// opening the lid or turning the mode off restores (agentrepl.v1
// PersistentWifiState.lid_dim). A SINGLETON like feed_text_scale, and its ROW
// IS THE DIM: present means a lid-close dim is in force, absent means the
// display is at the user's own brightness. Durable because a daemon that
// restarts, or hands over, while the lid is closed and dimmed must still know
// the level to restore. Kept apart for the reason feedTextScaleDDL is: a fresh
// file and the 98 -> 99 migration write the one text.
const persistentWifiLidDimDDL = `
CREATE TABLE persistent_wifi_lid_dim (
  id            INTEGER PRIMARY KEY CHECK (id = 1),
  restore_level REAL NOT NULL
);
`

// PutLidDimLevel keeps level as the pre-close brightness, replacing any kept
// one. A level outside 0..1 is a bug in the caller (the brightness tool's
// reading is range-checked before it is kept), so it is refused loudly.
func (s *store) PutLidDimLevel(ctx context.Context, level float64) error {
	const op = "daemon.wsm.put_lid_dim_level"
	fields := dlog.Context{"restore_level": level}
	if err := validLidDimLevel(level); err != nil {
		s.log.Error(op, "refused a pre-close brightness outside 0..1", withError(fields, err))
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx,
			`INSERT INTO persistent_wifi_lid_dim (id, restore_level) VALUES (1, ?)
			 ON CONFLICT(id) DO UPDATE SET restore_level = excluded.restore_level`,
			level)
		return err
	})
}

// LidDimLevel answers the kept pre-close brightness, and false when no
// lid-close dim is in force (no row). A stored level outside 0..1 is a
// corrupt record, surfaced as a DecodeError.
func (s *store) LidDimLevel(ctx context.Context) (float64, bool, error) {
	var level float64
	var ok bool
	err := s.read(ctx, "daemon.wsm.lid_dim_level", dlog.Context{}, func(ctx context.Context) error {
		var stored float64
		err := s.q(ctx).QueryRowContext(ctx, `SELECT restore_level FROM persistent_wifi_lid_dim WHERE id = 1`).Scan(&stored)
		if errors.Is(err, sql.ErrNoRows) {
			return nil
		}
		if err != nil {
			return err
		}
		if err := validLidDimLevel(stored); err != nil {
			return &DecodeError{Table: "persistent_wifi_lid_dim", Row: "1", Field: "restore_level", Err: err}
		}
		level, ok = stored, true
		return nil
	})
	return level, ok, err
}

// ClearLidDimLevel forgets the kept pre-close brightness: the dim is over.
func (s *store) ClearLidDimLevel(ctx context.Context) error {
	return s.write(ctx, "daemon.wsm.clear_lid_dim_level", dlog.Context{}, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx, `DELETE FROM persistent_wifi_lid_dim WHERE id = 1`)
		return err
	})
}

// validLidDimLevel is the one range check both directions share.
func validLidDimLevel(level float64) error {
	if math.IsNaN(level) || level < 0 || level > 1 {
		return fmt.Errorf("wsm: a pre-close brightness is between 0 and 1, got %v", level)
	}
	return nil
}
