package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"time"

	"agent-repld/internal/dlog"
)

// A SINGLETON SWITCH is a daemon-wide operator switch kept as a one-row table
// whose ROW IS THE SWITCH: present means on, since the instant its column
// holds; absent means off, the state every daemon ran in before the table
// existed. The merge queue's rollout hold (rollouthold.go) and its racing
// switch (racing.go) are both one.
//
// Turning on a switch that is already on keeps the ORIGINAL start, so the
// instant always answers how long it has stood.

// singletonSwitch names one switch's table and the column holding its start.
// Both are constants of this package, never input.
type singletonSwitch struct {
	table  string
	column string
}

// set turns the switch on from at (on true), or off.
func (sw singletonSwitch) set(ctx context.Context, s *store, op string, on bool, at time.Time) error {
	return s.write(ctx, op, dlog.Context{"on": on}, func(ctx context.Context, tx *sql.Tx) error {
		if !on {
			_, err := tx.ExecContext(ctx, fmt.Sprintf(`DELETE FROM %s WHERE id = 1`, sw.table))
			return err
		}
		_, err := tx.ExecContext(ctx,
			fmt.Sprintf(`INSERT INTO %s (id, %s) VALUES (1, ?) ON CONFLICT(id) DO NOTHING`, sw.table, sw.column),
			nanos(at))
		return err
	})
}

// get reports whether the switch is on, and since when; an absent row is off
// and answered without an error.
func (sw singletonSwitch) get(ctx context.Context, s *store, op string) (bool, time.Time, error) {
	var on bool
	var since time.Time
	err := s.read(ctx, op, dlog.Context{}, func(ctx context.Context) error {
		var at int64
		err := s.db().QueryRowContext(ctx, fmt.Sprintf(`SELECT %s FROM %s WHERE id = 1`, sw.column, sw.table)).Scan(&at)
		if errors.Is(err, sql.ErrNoRows) {
			on, since = false, time.Time{}
			return nil
		}
		if err != nil {
			return err
		}
		on, since = true, fromNanos(at)
		return nil
	})
	return on, since, err
}
