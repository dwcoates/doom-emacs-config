package main

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"math"
	"net/url"
	"os"

	_ "modernc.org/sqlite"
)

// lidDimBusyMillis is how long a read or clear of the lid-dim row waits for the
// daemon's writer, the busy_timeout the daemon's own state database opens with
// (wsm's writingPragmas), since the row lives in that file.
const lidDimBusyMillis = 5000

// sqliteLidDims is the earlier build's kept lid dim, read from the row it
// left in the daemon's state database (the `persistent_wifi_lid_dim` table,
// wsm layout 99), which nothing writes any more. The service undoes a row it
// finds once at startup (persistentwifi.Controller.Refresh) and clears it.
//
// IT NEVER CREATES THE DATABASE OR THE TABLE: a state root with no database,
// or one whose layout predates the table, has no dim in force. Each call opens
// the file for its own use and closes it, so the service holds no handle on
// the daemon's database between startups.
type sqliteLidDims struct {
	path string
}

// open answers a handle on the existing database, or nil with no error when
// the file does not exist.
func (s sqliteLidDims) open() (*sql.DB, error) {
	if _, err := os.Stat(s.path); errors.Is(err, os.ErrNotExist) {
		return nil, nil
	} else if err != nil {
		return nil, fmt.Errorf("stat the state database %s: %w", s.path, err)
	}
	dsn := (&url.URL{Scheme: "file", Path: s.path}).String() + fmt.Sprintf("?mode=rw&_pragma=busy_timeout(%d)", lidDimBusyMillis)
	db, err := sql.Open("sqlite", dsn)
	if err != nil {
		return nil, fmt.Errorf("open the state database %s: %w", s.path, err)
	}
	return db, nil
}

// hasTable answers whether the lid-dim table exists in db.
func hasTable(ctx context.Context, db *sql.DB) (bool, error) {
	var name string
	err := db.QueryRowContext(ctx, `SELECT name FROM sqlite_master WHERE type = 'table' AND name = 'persistent_wifi_lid_dim'`).Scan(&name)
	if errors.Is(err, sql.ErrNoRows) {
		return false, nil
	}
	return err == nil, err
}

// LidDimLevel answers the kept pre-close brightness, and false when no dim is
// in force. A stored level outside 0 to 1 is a corrupt record and an error.
func (s sqliteLidDims) LidDimLevel(ctx context.Context) (float64, bool, error) {
	db, err := s.open()
	if err != nil || db == nil {
		return 0, false, err
	}
	defer func() { _ = db.Close() }()
	if ok, err := hasTable(ctx, db); err != nil || !ok {
		return 0, false, err
	}
	var level float64
	err = db.QueryRowContext(ctx, `SELECT restore_level FROM persistent_wifi_lid_dim WHERE id = 1`).Scan(&level)
	if errors.Is(err, sql.ErrNoRows) {
		return 0, false, nil
	}
	if err != nil {
		return 0, false, fmt.Errorf("read the kept lid dim: %w", err)
	}
	if math.IsNaN(level) || level < 0 || level > 1 {
		return 0, false, fmt.Errorf("the kept lid dim %v is outside 0 to 1", level)
	}
	return level, true, nil
}

// ClearLidDimLevel forgets the kept pre-close brightness.
func (s sqliteLidDims) ClearLidDimLevel(ctx context.Context) error {
	db, err := s.open()
	if err != nil || db == nil {
		return err
	}
	defer func() { _ = db.Close() }()
	if ok, err := hasTable(ctx, db); err != nil || !ok {
		return err
	}
	if _, err := db.ExecContext(ctx, `DELETE FROM persistent_wifi_lid_dim WHERE id = 1`); err != nil {
		return fmt.Errorf("clear the kept lid dim: %w", err)
	}
	return nil
}
