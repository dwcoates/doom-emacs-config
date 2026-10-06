package harness

import (
	"database/sql"
	"path/filepath"

	// The daemon's own sqlite driver, so a test can corrupt one row of the
	// workspace state database the way a truncated write would.
	_ "modernc.org/sqlite"
)

// DBPath is the workspace state database inside this daemon's state root.
func (d *Daemon) DBPath() string { return filepath.Join(d.StateDir, "wsm.db") }

// WithDB opens the workspace state database directly and hands it to the body.
//
// IT IS FOR CORRUPTION ONLY. The daemon holds a writing handle while it runs,
// so a test that writes here must have stopped its daemon first; the point is
// to produce the half-written row no rpc can produce, and then to restart and
// watch the daemon refuse it.
func (d *Daemon) WithDB(body func(*sql.DB)) {
	d.t.Helper()
	db, err := sql.Open("sqlite", d.DBPath()+"?_pragma=synchronous(OFF)")
	if err != nil {
		d.t.Fatalf("harness: open %s: %v", d.DBPath(), err)
	}
	defer db.Close()
	body(db)
}

// CorruptRow overwrites one column of one row of the state database, which is
// how a test produces an undecodable row. The daemon must be stopped.
func (d *Daemon) CorruptRow(table, column, keyColumn string, key any, value any) {
	d.t.Helper()
	d.WithDB(func(db *sql.DB) {
		res, err := db.Exec("UPDATE "+table+" SET "+column+" = ? WHERE "+keyColumn+" = ?", value, key)
		if err != nil {
			d.t.Fatalf("harness: corrupt %s.%s: %v", table, column, err)
		}
		n, err := res.RowsAffected()
		if err != nil {
			d.t.Fatalf("harness: corrupt %s.%s: rows affected: %v", table, column, err)
		}
		if n != 1 {
			d.t.Fatalf("harness: corrupting %s.%s where %s = %v touched %d rows, want exactly 1", table, column, keyColumn, key, n)
		}
	})
}

// CountRows answers how many rows a table holds, for the assertions whose
// subject is that a restore loaded nothing.
func (d *Daemon) CountRows(table string) int {
	d.t.Helper()
	var n int
	d.WithDB(func(db *sql.DB) {
		if err := db.QueryRow("SELECT count(*) FROM " + table).Scan(&n); err != nil {
			d.t.Fatalf("harness: count %s: %v", table, err)
		}
	})
	return n
}

// DisplacedTurnCount answers how many turns still carry the displaced mark,
// which is what a recovery that put a turn back must leave at zero: a record
// still marked is one the next boot would submit a second time.
func (d *Daemon) DisplacedTurnCount() int {
	d.t.Helper()
	var n int
	d.WithDB(func(db *sql.DB) {
		if err := db.QueryRow("SELECT count(*) FROM turns WHERE displaced = 1").Scan(&n); err != nil {
			d.t.Fatalf("harness: count the displaced turns: %v", err)
		}
	})
	return n
}
