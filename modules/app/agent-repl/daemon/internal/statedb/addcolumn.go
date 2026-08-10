package statedb

import (
	"database/sql"
	"fmt"
)

// This file is the ONE additive-column migration in the daemon's state store.
//
// SQLite has no `ALTER TABLE ... ADD COLUMN IF NOT EXISTS`, so every table that
// grew a column after its first release carries the same three steps: inspect
// the column list, treat an existing column as the migration's SUCCESS
// condition rather than an error, and ALTER otherwise. That shape was written
// out by hand at four sites across two packages — three in the session-state
// manager's schema (`turn_lifecycle_claim`, `workspace_state`, `merge_lease`)
// and one here (`shutdown_hold_prompt`) — with the last of them re-deriving the
// column list through a full `PRAGMA table_info` scan rather than the
// `pragma_table_info` table-valued function the others use.
//
// Four copies of a migration primitive is four chances for one of them to drift
// into a migration that fails on a database it should accept. It lives here
// because `statedb` owns the store every one of those tables is in, and because
// `ssm` already depends on this package while nothing depends on `ssm`.

// HasColumn reports whether table carries column.
//
// A table that does not exist has no columns, which this reports as false
// rather than as an error: SQLite's `pragma_table_info` answers an unknown
// table with an empty row set, and a caller adding a column to a table it just
// created never reaches that case.
func HasColumn(db *sql.DB, table, column string) (bool, error) {
	if db == nil {
		return false, fmt.Errorf("statedb: inspect %s.%s needs an open state store", table, column)
	}
	rows, err := db.Query(`SELECT name FROM pragma_table_info(?)`, table)
	if err != nil {
		return false, fmt.Errorf("statedb: inspect %s columns: %w", table, err)
	}
	defer rows.Close()
	for rows.Next() {
		var name string
		if err := rows.Scan(&name); err != nil {
			return false, fmt.Errorf("statedb: scan %s column name: %w", table, err)
		}
		if name == column {
			return true, nil
		}
	}
	if err := rows.Err(); err != nil {
		return false, fmt.Errorf("statedb: iterate %s columns: %w", table, err)
	}
	return false, nil
}

// AddColumnIfMissing runs one additive migration: `ALTER TABLE table ADD COLUMN
// column ddl`, unless the column is already there.
//
// ddl is the column's type and constraints exactly as they would follow the
// name in a CREATE TABLE — `TEXT`, `TEXT NOT NULL DEFAULT ''`, `INTEGER`. It is
// interpolated rather than bound because SQLite binds values, not identifiers
// or type names; every caller is a compile-time literal in this repository, and
// a caller passing user input would be a defect at the call site rather than
// something this helper could sanitize.
//
// It reports whether it added the column, so a caller that wants to say which
// databases it migrated can, and returns any OTHER failure untouched — an
// unmigratable store is never carried on through.
func AddColumnIfMissing(db *sql.DB, table, column, ddl string) (bool, error) {
	has, err := HasColumn(db, table, column)
	if err != nil {
		return false, err
	}
	if has {
		return false, nil
	}
	if _, err := db.Exec(fmt.Sprintf(`ALTER TABLE %s ADD COLUMN %s %s`, table, column, ddl)); err != nil {
		return false, fmt.Errorf("statedb: add %s.%s: %w", table, column, err)
	}
	return true, nil
}

// AddColumnsIfMissing runs AddColumnIfMissing over a whole column list, which
// is what every multi-column migration in this store actually needs. It stops
// at the first failure, because a half-migrated table is not a state to keep
// widening.
func AddColumnsIfMissing(db *sql.DB, table string, columns []ColumnSpec) error {
	for _, column := range columns {
		if _, err := AddColumnIfMissing(db, table, column.Name, column.DDL); err != nil {
			return err
		}
	}
	return nil
}

// ColumnSpec is one column an additive migration may need to add.
type ColumnSpec struct {
	// Name is the column name.
	Name string
	// DDL is the type and constraints that follow the name.
	DDL string
}
