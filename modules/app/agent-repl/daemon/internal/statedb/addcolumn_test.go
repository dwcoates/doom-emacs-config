package statedb

import (
	"database/sql"
	"path/filepath"
	"strings"
	"testing"
)

// openColumnStore opens a fresh state store carrying one table to migrate.
func openColumnStore(t *testing.T) *sql.DB {
	t.Helper()
	db, err := Open(filepath.Join(t.TempDir(), "state.db"))
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	t.Cleanup(func() { _ = db.Close() })
	if _, err := db.Exec(`CREATE TABLE widget (id TEXT PRIMARY KEY)`); err != nil {
		t.Fatalf("create widget: %v", err)
	}
	return db
}

// columnNames reads a table's column list straight from SQLite, so an
// assertion about a migration never reads back through the helper that
// performed it.
func columnNames(t *testing.T, db *sql.DB, table string) []string {
	t.Helper()
	rows, err := db.Query(`SELECT name FROM pragma_table_info(?)`, table)
	if err != nil {
		t.Fatalf("pragma_table_info(%s): %v", table, err)
	}
	defer rows.Close()
	var names []string
	for rows.Next() {
		var name string
		if err := rows.Scan(&name); err != nil {
			t.Fatalf("scan column name: %v", err)
		}
		names = append(names, name)
	}
	if err := rows.Err(); err != nil {
		t.Fatalf("iterate columns: %v", err)
	}
	return names
}

func TestHasColumnFindsAColumnTheTableCarries(t *testing.T) {
	// Arrange.
	db := openColumnStore(t)

	// Act.
	has, err := HasColumn(db, "widget", "id")

	// Assert.
	if err != nil {
		t.Fatalf("HasColumn: %v", err)
	}
	if !has {
		t.Fatal("has = false, want true for the table's own primary key")
	}
}

func TestHasColumnReportsAColumnTheTableLacks(t *testing.T) {
	// Arrange.
	db := openColumnStore(t)

	// Act.
	has, err := HasColumn(db, "widget", "colour")

	// Assert.
	if err != nil {
		t.Fatalf("HasColumn: %v", err)
	}
	if has {
		t.Fatal("has = true, want false for a column that was never added")
	}
}

func TestHasColumnReportsNoColumnsForATableThatDoesNotExist(t *testing.T) {
	// Arrange — an unknown table has no columns rather than being an error:
	// SQLite answers pragma_table_info for one with an empty row set.
	db := openColumnStore(t)

	// Act.
	has, err := HasColumn(db, "no_such_table", "id")

	// Assert.
	if err != nil {
		t.Fatalf("HasColumn: %v", err)
	}
	if has {
		t.Fatal("has = true, want false for a table that does not exist")
	}
}

func TestHasColumnRefusesAnAbsentStore(t *testing.T) {
	// Arrange, Act.
	_, err := HasColumn(nil, "widget", "id")

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "open state store") {
		t.Fatalf("err = %v, want a refusal naming the missing store", err)
	}
}

func TestAddColumnIfMissingAddsAColumnTheTableLacks(t *testing.T) {
	// Arrange.
	db := openColumnStore(t)

	// Act.
	added, err := AddColumnIfMissing(db, "widget", "colour", `TEXT NOT NULL DEFAULT ''`)

	// Assert.
	if err != nil {
		t.Fatalf("AddColumnIfMissing: %v", err)
	}
	if !added {
		t.Fatal("added = false, want true for a column the table lacked")
	}
	if got := columnNames(t, db, "widget"); len(got) != 2 || got[1] != "colour" {
		t.Fatalf("columns = %v, want the added colour column", got)
	}
}

func TestAddColumnIfMissingAppliesTheSuppliedDDL(t *testing.T) {
	// Arrange — the DDL is the whole point of the helper: a NOT NULL DEFAULT
	// column must read back its default on the rows that predate it.
	db := openColumnStore(t)
	if _, err := db.Exec(`INSERT INTO widget(id) VALUES ('w-1')`); err != nil {
		t.Fatalf("seed widget: %v", err)
	}

	// Act.
	if _, err := AddColumnIfMissing(db, "widget", "colour", `TEXT NOT NULL DEFAULT 'unset'`); err != nil {
		t.Fatalf("AddColumnIfMissing: %v", err)
	}

	// Assert.
	var colour string
	if err := db.QueryRow(`SELECT colour FROM widget WHERE id = 'w-1'`).Scan(&colour); err != nil {
		t.Fatalf("read back colour: %v", err)
	}
	if colour != "unset" {
		t.Fatalf("colour = %q, want the DDL's default on the pre-existing row", colour)
	}
}

func TestAddColumnIfMissingIsANoOpWhenTheColumnIsAlreadyThere(t *testing.T) {
	// Arrange — an already-migrated store is the migration's SUCCESS
	// condition, which is the entire reason this helper exists.
	db := openColumnStore(t)
	if _, err := AddColumnIfMissing(db, "widget", "colour", `TEXT`); err != nil {
		t.Fatalf("first AddColumnIfMissing: %v", err)
	}

	// Act.
	added, err := AddColumnIfMissing(db, "widget", "colour", `TEXT`)

	// Assert.
	if err != nil {
		t.Fatalf("second AddColumnIfMissing: %v", err)
	}
	if added {
		t.Fatal("added = true, want false on the second pass over the same column")
	}
}

func TestAddColumnIfMissingSurfacesAnUnmigratableTable(t *testing.T) {
	// Arrange — SQLite refuses ADD COLUMN on a table that does not exist, and
	// that is a failure to carry on through rather than one to absorb.
	db := openColumnStore(t)

	// Act.
	_, err := AddColumnIfMissing(db, "no_such_table", "colour", `TEXT`)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "add no_such_table.colour") {
		t.Fatalf("err = %v, want the failure named against the table and column", err)
	}
}

func TestAddColumnsIfMissingAddsEveryColumnInTheList(t *testing.T) {
	// Arrange.
	db := openColumnStore(t)

	// Act.
	err := AddColumnsIfMissing(db, "widget", []ColumnSpec{
		{Name: "colour", DDL: `TEXT`},
		{Name: "size", DDL: `INTEGER`},
	})

	// Assert.
	if err != nil {
		t.Fatalf("AddColumnsIfMissing: %v", err)
	}
	got := columnNames(t, db, "widget")
	if len(got) != 3 || got[1] != "colour" || got[2] != "size" {
		t.Fatalf("columns = %v, want both listed columns added", got)
	}
}

func TestAddColumnsIfMissingStopsAtTheFirstFailure(t *testing.T) {
	// Arrange — a half-migrated table is not a state to keep widening, so the
	// column AFTER a failed one must not be added.
	db := openColumnStore(t)

	// Act.
	err := AddColumnsIfMissing(db, "widget", []ColumnSpec{
		{Name: "colour", DDL: `NOT A TYPE(((`},
		{Name: "size", DDL: `INTEGER`},
	})

	// Assert.
	if err == nil {
		t.Fatal("err = nil, want the malformed DDL to surface")
	}
	for _, name := range columnNames(t, db, "widget") {
		if name == "size" {
			t.Fatal("size was added after an earlier column failed")
		}
	}
}
