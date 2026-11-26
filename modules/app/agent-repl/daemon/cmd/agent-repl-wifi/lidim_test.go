package main

import (
	"context"
	"database/sql"
	"os"
	"path/filepath"
	"testing"
)

// stateDB makes a state database at a fresh path, with the lid-dim table when
// withTable is set and the row when level is not nil.
func stateDB(t *testing.T, withTable bool, level *float64) string {
	t.Helper()
	path := filepath.Join(t.TempDir(), "wsm.db")
	db, err := sql.Open("sqlite", "file:"+path)
	if err != nil {
		t.Fatalf("open: %v", err)
	}
	defer func() { _ = db.Close() }()
	if _, err := db.Exec(`CREATE TABLE unrelated (id INTEGER)`); err != nil {
		t.Fatalf("create the unrelated table: %v", err)
	}
	if withTable {
		if _, err := db.Exec(`CREATE TABLE persistent_wifi_lid_dim (id INTEGER PRIMARY KEY CHECK (id = 1), restore_level REAL NOT NULL)`); err != nil {
			t.Fatalf("create the lid-dim table: %v", err)
		}
	}
	if level != nil {
		if _, err := db.Exec(`INSERT INTO persistent_wifi_lid_dim (id, restore_level) VALUES (1, ?)`, *level); err != nil {
			t.Fatalf("insert the row: %v", err)
		}
	}
	return path
}

func ptr(f float64) *float64 { return &f }

func TestLidDimLevelReadsTheRowAnEarlierBuildKept(t *testing.T) {
	tests := []struct {
		name      string
		path      func(t *testing.T) string
		wantLevel float64
		wantOK    bool
		wantErr   bool
	}{
		{"no database has no dim", func(t *testing.T) string { return filepath.Join(t.TempDir(), "absent.db") }, 0, false, false},
		{"a database without the table has no dim", func(t *testing.T) string { return stateDB(t, false, nil) }, 0, false, false},
		{"a table without the row has no dim", func(t *testing.T) string { return stateDB(t, true, nil) }, 0, false, false},
		{"a kept row is the dim", func(t *testing.T) string { return stateDB(t, true, ptr(0.6)) }, 0.6, true, false},
		{"a level above 1 is a corrupt record", func(t *testing.T) string { return stateDB(t, true, ptr(1.5)) }, 0, false, true},
		{"a level below 0 is a corrupt record", func(t *testing.T) string { return stateDB(t, true, ptr(-0.1)) }, 0, false, true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			dims := sqliteLidDims{path: tt.path(t)}

			// Act
			level, ok, err := dims.LidDimLevel(context.Background())

			// Assert
			if (err != nil) != tt.wantErr || ok != tt.wantOK || level != tt.wantLevel {
				t.Fatalf("LidDimLevel() = (%v, %v, %v), want (%v, %v, err %v)", level, ok, err, tt.wantLevel, tt.wantOK, tt.wantErr)
			}
		})
	}
}

func TestLidDimLevelFailsOnAFileThatIsNoDatabase(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "wsm.db")
	if err := os.WriteFile(path, []byte("not a database, just bytes that are long enough to be read as a page header"), 0o600); err != nil {
		t.Fatalf("write: %v", err)
	}

	// Act
	_, _, err := sqliteLidDims{path: path}.LidDimLevel(context.Background())

	// Assert
	if err == nil {
		t.Fatal("LidDimLevel() read a file that is no database")
	}
}

func TestClearLidDimLevelForgetsTheKeptRowAndTouchesNothingElse(t *testing.T) {
	tests := []struct {
		name string
		path func(t *testing.T) string
	}{
		{"a kept row is removed", func(t *testing.T) string { return stateDB(t, true, ptr(0.6)) }},
		{"no row is a no-op", func(t *testing.T) string { return stateDB(t, true, nil) }},
		{"no table is a no-op", func(t *testing.T) string { return stateDB(t, false, nil) }},
		{"no database is a no-op", func(t *testing.T) string { return filepath.Join(t.TempDir(), "absent.db") }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			dims := sqliteLidDims{path: tt.path(t)}

			// Act
			err := dims.ClearLidDimLevel(context.Background())

			// Assert
			if err != nil {
				t.Fatalf("ClearLidDimLevel() error = %v", err)
			}
			if _, ok, err := dims.LidDimLevel(context.Background()); err != nil || ok {
				t.Fatalf("after the clear LidDimLevel() = (ok %v, err %v), want no dim", ok, err)
			}
		})
	}
}
