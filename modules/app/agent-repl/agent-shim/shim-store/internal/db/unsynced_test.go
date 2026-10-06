package db

import (
	"os"
	"path/filepath"
	"testing"
)

func TestUnsyncedFromEnv(t *testing.T) {
	tests := []struct {
		name    string
		forbid  string
		value   string
		want    bool
		wantErr bool
	}{
		{name: "a live store with no flag stays durable", forbid: "", value: "", want: false},
		{name: "a live store handed the flag is refused", forbid: "", value: "1", wantErr: true},
		{name: "a test-run store honors the flag", forbid: "1", value: "1", want: true},
		{name: "a test-run store with no flag stays durable", forbid: "1", value: "", want: false},
		{name: "a malformed flag is refused even under the vendor guard", forbid: "1", value: "yes", wantErr: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			t.Setenv(envForbidVendorCalls, tc.forbid)
			t.Setenv(EnvTestUnsyncedWrites, tc.value)

			// Act
			got, err := UnsyncedFromEnv()

			// Assert
			if (err != nil) != tc.wantErr {
				t.Fatalf("UnsyncedFromEnv error = %v, wantErr %v", err, tc.wantErr)
			}
			if got != tc.want {
				t.Fatalf("UnsyncedFromEnv = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestOpenRefusesTheUnsyncedFlagWithoutTheVendorGuard(t *testing.T) {
	// Arrange
	t.Setenv(envForbidVendorCalls, "")
	t.Setenv(EnvTestUnsyncedWrites, "1")
	s, log := newSink(t)
	path := filepath.Join(t.TempDir(), "store.db")

	// Act
	_, err := Open(path, log)

	// Assert
	if err == nil {
		t.Fatal("Open accepted the unsynced flag without the vendor guard")
	}
	s.assertLogged(t, "error", "durability could not be settled")
	if _, statErr := os.Stat(path); !os.IsNotExist(statErr) {
		t.Fatalf("the refused open created the database (stat err = %v)", statErr)
	}
}

func TestOpenRecordsAnUnsyncedDatabaseAtInfo(t *testing.T) {
	// Arrange
	t.Setenv(envForbidVendorCalls, "1")
	t.Setenv(EnvTestUnsyncedWrites, "1")
	s, log := newSink(t)

	// Act
	d, err := Open(filepath.Join(t.TempDir(), "store.db"), log)

	// Assert
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	t.Cleanup(func() { d.Close() }) //nolint:errcheck // best-effort test teardown
	s.assertLogged(t, "info", "skips SQLite's forced flushes")
}
