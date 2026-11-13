package persistentwifi

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestEnergyFileStoreLoad(t *testing.T) {
	cases := []struct {
		name    string
		body    *string // nil writes no file
		want    EnergyRecord
		wantOK  bool
		wantErr string // a substring of the error, empty for none
	}{
		{name: "no file is no record"},
		{name: "a record is read", body: textPtr(`{"mode":"2"}`), want: EnergyRecord{Mode: "2"}, wantOK: true},
		{name: "a file that is not JSON is an error", body: textPtr(`{"mode":`), wantErr: "is not a record"},
		{name: "a record naming no powermode is an error", body: textPtr(`{"mode":"high"}`), wantErr: "names no powermode"},
		{name: "an empty record is an error", body: textPtr(`{}`), wantErr: "names no powermode"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			path := filepath.Join(t.TempDir(), EnergyRecordFile)
			if tc.body != nil {
				if err := os.WriteFile(path, []byte(*tc.body), 0o600); err != nil {
					t.Fatal(err)
				}
			}

			// Act
			got, ok, err := EnergyFileStore{Path: path}.Load()

			// Assert
			if tc.wantErr == "" && err != nil || tc.wantErr != "" && (err == nil || !strings.Contains(err.Error(), tc.wantErr)) {
				t.Fatalf("Load() error = %v, want %q", err, tc.wantErr)
			}
			if got != tc.want || ok != tc.wantOK {
				t.Fatalf("Load() = (%+v, %v), want (%+v, %v)", got, ok, tc.want, tc.wantOK)
			}
		})
	}
}

func TestEnergyFileStoreSaveMakesItsDirectoryAndReplaces(t *testing.T) {
	// Arrange
	s := EnergyFileStore{Path: filepath.Join(t.TempDir(), "persistent-wifi", EnergyRecordFile)}
	if err := s.Save(EnergyRecord{Mode: "1"}); err != nil {
		t.Fatalf("Save() error = %v", err)
	}

	// Act
	err := s.Save(EnergyRecord{Mode: "2"})

	// Assert
	got, ok, loadErr := s.Load()
	if err != nil || loadErr != nil || !ok || got.Mode != "2" {
		t.Fatalf("Save() = %v, Load() = (%+v, %v, %v), want High Power Mode kept", err, got, ok, loadErr)
	}
}

func TestEnergyFileStoreSaveReportsAnUnwritableDirectory(t *testing.T) {
	// Arrange: the record's directory is a file.
	dir := t.TempDir()
	blocker := filepath.Join(dir, "persistent-wifi")
	if err := os.WriteFile(blocker, nil, 0o600); err != nil {
		t.Fatal(err)
	}

	// Act
	err := EnergyFileStore{Path: filepath.Join(blocker, EnergyRecordFile)}.Save(EnergyRecord{Mode: "2"})

	// Assert
	if err == nil || !strings.Contains(err.Error(), "make the recorded energy mode's directory") {
		t.Fatalf("Save() error = %v, want the directory named", err)
	}
}

func TestEnergyFileStoreClear(t *testing.T) {
	cases := []struct {
		name string
		kept bool
	}{
		{name: "a kept record is removed", kept: true},
		{name: "clearing none is no error"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s := EnergyFileStore{Path: filepath.Join(t.TempDir(), EnergyRecordFile)}
			if tc.kept {
				if err := s.Save(EnergyRecord{Mode: "2"}); err != nil {
					t.Fatal(err)
				}
			}

			// Act
			err := s.Clear()

			// Assert
			if _, ok, loadErr := s.Load(); err != nil || loadErr != nil || ok {
				t.Fatalf("Clear() = %v, then Load() = (%v, %v), want no record", err, ok, loadErr)
			}
		})
	}
}

func TestEnergyFileStoreClearReportsAFailure(t *testing.T) {
	// Arrange: the record's path is a non-empty directory, which no remove takes.
	path := filepath.Join(t.TempDir(), EnergyRecordFile)
	if err := os.MkdirAll(filepath.Join(path, "inside"), 0o700); err != nil {
		t.Fatal(err)
	}

	// Act
	err := EnergyFileStore{Path: path}.Clear()

	// Assert
	if err == nil || !strings.Contains(err.Error(), "clear the recorded energy mode") {
		t.Fatalf("Clear() error = %v, want the failure named", err)
	}
}

// textPtr answers a pointer to s, for a case that writes a file.
func textPtr(s string) *string { return &s }
