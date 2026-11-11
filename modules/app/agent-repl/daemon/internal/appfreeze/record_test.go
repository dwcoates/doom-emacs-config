package appfreeze

import (
	"os"
	"path/filepath"
	"reflect"
	"testing"
)

func TestFileStoreRoundTrips(t *testing.T) {
	// Arrange
	s := FileStore{Path: filepath.Join(t.TempDir(), "persistent-wifi", RecordFile)}
	want := Record{Apps: []RecordedApp{{Name: "Slack", PIDs: []int{200, 201}}}}

	// Act
	_, before, beforeErr := s.Load()
	saveErr := s.Save(want)
	got, ok, loadErr := s.Load()
	clearErr := s.Clear()
	_, after, afterErr := s.Load()

	// Assert
	if before || beforeErr != nil {
		t.Fatalf("Load() before any save = %v, %v; want nothing kept", before, beforeErr)
	}
	if saveErr != nil || loadErr != nil || !ok || !reflect.DeepEqual(got, want) {
		t.Fatalf("round trip = %v, %v (save %v, load %v); want %v", got, ok, saveErr, loadErr, want)
	}
	if clearErr != nil || after || afterErr != nil {
		t.Fatalf("after Clear: kept %v (clear %v, load %v); want nothing", after, clearErr, afterErr)
	}
}

func TestFileStoreClearOfNothingIsNoError(t *testing.T) {
	// Arrange
	s := FileStore{Path: filepath.Join(t.TempDir(), RecordFile)}

	// Act and assert
	if err := s.Clear(); err != nil {
		t.Fatalf("Clear() = %v, want nil", err)
	}
}

func TestFileStoreRefusesACorruptRecord(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), RecordFile)
	if err := os.WriteFile(path, []byte("{not json"), 0o600); err != nil {
		t.Fatal(err)
	}

	// Act
	_, _, err := FileStore{Path: path}.Load()

	// Assert
	if err == nil {
		t.Fatal("Load() accepted a corrupt record")
	}
}

func TestFileStoreSaveFailsWhereNoDirectoryCanBeMade(t *testing.T) {
	// Arrange: the directory's parent is a file.
	dir := t.TempDir()
	blocker := filepath.Join(dir, "blocker")
	if err := os.WriteFile(blocker, nil, 0o600); err != nil {
		t.Fatal(err)
	}

	// Act
	err := FileStore{Path: filepath.Join(blocker, "sub", RecordFile)}.Save(Record{})

	// Assert
	if err == nil {
		t.Fatal("Save() succeeded under a file")
	}
}
