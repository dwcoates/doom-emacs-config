package wsm

import (
	"context"
	"database/sql"
	"errors"
	"os"
	"path/filepath"
	"slices"
	"testing"
	"time"

	"agent-repld/internal/dlog"
)

// changeBound is one wait's failure bound for a commit's report.
const changeBound = 5 * time.Second

func TestChangesReportsACommit(t *testing.T) {
	cases := []struct {
		name string
		// open answers the handle under test and a second handle on the same
		// database (the same one when there is no second).
		open func(t *testing.T) (watched, writer *store)
	}{
		{name: "an in-memory database's own commit", open: func(t *testing.T) (*store, *store) {
			s, _ := testStore(t)
			return s, s
		}},
		{name: "a file database's own commit", open: func(t *testing.T) (*store, *store) {
			s, _ := fileStore(t)
			return s, s
		}},
		{name: "a commit by another handle on the same file", open: func(t *testing.T) (*store, *store) {
			s, _ := fileStore(t)
			other, err := Open(context.Background(), s.path, WithUnsyncedWrites())
			if err != nil {
				t.Fatalf("Open a second handle: %v", err)
			}
			t.Cleanup(func() { other.Close() })
			return s, other.(*store)
		}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			watched, writer := tc.open(t)
			ctx, cancel := context.WithTimeout(context.Background(), changeBound)
			defer cancel()
			changes, stop := watched.Changes(ctx)
			defer stop()

			// Act
			if err := writer.SetMergedSectionFolded(ctx, true); err != nil {
				t.Fatalf("SetMergedSectionFolded: %v", err)
			}
			var err error
			select {
			case err = <-changes:
			case <-ctx.Done():
				t.Fatalf("no commit was reported within %s", changeBound)
			}

			// Assert
			if err != nil {
				t.Fatalf("Changes reported %v, want the commit", err)
			}
		})
	}
}

func TestChangesOnAnInMemoryDatabaseIgnoresAWriteThatFailed(t *testing.T) {
	// Arrange: a read-only refusal is a write that never commits.
	s, _ := testStore(t)
	changes, stop := s.Changes(context.Background())
	defer stop()

	// Act
	err := s.write(context.Background(), "daemon.test.write", nil, func(context.Context, *sql.Tx) error { return errors.New("refused") })

	// Assert
	if err == nil {
		t.Fatalf("write = nil, want the refusal")
	}
	select {
	case got := <-changes:
		t.Fatalf("Changes reported %v for a write that did not commit", got)
	default:
	}
}

func TestChangesStopEndsTheReports(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	changes, stop := s.Changes(context.Background())

	// Act
	stop()
	if err := s.SetMergedSectionFolded(context.Background(), true); err != nil {
		t.Fatalf("SetMergedSectionFolded: %v", err)
	}

	// Assert
	select {
	case got := <-changes:
		t.Fatalf("Changes reported %v after its stop", got)
	default:
	}
}

func TestCommitPathsNameTheDirectoryTheFileItsLogAndTheCommitSignal(t *testing.T) {
	// Arrange
	path := filepath.Join("/state", "wsm.db")

	// Act
	got := CommitPaths(path)

	// Assert
	want := []string{"/state", "/state/wsm.db", "/state/wsm.db-wal", "/state/wsm.db-commits"}
	if !slices.Equal(got, want) {
		t.Fatalf("CommitPaths = %v, want %v", got, want)
	}
}

func TestACommittedWriteOfAFileDatabaseWritesTheCommitSignal(t *testing.T) {
	// Arrange
	s, _ := fileStore(t)
	signal := CommitSignalPath(s.path)
	if err := os.Remove(signal); err != nil && !errors.Is(err, os.ErrNotExist) {
		t.Fatalf("remove the signal: %v", err)
	}

	// Act
	err := s.SetMergedSectionFolded(context.Background(), true)

	// Assert
	info, statErr := os.Stat(signal)
	if err != nil || statErr != nil || info.Size() != 1 {
		t.Fatalf("write = %v, signal = %v (%v), want the commit and a one-byte signal", err, info, statErr)
	}
}

func TestAWriteThatFailedWritesNoCommitSignal(t *testing.T) {
	// Arrange
	s, _ := fileStore(t)
	signal := CommitSignalPath(s.path)
	if err := os.Remove(signal); err != nil && !errors.Is(err, os.ErrNotExist) {
		t.Fatalf("remove the signal: %v", err)
	}

	// Act
	err := s.write(context.Background(), "daemon.test.write", nil, func(context.Context, *sql.Tx) error { return errors.New("refused") })

	// Assert
	if _, statErr := os.Stat(signal); err == nil || !errors.Is(statErr, os.ErrNotExist) {
		t.Fatalf("write = %v, signal stat = %v; want the refusal and no signal", err, statErr)
	}
}

func TestTheCommitSignalNeverGrows(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "wsm.db-commits")

	// Act
	err := errors.Join(writeCommitSignal(path), writeCommitSignal(path), writeCommitSignal(path))

	// Assert
	info, statErr := os.Stat(path)
	if err != nil || statErr != nil || info.Size() != 1 {
		t.Fatalf("writes = %v, signal = %v (%v); want one byte", err, info, statErr)
	}
}

func TestAnUnwritableCommitSignalIsAnErrorAndTheCommitStands(t *testing.T) {
	// Arrange: a directory where the signal file belongs cannot be written.
	s, log := fileStore(t)
	signal := CommitSignalPath(s.path)
	if err := os.Remove(signal); err != nil && !errors.Is(err, os.ErrNotExist) {
		t.Fatalf("remove the signal: %v", err)
	}
	if err := os.Mkdir(signal, 0o700); err != nil {
		t.Fatalf("mkdir: %v", err)
	}

	// Act
	err := s.SetMergedSectionFolded(context.Background(), true)

	// Assert
	if err != nil {
		t.Fatalf("write = %v, want the commit to stand", err)
	}
	records := dlog.FilterRecords(log.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: opSignalCommit})
	if len(records) != 1 || records[0].Context["path"] != signal || records[0].Context["error"] == nil {
		t.Fatalf("records = %+v, want one ERROR naming the signal and its cause", log.Records())
	}
}
