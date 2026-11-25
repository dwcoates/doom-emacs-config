package account_test

import (
	"context"
	"os"
	"path/filepath"
	"testing"

	"agent-repld/internal/account"
	"agent-repld/internal/dlog"
)

func TestMoveProjectMovesTheFolderUnderEveryRoot(t *testing.T) {
	// Arrange
	f := newTranscriptFixture(t)
	to := filepath.Join(filepath.Dir(f.ws), "ws2")
	def := plantTranscript(t, f.def, f.ws, "uuid-1", "{}")
	multi := plantTranscript(t, f.multi, f.ws, "uuid-2", "{}")
	before, _ := os.Stat(def)

	// Act
	err := f.r.MoveProject(context.Background(), f.ws, to)

	// Assert
	if err != nil {
		t.Fatalf("MoveProject: %v", err)
	}
	for _, path := range []string{account.TranscriptPath(f.def, to, "uuid-1"), account.TranscriptPath(f.multi, to, "uuid-2")} {
		if _, err := os.Stat(path); err != nil {
			t.Fatalf("%s after the move: %v", path, err)
		}
	}
	for _, gone := range []string{def, multi} {
		if _, err := os.Stat(gone); !os.IsNotExist(err) {
			t.Fatalf("%s still stands after the move (%v)", gone, err)
		}
	}
	after, _ := os.Stat(account.TranscriptPath(f.def, to, "uuid-1"))
	if !os.SameFile(before, after) {
		t.Fatal("the moved transcript is a different file, so its inode-keyed cursor would restart")
	}
}

func TestMoveProjectWithNothingToMoveIsNoFailure(t *testing.T) {
	// Arrange
	f := newTranscriptFixture(t)

	// Act
	err := f.r.MoveProject(context.Background(), f.ws, filepath.Join(filepath.Dir(f.ws), "ws2"))

	// Assert
	if err != nil {
		t.Fatalf("MoveProject = %v, want nothing moved and no failure", err)
	}
}

func TestMoveProjectBesideAnExistingFolderMovesEachEntry(t *testing.T) {
	// Arrange
	f := newTranscriptFixture(t)
	to := filepath.Join(filepath.Dir(f.ws), "ws2")
	plantTranscript(t, f.def, f.ws, "uuid-moved", "{}")
	kept := plantTranscript(t, f.def, to, "uuid-kept", "{}")

	// Act
	err := f.r.MoveProject(context.Background(), f.ws, to)

	// Assert
	if err != nil {
		t.Fatalf("MoveProject: %v", err)
	}
	for _, path := range []string{kept, account.TranscriptPath(f.def, to, "uuid-moved")} {
		if _, err := os.Stat(path); err != nil {
			t.Fatalf("%s: %v", path, err)
		}
	}
	if _, err := os.Stat(account.ProjectDir(f.def, f.ws)); !os.IsNotExist(err) {
		t.Fatalf("the emptied source folder still stands (%v)", err)
	}
}

func TestMoveProjectRefusesAnEntryBothFoldersHold(t *testing.T) {
	// Arrange
	f := newTranscriptFixture(t)
	log := dlog.NewTestLogger()
	r, err := account.New(account.Roots{Default: f.def, MultiRepo: f.multi, MultiRepoRoot: filepath.Join(filepath.Dir(f.def), "multi-repos")}, log)
	if err != nil {
		t.Fatalf("account.New: %v", err)
	}
	to := filepath.Join(filepath.Dir(f.ws), "ws2")
	from := plantTranscript(t, f.def, f.ws, "uuid-same", "{}")
	plantTranscript(t, f.def, to, "uuid-same", "{}")

	// Act
	err = r.MoveProject(context.Background(), f.ws, to)

	// Assert
	if err == nil {
		t.Fatal("MoveProject = nil, want the collision refused")
	}
	if _, err := os.Stat(from); err != nil {
		t.Fatalf("the source transcript moved despite the refusal: %v", err)
	}
	if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: "daemon.account.move_project"}); !ok {
		t.Fatalf("records = %v, want the refusal at ERROR", log.Records())
	}
}
