package dlog

import (
	"os"
	"path/filepath"
	"testing"
)

func TestOpenRunLogRotatesThePreviousRun(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), "logs", "daemon.run.log")
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	if err := os.WriteFile(path, []byte("previous run\n"), 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}

	// Act.
	rl, err := openRunLog(path, RunLogBackups)
	if err != nil {
		t.Fatalf("openRunLog: %v", err)
	}
	defer rl.close()

	// Assert.
	current, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read current: %v", err)
	}
	if len(current) != 0 {
		t.Fatalf("current run log = %q, want empty — it is restart-scoped", current)
	}
	backup, err := os.ReadFile(path + ".1")
	if err != nil {
		t.Fatalf("read backup: %v", err)
	}
	if string(backup) != "previous run\n" {
		t.Fatalf("backup .1 = %q, want the previous run", backup)
	}
}

func TestOpenRunLogOnAFreshStateRootIsNotAnError(t *testing.T) {
	// Arrange: nothing exists, not even the logs directory.
	path := filepath.Join(t.TempDir(), "logs", "daemon.run.log")

	// Act.
	rl, err := openRunLog(path, RunLogBackups)

	// Assert.
	if err != nil {
		t.Fatalf("openRunLog on a fresh state root: %v", err)
	}
	defer rl.close()
	if _, err := os.Stat(path); err != nil {
		t.Fatalf("stat: %v", err)
	}
}

func TestRunLogRetainsExactlyNBackups(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	path := filepath.Join(dir, "daemon.run.log")

	// Act: more restarts than there are retained slots.
	for i := 0; i < RunLogBackups+3; i++ {
		rl, err := openRunLog(path, RunLogBackups)
		if err != nil {
			t.Fatalf("openRunLog %d: %v", i, err)
		}
		if err := rl.write([]byte("run\n")); err != nil {
			t.Fatalf("write %d: %v", i, err)
		}
		if err := rl.close(); err != nil {
			t.Fatalf("close %d: %v", i, err)
		}
	}

	// Assert.
	if _, err := os.Stat(backupPath(path, RunLogBackups)); err != nil {
		t.Fatalf("backup %d is missing: %v", RunLogBackups, err)
	}
	if _, err := os.Stat(backupPath(path, RunLogBackups+1)); !os.IsNotExist(err) {
		t.Fatalf("backup %d exists; exactly %d are retained", RunLogBackups+1, RunLogBackups)
	}
}

func TestRunLogBackupsAreOrderedNewestFirst(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), "daemon.run.log")
	for _, body := range []string{"oldest\n", "middle\n", "newest\n"} {
		rl, err := openRunLog(path, RunLogBackups)
		if err != nil {
			t.Fatalf("openRunLog: %v", err)
		}
		if err := rl.write([]byte(body)); err != nil {
			t.Fatalf("write: %v", err)
		}
		if err := rl.close(); err != nil {
			t.Fatalf("close: %v", err)
		}
	}

	// Act: one more restart moves "newest" into slot 1.
	rl, err := openRunLog(path, RunLogBackups)
	if err != nil {
		t.Fatalf("openRunLog: %v", err)
	}
	defer rl.close()

	// Assert.
	for slot, want := range map[int]string{1: "newest\n", 2: "middle\n", 3: "oldest\n"} {
		got, err := os.ReadFile(backupPath(path, slot))
		if err != nil {
			t.Fatalf("read backup %d: %v", slot, err)
		}
		if string(got) != want {
			t.Fatalf("backup %d = %q, want %q", slot, got, want)
		}
	}
}

func TestRunLogRollsAtTheInRunCap(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), "daemon.run.log")
	rl, err := openRunLog(path, RunLogBackups)
	if err != nil {
		t.Fatalf("openRunLog: %v", err)
	}
	defer rl.close()
	if err := rl.f.Truncate(CapBytes); err != nil {
		t.Fatalf("grow: %v", err)
	}
	rl.size = CapBytes

	// Act.
	if err := rl.write([]byte("after the cap\n")); err != nil {
		t.Fatalf("write: %v", err)
	}

	// Assert: the newest evidence is in the current file, the older half is
	// retained rather than discarded.
	current, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read current: %v", err)
	}
	if string(current) != "after the cap\n" {
		t.Fatalf("current = %q, want only the post-cap record", current)
	}
	info, err := os.Stat(backupPath(path, 1))
	if err != nil {
		t.Fatalf("stat backup: %v", err)
	}
	if info.Size() != CapBytes {
		t.Fatalf("backup size = %d, want the capped %d retained", info.Size(), CapBytes)
	}
}

func TestRunLogRefusesRecordsOnceClosed(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), "daemon.run.log")
	rl, err := openRunLog(path, RunLogBackups)
	if err != nil {
		t.Fatalf("openRunLog: %v", err)
	}
	if err := rl.close(); err != nil {
		t.Fatalf("close: %v", err)
	}

	// Act.
	err = rl.write([]byte("late\n"))

	// Assert.
	if err == nil {
		t.Fatalf("a closed run log accepted a record")
	}
}
