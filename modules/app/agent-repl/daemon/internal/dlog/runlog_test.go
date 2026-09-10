package dlog

import (
	"os"
	"path/filepath"
	"testing"
)

func TestOpenRunLogAppendsAcrossRestart(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), "logs", "daemon.run.log")
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	if err := os.WriteFile(path, []byte("previous run\n"), 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}

	// Act.
	run, err := openRunLog(path, RunLogBackups)
	if err != nil {
		t.Fatalf("openRunLog: %v", err)
	}
	if err := run.write([]byte("current run\n")); err != nil {
		t.Fatalf("write current run: %v", err)
	}
	if err := run.close(); err != nil {
		t.Fatalf("close: %v", err)
	}

	// Assert.
	current, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read current: %v", err)
	}
	if got, want := string(current), "previous run\ncurrent run\n"; got != want {
		t.Fatalf("current run log = %q, want %q", got, want)
	}
	if _, err := os.Stat(path + ".1"); !os.IsNotExist(err) {
		t.Fatalf("opening consumed a generation; stat .1 = %v", err)
	}
}

func TestOpenRunLogOnAFreshStateRootIsNotAnError(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), "logs", "daemon.run.log")

	// Act.
	run, err := openRunLog(path, RunLogBackups)

	// Assert.
	if err != nil {
		t.Fatalf("openRunLog: %v", err)
	}
	t.Cleanup(func() { _ = run.close() })
	if _, err := os.Stat(path); err != nil {
		t.Fatalf("stat: %v", err)
	}
}

func TestRunLogRotatesOnlyAtTheSizeCap(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), "daemon.run.log")
	run, err := openRunLogSized(path, 10, 2)
	if err != nil {
		t.Fatalf("openRunLogSized: %v", err)
	}
	t.Cleanup(func() { _ = run.close() })
	if err := run.write([]byte("12345678\n")); err != nil {
		t.Fatalf("write first: %v", err)
	}

	// Act.
	if err := run.write([]byte("abcdefgh\n")); err != nil {
		t.Fatalf("write second: %v", err)
	}

	// Assert.
	if got := readText(t, path+".1"); got != "12345678\n" {
		t.Fatalf("generation .1 = %q, want the pre-cap record", got)
	}
	if got := readText(t, path); got != "abcdefgh\n" {
		t.Fatalf("current = %q, want the post-cap record", got)
	}
}

func TestRunLogRetainsExactlyNGenerations(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	path := filepath.Join(dir, "daemon.run.log")
	run, err := openRunLogSized(path, 4, 2)
	if err != nil {
		t.Fatalf("openRunLogSized: %v", err)
	}
	t.Cleanup(func() { _ = run.close() })

	// Act.
	for _, line := range []string{"aaa\n", "bbb\n", "ccc\n", "ddd\n"} {
		if err := run.write([]byte(line)); err != nil {
			t.Fatalf("write %q: %v", line, err)
		}
	}

	// Assert.
	entries, err := os.ReadDir(dir)
	if err != nil {
		t.Fatalf("ReadDir: %v", err)
	}
	if len(entries) != 3 {
		t.Fatalf("files = %d, want current plus two generations", len(entries))
	}
}

func TestRunLogRefusesRecordsOnceClosed(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), "daemon.run.log")
	run, err := openRunLog(path, RunLogBackups)
	if err != nil {
		t.Fatalf("openRunLog: %v", err)
	}
	if err := run.close(); err != nil {
		t.Fatalf("close: %v", err)
	}

	// Act.
	err = run.write([]byte("late\n"))

	// Assert.
	if err == nil {
		t.Fatal("a closed run log accepted a record")
	}
}

func readText(t *testing.T, path string) string {
	t.Helper()
	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read %s: %v", path, err)
	}
	return string(data)
}
