package dlog

import (
	"errors"
	"os"
	"path/filepath"
	"syscall"
	"testing"
	"time"

	"agentrepl/logging"

	"golang.org/x/sys/unix"
)

var stdioNow = time.Date(2026, 10, 20, 12, 0, 0, 0, time.UTC)

// stdioFixture is a logs directory holding a stdio log, a descriptor on it
// standing in for the daemon's stderr, and one test-owned descriptor standing
// in for fds 1 and 2.
type stdioFixture struct {
	logsDir string
	path    string
	std     *os.File
	target  *os.File
}

func newStdioFixture(t *testing.T) stdioFixture {
	t.Helper()
	logsDir := t.TempDir()
	path := filepath.Join(logsDir, StdioLogName)
	std, err := os.OpenFile(path, os.O_CREATE|os.O_APPEND|os.O_WRONLY, 0o644)
	if err != nil {
		t.Fatalf("open the stand-in stderr: %v", err)
	}
	t.Cleanup(func() { _ = std.Close() })
	target, err := os.CreateTemp(t.TempDir(), "fd-*")
	if err != nil {
		t.Fatalf("create the stand-in fd: %v", err)
	}
	t.Cleanup(func() { _ = target.Close() })
	return stdioFixture{logsDir: logsDir, path: path, std: std, target: target}
}

func (f stdioFixture) adopt(t *testing.T) *stdioLog {
	t.Helper()
	l, err := adoptStdio(f.logsDir, f.std, []int{int(f.target.Fd())}, unix.Dup2)
	if err != nil || l == nil {
		t.Fatalf("adoptStdio = (%v, %v), want the stdio log taken over", l, err)
	}
	t.Cleanup(func() { _ = l.close() })
	return l
}

// writeThroughFd writes s through the stand-in fd, as the daemon's stderr would.
func (f stdioFixture) writeThroughFd(t *testing.T, s string) {
	t.Helper()
	if _, err := f.target.WriteString(s); err != nil {
		t.Fatalf("write through the stand-in fd: %v", err)
	}
}

func TestAdoptStdioTakesOverOnlyTheStdioLog(t *testing.T) {
	cases := []struct {
		name string
		std  func(t *testing.T, f stdioFixture) *os.File
		want bool
	}{
		{name: "stderr is the stdio log", std: func(_ *testing.T, f stdioFixture) *os.File { return f.std }, want: true},
		{name: "stderr is another regular file", std: func(t *testing.T, _ stdioFixture) *os.File {
			other, err := os.CreateTemp(t.TempDir(), "other-*")
			if err != nil {
				t.Fatalf("create: %v", err)
			}
			t.Cleanup(func() { _ = other.Close() })
			return other
		}, want: false},
		{name: "stderr is a pipe", std: func(t *testing.T, _ stdioFixture) *os.File {
			r, w, err := os.Pipe()
			if err != nil {
				t.Fatalf("pipe: %v", err)
			}
			t.Cleanup(func() { _ = r.Close(); _ = w.Close() })
			return w
		}, want: false},
		{name: "the stdio log is absent", std: func(t *testing.T, f stdioFixture) *os.File {
			if err := os.Remove(f.path); err != nil {
				t.Fatalf("remove: %v", err)
			}
			return f.std
		}, want: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			f := newStdioFixture(t)
			std := tc.std(t, f)

			// Act.
			l, err := adoptStdio(f.logsDir, std, []int{int(f.target.Fd())}, unix.Dup2)
			if l != nil {
				t.Cleanup(func() { _ = l.close() })
			}

			// Assert.
			if err != nil || (l != nil) != tc.want {
				t.Fatalf("adoptStdio = (%v, %v), want adopted=%v", l, err, tc.want)
			}
		})
	}
}

func TestAdoptedStdioWritesLandInTheStdioLog(t *testing.T) {
	// Arrange.
	f := newStdioFixture(t)
	f.adopt(t)

	// Act.
	f.writeThroughFd(t, "panic: boom\n")

	// Assert.
	if got := readText(t, f.path); got != "panic: boom\n" {
		t.Fatalf("the stdio log holds %q, want the write through the pointed fd", got)
	}
}

func TestAdoptStdioRefusesWhenTheFdCannotBePointed(t *testing.T) {
	// Arrange.
	f := newStdioFixture(t)
	refused := errors.New("dup2 refused")

	// Act.
	l, err := adoptStdio(f.logsDir, f.std, []int{int(f.target.Fd())}, func(int, int) error { return refused })

	// Assert: refused, and the lock released with the file.
	if l != nil || !errors.Is(err, refused) {
		t.Fatalf("adoptStdio = (%v, %v), want the dup2 failure", l, err)
	}
	if held := heldByAnother(t, f.path); held {
		t.Errorf("the stdio log is still locked after a refused adoption")
	}
}

func TestStdioMaintain(t *testing.T) {
	cases := []struct {
		name string
		// arrange runs after adoption.
		arrange     func(t *testing.T, f stdioFixture)
		wantRolled  bool
		wantRemoved int
	}{
		{name: "a small fresh log is left alone", arrange: func(t *testing.T, f stdioFixture) {
			f.writeThroughFd(t, "x\n")
		}},
		{name: "a log at the cap is rolled", arrange: func(t *testing.T, f stdioFixture) {
			if err := f.target.Truncate(CapBytes); err != nil {
				t.Fatalf("truncate: %v", err)
			}
		}, wantRolled: true},
		{name: "a log past the retention is rolled and removed", arrange: func(t *testing.T, f stdioFixture) {
			f.writeThroughFd(t, "x\n")
			age(t, f.path, stdioNow.Add(-logging.DefaultRetention-time.Hour))
		}, wantRolled: true, wantRemoved: 1},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			f := newStdioFixture(t)
			l := f.adopt(t)
			tc.arrange(t, f)

			// Act.
			result, err := l.maintain(stdioNow)

			// Assert: the outcome, and that the fd now writes the path's file.
			if err != nil || result.RolledCurrent != tc.wantRolled || len(result.Removed) != tc.wantRemoved {
				t.Fatalf("maintain = (%+v, %v), want rolled=%v removed=%d", result, err, tc.wantRolled, tc.wantRemoved)
			}
			if !tc.wantRolled {
				return
			}
			f.writeThroughFd(t, "after\n")
			if got := readText(t, f.path); got != "after\n" {
				t.Errorf("the stdio log holds %q, want only the write after the roll", got)
			}
		})
	}
}

func TestStdioMaintainRefusals(t *testing.T) {
	cases := []struct {
		name    string
		arrange func(t *testing.T, f stdioFixture, l *stdioLog)
	}{
		{name: "the file is closed", arrange: func(t *testing.T, _ stdioFixture, l *stdioLog) {
			if err := l.close(); err != nil {
				t.Fatalf("close: %v", err)
			}
		}},
		{name: "the fd cannot be re-pointed after a roll", arrange: func(t *testing.T, f stdioFixture, l *stdioLog) {
			if err := f.target.Truncate(CapBytes); err != nil {
				t.Fatalf("truncate: %v", err)
			}
			l.dup = func(int, int) error { return errors.New("dup2 refused") }
		}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			f := newStdioFixture(t)
			l := f.adopt(t)
			tc.arrange(t, f, l)

			// Act.
			_, err := l.maintain(stdioNow)

			// Assert.
			if err == nil {
				t.Fatalf("maintain succeeded, want %s refused", tc.name)
			}
		})
	}
}

func age(t *testing.T, path string, at time.Time) {
	t.Helper()
	if err := os.Chtimes(path, at, at); err != nil {
		t.Fatalf("age %s: %v", path, err)
	}
}

// heldByAnother answers whether some other descriptor holds a flock on path.
func heldByAnother(t *testing.T, path string) bool {
	t.Helper()
	f, err := os.Open(path)
	if err != nil {
		t.Fatalf("open %s: %v", path, err)
	}
	defer f.Close()
	err = syscall.Flock(int(f.Fd()), syscall.LOCK_EX|syscall.LOCK_NB)
	if errors.Is(err, syscall.EWOULDBLOCK) {
		return true
	}
	if err != nil {
		t.Fatalf("flock %s: %v", path, err)
	}
	return false
}
