package kernelevent

import (
	"context"
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"testing"
)

func TestWatchReportsAChange(t *testing.T) {
	cases := []struct {
		name string
		// watched answers the path to watch, made under DIR.
		watched func(t *testing.T, dir string) string
		// change makes the change under DIR.
		change func(t *testing.T, dir string)
	}{
		{
			name:    "an entry made in a watched directory",
			watched: func(t *testing.T, dir string) string { return dir },
			change:  func(t *testing.T, dir string) { writeFile(t, filepath.Join(dir, "f"), "x") },
		},
		{
			name:    "an entry removed from a watched directory",
			watched: func(t *testing.T, dir string) string { writeFile(t, filepath.Join(dir, "f"), "x"); return dir },
			change:  func(t *testing.T, dir string) { removeFile(t, filepath.Join(dir, "f")) },
		},
		{
			name:    "a watched file that grows",
			watched: func(t *testing.T, dir string) string { p := filepath.Join(dir, "f"); writeFile(t, p, "x"); return p },
			change:  func(t *testing.T, dir string) { appendFile(t, filepath.Join(dir, "f"), "more") },
		},
		{
			name:    "a watched file that is removed",
			watched: func(t *testing.T, dir string) string { p := filepath.Join(dir, "f"); writeFile(t, p, "x"); return p },
			change:  func(t *testing.T, dir string) { removeFile(t, filepath.Join(dir, "f")) },
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			dir := t.TempDir()
			w := newWatch(t, tc.watched(t, dir))
			ctx, cancel := context.WithTimeout(context.Background(), waitBound)
			defer cancel()

			// Act
			tc.change(t, dir)
			err := w.Wait(ctx)

			// Assert
			if err != nil {
				t.Fatalf("Wait = %v, want the change reported", err)
			}
		})
	}
}

func TestWatchOnADirectoryIgnoresWritesToAFileInside(t *testing.T) {
	// Arrange: a file inside the watched directory, made before the watch.
	dir := t.TempDir()
	inside := filepath.Join(dir, "f")
	writeFile(t, inside, "x")
	w := newWatch(t, dir)
	quiet, stop := context.WithCancel(context.Background())
	stop()

	// Act: a write to the file changes no entry of the directory.
	appendFile(t, inside, "more")
	err := w.Wait(quiet)

	// Assert
	if !errors.Is(err, context.Canceled) {
		t.Fatalf("Wait = %v, want context.Canceled (no entry changed)", err)
	}
}

func TestWatchWaitTakesEveryPendingChange(t *testing.T) {
	// Arrange: two changes, both pending before the first Wait.
	dir := t.TempDir()
	w := newWatch(t, dir)
	writeFile(t, filepath.Join(dir, "a"), "x")
	writeFile(t, filepath.Join(dir, "b"), "x")
	ctx, cancel := context.WithTimeout(context.Background(), waitBound)
	defer cancel()
	if err := w.Wait(ctx); err != nil {
		t.Fatalf("first Wait: %v", err)
	}
	quiet, stop := context.WithCancel(context.Background())
	stop()

	// Act: nothing changed since, so a wait on an ended context has nothing
	// pending to answer.
	err := w.Wait(quiet)

	// Assert
	if !errors.Is(err, context.Canceled) {
		t.Fatalf("second Wait = %v, want context.Canceled (no change left pending)", err)
	}
}

func TestWatchAddRefusesAMissingPath(t *testing.T) {
	// Arrange
	w, err := NewWatch()
	if err != nil {
		t.Fatalf("NewWatch: %v", err)
	}
	defer w.Close()

	// Act
	err = w.Add(filepath.Join(t.TempDir(), "missing"))

	// Assert
	if !errors.Is(err, fs.ErrNotExist) {
		t.Fatalf("Add = %v, want fs.ErrNotExist", err)
	}
}

func newWatch(t *testing.T, path string) *Watch {
	t.Helper()
	w, err := NewWatch()
	if err != nil {
		t.Fatalf("NewWatch: %v", err)
	}
	t.Cleanup(w.Close)
	if err := w.Add(path); err != nil {
		t.Fatalf("Add(%s): %v", path, err)
	}
	return w
}

func writeFile(t *testing.T, path, body string) {
	t.Helper()
	if err := os.WriteFile(path, []byte(body), 0o600); err != nil {
		t.Fatalf("write %s: %v", path, err)
	}
}

func appendFile(t *testing.T, path, body string) {
	t.Helper()
	f, err := os.OpenFile(path, os.O_APPEND|os.O_WRONLY, 0)
	if err != nil {
		t.Fatalf("open %s: %v", path, err)
	}
	defer f.Close()
	if _, err := f.WriteString(body); err != nil {
		t.Fatalf("append %s: %v", path, err)
	}
}

func removeFile(t *testing.T, path string) {
	t.Helper()
	if err := os.Remove(path); err != nil {
		t.Fatalf("remove %s: %v", path, err)
	}
}
