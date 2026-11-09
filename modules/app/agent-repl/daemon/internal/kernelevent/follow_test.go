package kernelevent

import (
	"context"
	"errors"
	"net"
	"os"
	"path/filepath"
	"testing"
)

func TestFollowReportsAChange(t *testing.T) {
	cases := []struct {
		name string
		// follow answers the paths to follow, made under DIR.
		follow func(t *testing.T, dir string) []string
		// change makes the change under DIR.
		change func(t *testing.T, dir string)
	}{
		{name: "an entry made in a followed directory",
			follow: func(_ *testing.T, dir string) []string { return []string{dir} },
			change: func(t *testing.T, dir string) { writeFile(t, filepath.Join(dir, "f"), "x") }},
		{name: "a followed file that grows",
			follow: func(t *testing.T, dir string) []string {
				p := filepath.Join(dir, "f")
				writeFile(t, p, "x")
				return []string{p}
			},
			change: func(t *testing.T, dir string) { appendFile(t, filepath.Join(dir, "f"), "more") }},
		{name: "a followed file made after the follow began, once its directory changed",
			follow: func(_ *testing.T, dir string) []string { return []string{dir, filepath.Join(dir, "f")} },
			change: func(t *testing.T, dir string) { writeFile(t, filepath.Join(dir, "f"), "x") }},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			dir := t.TempDir()
			ctx, cancel := context.WithTimeout(context.Background(), waitBound)
			defer cancel()
			changes, stop := Follow(ctx, tc.follow(t, dir)...)
			defer stop()

			// Act
			tc.change(t, dir)
			err := <-changes

			// Assert
			if err != nil {
				t.Fatalf("Follow reported %v, want the change", err)
			}
		})
	}
}

func TestFollowWatchesAFileMadeAfterItBegan(t *testing.T) {
	// Arrange: the file does not exist when the follow begins; making it is
	// one change, and its growth must be a second.
	dir := t.TempDir()
	file := filepath.Join(dir, "f")
	ctx, cancel := context.WithTimeout(context.Background(), waitBound)
	defer cancel()
	changes, stop := Follow(ctx, dir, file)
	defer stop()
	writeFile(t, file, "x")
	if err := <-changes; err != nil {
		t.Fatalf("Follow reported %v, want the file made", err)
	}

	// Act
	appendFile(t, file, "more")
	err := <-changes

	// Assert
	if err != nil {
		t.Fatalf("Follow reported %v, want the growth of the file made later", err)
	}
}

func TestFollowRefusesWhenNothingItFollowsExists(t *testing.T) {
	// Arrange
	missing := filepath.Join(t.TempDir(), "missing")

	// Act
	changes, stop := Follow(context.Background(), missing)
	defer stop()
	err := <-changes

	// Assert
	if !errors.Is(err, ErrNothingToWatch) {
		t.Fatalf("Follow reported %v, want ErrNothingToWatch", err)
	}
}

func TestFollowReportsTheLossOfEverythingItFollows(t *testing.T) {
	// Arrange: the one followed directory is removed.
	parent := t.TempDir()
	dir := filepath.Join(parent, "d")
	if err := os.Mkdir(dir, 0o755); err != nil {
		t.Fatal(err)
	}
	ctx, cancel := context.WithTimeout(context.Background(), waitBound)
	defer cancel()
	changes, stop := Follow(ctx, dir)
	defer stop()

	// Act: the removal is a change, after which nothing is left to follow.
	if err := os.Remove(dir); err != nil {
		t.Fatal(err)
	}
	var err error
	for err == nil {
		err = <-changes
	}

	// Assert
	if !errors.Is(err, ErrNothingToWatch) {
		t.Fatalf("Follow reported %v, want ErrNothingToWatch", err)
	}
}

func TestFollowStopEndsTheWatch(t *testing.T) {
	// Arrange
	changes, stop := Follow(context.Background(), t.TempDir())

	// Act
	stop()

	// Assert: nothing is sent once stopped.
	select {
	case err := <-changes:
		t.Fatalf("Follow sent %v after its stop", err)
	default:
	}
}

func TestMergeReportsAChangeFromEitherSource(t *testing.T) {
	for _, changed := range []int{0, 1} {
		t.Run([]string{"the first source", "the second source"}[changed], func(t *testing.T) {
			// Arrange
			dirs := []string{t.TempDir(), t.TempDir()}
			ctx, cancel := context.WithTimeout(context.Background(), waitBound)
			defer cancel()
			changes, stop := Merge(ctx, FollowPaths(dirs[0]), FollowPaths(dirs[1]))
			defer stop()

			// Act
			writeFile(t, filepath.Join(dirs[changed], "f"), "x")
			err := <-changes

			// Assert
			if err != nil {
				t.Fatalf("Merge reported %v, want the change", err)
			}
		})
	}
}

func TestMergeReportsASourcesError(t *testing.T) {
	// Arrange: the second source follows nothing that exists.
	ctx, cancel := context.WithTimeout(context.Background(), waitBound)
	defer cancel()

	// Act
	changes, stop := Merge(ctx, FollowPaths(t.TempDir()), FollowPaths(filepath.Join(t.TempDir(), "missing")))
	defer stop()
	err := <-changes

	// Assert
	if !errors.Is(err, ErrNothingToWatch) {
		t.Fatalf("Merge reported %v, want ErrNothingToWatch", err)
	}
}

func TestFollowSkipsASocketAndFollowsItsDirectory(t *testing.T) {
	// Arrange: a listening unix socket beside the directory it lives in.
	dir, err := os.MkdirTemp("", "kes")
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { _ = os.RemoveAll(dir) })
	sock := filepath.Join(dir, "s.sock")
	l, err := net.Listen("unix", sock)
	if err != nil {
		t.Fatalf("listen: %v", err)
	}
	t.Cleanup(func() { l.Close() })
	ctx, cancel := context.WithTimeout(context.Background(), waitBound)
	defer cancel()

	// Act: follow the socket and its directory, then change the directory.
	changes, stop := Follow(ctx, sock, dir)
	defer stop()
	writeFile(t, filepath.Join(dir, "f"), "x")
	err = <-changes

	// Assert
	if err != nil {
		t.Fatalf("Follow reported %v, want the directory's change", err)
	}
}
