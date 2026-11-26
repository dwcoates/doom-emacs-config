package tempdirstest

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"agent-repld/internal/tempdirs"
)

func TestMain(m *testing.M) { os.Exit(Main(m)) }

func TestGuardExemptsOnlyTheRunsOwnTemporaryRoot(t *testing.T) {
	tests := []struct {
		name    string
		dir     func(t *testing.T) string
		refused bool
	}{
		{name: "a directory from a test's TempDir", dir: func(t *testing.T) string { return filepath.Join(t.TempDir(), "repo") }, refused: false},
		{name: "a directory from os.MkdirTemp", dir: func(t *testing.T) string {
			dir, err := os.MkdirTemp("", "x")
			if err != nil {
				t.Fatal(err)
			}
			t.Cleanup(func() { _ = os.RemoveAll(dir) })
			return dir
		}, refused: false},
		{name: "another temporary folder", dir: func(*testing.T) string { return "/tmp/agent-repl-not-this-run" }, refused: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			g := Guard(t)

			// Act
			err := g.Check(tc.dir(t))

			// Assert
			if _, inside := tempdirs.AsInside(err); inside != tc.refused {
				t.Fatalf("Check = %v, want refused=%v", err, tc.refused)
			}
		})
	}
}

func TestShortBase(t *testing.T) {
	existing, err := os.MkdirTemp("/tmp", "arbase")
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { _ = os.RemoveAll(existing) })
	tests := []struct {
		name    string
		set     string
		want    string
		wantErr bool
	}{
		{name: "unset is /tmp", set: "", want: DefaultShortBase},
		{name: "a directory beneath /tmp is honored", set: existing, want: existing},
		{name: "a directory outside /tmp is refused", set: "/usr", wantErr: true},
		{name: "an absent directory is refused", set: existing + "/absent", wantErr: true},
		{name: "an unclean spelling is refused", set: existing + "/", wantErr: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			getenv := func(string) string { return tc.set }

			// Act
			got, err := ShortBase(getenv)

			// Assert
			if (err != nil) != tc.wantErr || got != tc.want {
				t.Fatalf("ShortBase = %q, %v; want %q (error %v)", got, err, tc.want, tc.wantErr)
			}
		})
	}
}

func TestShortDirIsAShortDirectoryInTheShortBaseRemovedWhenTheTestEnds(t *testing.T) {
	// Arrange
	var made string
	t.Run("a test that makes one", func(t *testing.T) {
		// Act
		made = ShortDir(t, "sd")

		// Assert: a directory under the base, short enough for a unix socket path.
		info, err := os.Stat(made)
		if err != nil || !info.IsDir() {
			t.Fatalf("ShortDir() = %q, stat %v: want an existing directory", made, err)
		}
		base, err := ShortBase(os.Getenv)
		if err != nil {
			t.Fatalf("ShortBase: %v", err)
		}
		if !strings.HasPrefix(made, base+"/sd") {
			t.Fatalf("ShortDir() = %q, want it under %s named for its prefix", made, base)
		}
		if limit := 104 - len("/some-long-socket-name.sock"); len(made) > limit {
			t.Fatalf("ShortDir() = %q (%d bytes), too long for a socket path beside a name", made, len(made))
		}
	})

	// Assert: the subtest's end removed it.
	if _, err := os.Stat(made); !os.IsNotExist(err) {
		t.Fatalf("after the test the directory %q stands (stat %v), want it removed", made, err)
	}
}

func TestShortDirFailsTheTestWhenTheShortBaseIsRefused(t *testing.T) {
	// Arrange: a base that is not beneath /tmp, which ShortBase refuses.
	t.Setenv(EnvShortBase, "/var/not-tmp")
	rec := &recordingT{TB: t}

	// Act
	func() {
		defer func() { _ = recover() }()
		ShortDir(rec, "sd")
	}()

	// Assert
	if !rec.failed {
		t.Fatal("ShortDir() accepted a short base beneath nothing under /tmp")
	}
}

// recordingT is a testing.TB that records a Fatalf instead of ending the test.
type recordingT struct {
	testing.TB
	failed bool
}

func (r *recordingT) Fatalf(string, ...any) { r.failed = true; panic("fatal") }
func (r *recordingT) Helper()               {}
func (r *recordingT) Cleanup(func())        {}
