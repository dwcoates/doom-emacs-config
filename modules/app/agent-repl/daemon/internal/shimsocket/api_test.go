package shimsocket

import (
	"net"
	"os"
	"path/filepath"
	"testing"
)

// sockDir is a temp directory SHORT enough for an AF_UNIX path. t.TempDir()
// embeds the test name, and a table-driven suite's names push sun_path past
// the kernel's 104-byte limit, which fails the bind for a reason that has
// nothing to do with what is under test.
func sockDir(t *testing.T) string {
	t.Helper()
	dir, err := os.MkdirTemp("", "shs")
	if err != nil {
		t.Fatalf("temp dir: %v", err)
	}
	t.Cleanup(func() { _ = os.RemoveAll(dir) })
	return dir
}

// listening binds a real AF_UNIX listener at path and accepts until the test
// ends. A real listener is used rather than a fake because the whole point of
// the probe is the KERNEL's answer to a connect.
func listening(t *testing.T, path string) net.Listener {
	t.Helper()
	ln, err := net.Listen("unix", path)
	if err != nil {
		t.Fatalf("listen %q: %v", path, err)
	}
	done := make(chan struct{})
	go func() {
		defer close(done)
		for {
			conn, err := ln.Accept()
			if err != nil {
				return
			}
			_ = conn.Close()
		}
	}()
	t.Cleanup(func() {
		_ = ln.Close()
		<-done
	})
	return ln
}

// staleSocket binds a listener and closes it WITHOUT unlinking, which is what
// a shim's death leaves behind.
func staleSocket(t *testing.T, path string) {
	t.Helper()
	ln, err := net.Listen("unix", path)
	if err != nil {
		t.Fatalf("listen %q: %v", path, err)
	}
	unix, ok := ln.(*net.UnixListener)
	if !ok {
		t.Fatalf("listener at %q is not a unix listener", path)
	}
	unix.SetUnlinkOnClose(false)
	if err := unix.Close(); err != nil {
		t.Fatalf("close %q: %v", path, err)
	}
}

func TestProbe(t *testing.T) {
	tests := []struct {
		name  string
		setup func(t *testing.T, dir string) string
		want  State
		error bool
	}{
		{
			name: "a path nothing ever bound is absent",
			setup: func(_ *testing.T, dir string) string {
				return filepath.Join(dir, "never.sock")
			},
			want: StateAbsent,
		},
		{
			name: "a bound listener is live",
			setup: func(t *testing.T, dir string) string {
				path := filepath.Join(dir, "live.sock")
				listening(t, path)
				return path
			},
			want: StateLive,
		},
		{
			name: "a socket file whose listener is gone is stale",
			setup: func(t *testing.T, dir string) string {
				path := filepath.Join(dir, "stale.sock")
				staleSocket(t, path)
				return path
			},
			want: StateStale,
		},
		{
			name: "a regular file at the socket path is undetermined, never absent",
			setup: func(t *testing.T, dir string) string {
				path := filepath.Join(dir, "regular.sock")
				if err := os.WriteFile(path, []byte("not a socket"), 0o600); err != nil {
					t.Fatalf("write %q: %v", path, err)
				}
				return path
			},
			want:  StateUndetermined,
			error: true,
		},
		{
			name: "an empty path is undetermined, never absent",
			setup: func(_ *testing.T, _ string) string {
				return "   "
			},
			want:  StateUndetermined,
			error: true,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			path := tc.setup(t, sockDir(t))

			// Act.
			got, err := Probe(path)

			// Assert.
			if got != tc.want {
				t.Fatalf("Probe(%q) = %v, want %v (err %v)", path, got, tc.want, err)
			}
			if tc.error != (err != nil) {
				t.Fatalf("Probe(%q) error = %v, want error: %v", path, err, tc.error)
			}
		})
	}
}

func TestStateString(t *testing.T) {
	tests := []struct {
		name  string
		state State
		want  string
	}{
		{name: "absent", state: StateAbsent, want: "absent"},
		{name: "stale", state: StateStale, want: "stale"},
		{name: "live", state: StateLive, want: "live"},
		{name: "undetermined", state: StateUndetermined, want: "undetermined"},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act.
			got := tc.state.String()

			// Assert.
			if got != tc.want {
				t.Fatalf("State(%d).String() = %q, want %q", tc.state, got, tc.want)
			}
		})
	}
}

func TestClearStaleUnlinksAStalePath(t *testing.T) {
	// Arrange.
	path := filepath.Join(sockDir(t), "stale.sock")
	staleSocket(t, path)

	// Act.
	err := ClearStale(nil, path)

	// Assert.
	if err != nil {
		t.Fatalf("ClearStale(%q) = %v, want nil", path, err)
	}
	if _, statErr := os.Lstat(path); !os.IsNotExist(statErr) {
		t.Fatalf("the stale socket %q survived the clear (stat err %v)", path, statErr)
	}
}

func TestClearStaleIsANoOpOnAnAbsentPath(t *testing.T) {
	// Arrange.
	path := filepath.Join(sockDir(t), "never.sock")

	// Act.
	err := ClearStale(nil, path)

	// Assert.
	if err != nil {
		t.Fatalf("ClearStale(%q) = %v, want nil", path, err)
	}
}

func TestClearStaleRefusesALiveListener(t *testing.T) {
	// Arrange.
	path := filepath.Join(sockDir(t), "live.sock")
	listening(t, path)

	// Act.
	err := ClearStale(nil, path)

	// Assert.
	if err == nil {
		t.Fatalf("ClearStale(%q) = nil, want a refusal: unlinking a live shim's socket strands it", path)
	}
	if _, statErr := os.Lstat(path); statErr != nil {
		t.Fatalf("the live socket %q was unlinked anyway: %v", path, statErr)
	}
}

func TestClearStaleRefusesAnUndeterminedPath(t *testing.T) {
	// Arrange.
	path := filepath.Join(sockDir(t), "regular.sock")
	if err := os.WriteFile(path, []byte("not a socket"), 0o600); err != nil {
		t.Fatalf("write %q: %v", path, err)
	}

	// Act.
	err := ClearStale(nil, path)

	// Assert.
	if err == nil {
		t.Fatalf("ClearStale(%q) = nil, want a refusal for a path that is not a socket", path)
	}
	if _, statErr := os.Lstat(path); statErr != nil {
		t.Fatalf("the non-socket %q was unlinked anyway: %v", path, statErr)
	}
}
