package main

import (
	"bytes"
	"net"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	sharedlogging "agentrepl/logging"
	"agentrepl/logging/recordlog"
)

func TestTheCacheDirFollowsXDGWhenItIsSet(t *testing.T) {
	tests := []struct {
		name string
		xdg  string
		want string
	}{
		{name: "the home's cache by default", want: filepath.Join("/home/u", ".cache", "agent-repl")},
		{name: "XDG's cache when set", xdg: "/xdg", want: filepath.Join("/xdg", "agent-repl")},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			t.Setenv("XDG_CACHE_HOME", tt.xdg)

			// Act
			got := cacheDir("/home/u")

			// Assert
			if got != tt.want {
				t.Fatalf("cacheDir = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestTheSocketEnvironmentOverridesTheDefault(t *testing.T) {
	tests := []struct {
		name string
		env  string
		want string
	}{
		{name: "the default when unset", want: "/default.sock"},
		{name: "the environment when set", env: "/private.sock", want: "/private.sock"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			t.Setenv("AGENT_REPL_TESTS_SOCKET", tt.env)

			// Act
			got := envOr("AGENT_REPL_TESTS_SOCKET", "/default.sock")

			// Assert
			if got != tt.want {
				t.Fatalf("envOr = %q, want %q", got, tt.want)
			}
		})
	}
}

// testLog is a debug-level record log written to BUF.
func testLog(buf *bytes.Buffer) *recordlog.Logger {
	return recordlog.New(Runtime, buf, &bytes.Buffer{}, sharedlogging.FixedWindow(sharedlogging.LevelDebug), time.Now)
}

func TestTheServiceServesOnAnInheritedListener(t *testing.T) {
	// Arrange: a listener bound as a harness binds it, handed over by fd.
	dir, err := os.MkdirTemp("/tmp", "tests-fd-")
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { _ = os.RemoveAll(dir) })
	path := filepath.Join(dir, "s.sock")
	bound, err := net.Listen("unix", path)
	if err != nil {
		t.Fatal(err)
	}
	unix := bound.(*net.UnixListener)
	file, err := unix.File()
	if err != nil {
		t.Fatal(err)
	}
	// The handed-over copy serves the path, so closing this one leaves it.
	unix.SetUnlinkOnClose(false)
	_ = unix.Close()
	var buf bytes.Buffer

	// Act.
	ln, err := listen("/unused.sock", int(file.Fd()), testLog(&buf))

	// Assert: a dial reaches the handed-over listener.
	if err != nil {
		t.Fatalf("listen: %v", err)
	}
	t.Cleanup(func() { _ = ln.Close() })
	conn, err := net.Dial("unix", path)
	if err != nil {
		t.Fatalf("dialing the inherited listener: %v", err)
	}
	_ = conn.Close()
	if !strings.Contains(buf.String(), "serving on the inherited listener") {
		t.Fatalf("log = %s, want the inherited listener recorded", buf.String())
	}
}

func TestAnInheritedFdThatIsNoListenerIsRecordedAndRefused(t *testing.T) {
	// Arrange: an fd naming a plain file.
	file, err := os.CreateTemp(t.TempDir(), "not-a-socket")
	if err != nil {
		t.Fatal(err)
	}
	var buf bytes.Buffer

	// Act.
	_, err = listen("/unused.sock", int(file.Fd()), testLog(&buf))

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "--listen-fd") {
		t.Fatalf("listen = %v, want the fd refused", err)
	}
	if !strings.Contains(buf.String(), `"level":"error"`) || !strings.Contains(buf.String(), "the inherited listener could not be opened") {
		t.Fatalf("log = %s, want the refusal recorded at error", buf.String())
	}
}
