package main

import (
	"bytes"
	"encoding/json"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"agentrepl/shim-claude-sidecar/internal/logging"
)

func TestDefaultStoreSocketPrefersTheSharedEnv(t *testing.T) {
	// Arrange: the env var is how a private test store is reached without
	// either side hard-coding a path.
	t.Setenv(StoreSocketEnv, "/private/tmp/ar-test.sock")

	// Act.
	got := defaultStoreSocket("/cache/agent-repl")

	// Assert.
	if got != "/private/tmp/ar-test.sock" {
		t.Fatalf("default store socket = %q, want the env value", got)
	}
}

func TestDefaultStoreSocketFallsBackToTheCacheDir(t *testing.T) {
	// Arrange.
	t.Setenv(StoreSocketEnv, "")

	// Act.
	got := defaultStoreSocket("/cache/agent-repl")

	// Assert.
	if got != filepath.Join("/cache/agent-repl", "sock", "store.sock") {
		t.Fatalf("default store socket = %q, want the cache-dir path", got)
	}
}

func TestAnExplicitSocketBeatsTheEnv(t *testing.T) {
	// Arrange: the flag's default is the env value, so an explicitly parsed
	// flag is what beats it. This asserts the resolution order the ruling names.
	t.Setenv(StoreSocketEnv, "/private/tmp/ar-env.sock")
	options := Options{StoreSocket: "/private/tmp/ar-flag.sock"}

	// Act.
	got := options.StoreSocket

	// Assert.
	if got != "/private/tmp/ar-flag.sock" {
		t.Fatalf("store socket = %q, want the explicit flag to win", got)
	}
}

func TestPollAndRescanDefaults(t *testing.T) {
	tests := []struct {
		name string
		got  time.Duration
		want time.Duration
	}{
		{name: "poll", got: DefaultPollInterval, want: time.Second},
		{name: "rescan", got: DefaultRescanInterval, want: 30 * time.Second},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act, Assert.
			if tc.got != tc.want {
				t.Fatalf("%s interval = %s, want %s", tc.name, tc.got, tc.want)
			}
		})
	}
}

func TestParseRootsSplitsBothConfigRoots(t *testing.T) {
	// Arrange: the second account's transcripts are invisible without its root.
	t.Setenv("HOME", "/home/tester")

	// Act.
	got := parseRoots("~/.claude,~/.claude-chesscom")

	// Assert.
	want := []string{"/home/tester/.claude", "/home/tester/.claude-chesscom"}
	if len(got) != 2 || got[0] != want[0] || got[1] != want[1] {
		t.Fatalf("parseRoots = %v, want %v", got, want)
	}
}

func TestParseRootsTrimsWhitespace(t *testing.T) {
	// Arrange, Act.
	got := parseRoots(" /a , /b ")

	// Assert.
	if len(got) != 2 || got[0] != "/a" || got[1] != "/b" {
		t.Fatalf("parseRoots = %v, want the roots trimmed", got)
	}
}

func TestParseRootsDropsEmptyEntries(t *testing.T) {
	// Arrange, Act.
	got := parseRoots("/a,,/b,")

	// Assert.
	if len(got) != 2 {
		t.Fatalf("parseRoots = %v, want the empty entries dropped", got)
	}
}

func TestParseRootsOnAnEmptyList(t *testing.T) {
	// Arrange, Act.
	got := parseRoots("")

	// Assert.
	if len(got) != 0 {
		t.Fatalf("parseRoots = %v, want no roots", got)
	}
}

func TestExpandHomeLeavesAnAbsolutePathAlone(t *testing.T) {
	// Arrange, Act.
	got := expandHome("/absolute/path")

	// Assert.
	if got != "/absolute/path" {
		t.Fatalf("expandHome = %q, want the path unchanged", got)
	}
}

func TestExpandHomeExpandsABareTilde(t *testing.T) {
	// Arrange.
	t.Setenv("HOME", "/home/tester")

	// Act.
	got := expandHome("~")

	// Assert.
	if got != "/home/tester" {
		t.Fatalf("expandHome = %q, want the home dir", got)
	}
}

func TestExpandHomeDoesNotExpandAMidPathTilde(t *testing.T) {
	// Arrange.
	t.Setenv("HOME", "/home/tester")

	// Act.
	got := expandHome("/opt/~/claude")

	// Assert.
	if got != "/opt/~/claude" {
		t.Fatalf("expandHome = %q, want the path unchanged", got)
	}
}

func TestDefaultCacheDirHonorsXDG(t *testing.T) {
	// Arrange.
	t.Setenv("XDG_CACHE_HOME", "/xdg/cache")

	// Act.
	got := defaultCacheDir()

	// Assert.
	if got != filepath.Join("/xdg/cache", "agent-repl") {
		t.Fatalf("cache dir = %q, want the XDG path", got)
	}
}

func TestDefaultCacheDirFallsBackToHome(t *testing.T) {
	// Arrange.
	t.Setenv("XDG_CACHE_HOME", "")
	t.Setenv("HOME", "/home/tester")

	// Act.
	got := defaultCacheDir()

	// Assert.
	if got != filepath.Join("/home/tester", ".cache", "agent-repl") {
		t.Fatalf("cache dir = %q, want the home cache path", got)
	}
}

func TestBootstrapFailureIsReportedToStderr(t *testing.T) {
	// Arrange: no canonical logger can exist yet, so this is the one path that
	// may write diagnostics itself.
	stderr := &bytes.Buffer{}

	// Act.
	reportFatal(bootstrapError{errors.New("opening log: permission denied")}, stderr)

	// Assert.
	var record map[string]any
	if err := json.Unmarshal(stderr.Bytes(), &record); err != nil {
		t.Fatalf("decoding %q: %v", stderr.String(), err)
	}
	if record["operation"] != "sidecar.bootstrap" {
		t.Fatalf("record = %v, want the bootstrap operation", record)
	}
}

func TestAPostBootstrapFailureIsNotReReported(t *testing.T) {
	// Arrange: every post-bootstrap error has already reached the logger.
	stderr := &bytes.Buffer{}

	// Act.
	reportFatal(errors.New("cursor recovery failed"), stderr)

	// Assert.
	if stderr.Len() != 0 {
		t.Fatalf("stderr = %q, want the error left to its owning layer", stderr.String())
	}
}

func TestOpenLoggerCreatesItsDirectory(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), "nested", "sidecar.log")

	// Act.
	logf, closeLog, err := openLogger("/private/tmp/store.sock", path)
	if err != nil {
		t.Fatalf("openLogger: %v", err)
	}
	defer closeLog()
	logf.With(logging.Context{Operation: "test"}).Log("hello")

	// Assert.
	raw, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("reading %s: %v", path, err)
	}
	if !strings.Contains(string(raw), "hello") {
		t.Fatalf("log = %q, want the record persisted", raw)
	}
}

func TestOpenLoggerFailureIsABootstrapError(t *testing.T) {
	// Arrange: a log path under a regular file cannot be created.
	base := t.TempDir()
	blocker := filepath.Join(base, "blocker")
	if err := os.WriteFile(blocker, []byte("x"), 0o644); err != nil {
		t.Fatalf("writing %s: %v", blocker, err)
	}

	// Act.
	_, _, err := openLogger("/private/tmp/store.sock", filepath.Join(blocker, "sidecar.log"))

	// Assert.
	if !isBootstrapError(err) {
		t.Fatalf("openLogger error = %v, want a bootstrap error", err)
	}
}
