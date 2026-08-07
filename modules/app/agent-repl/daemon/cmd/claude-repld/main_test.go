package main

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/server"
)

type testFailingWriter struct{}

func (testFailingWriter) Write([]byte) (int, error) { return 0, errors.New("disk full") }

func TestWebappHandlerEmptyDirReturnsNil(t *testing.T) {
	if webappHandler("", func(string, ...any) {}) != nil {
		t.Fatal("expected nil handler when -webapp is empty")
	}
}

func TestHealthzRequiresExplicitReadiness(t *testing.T) {
	ready := &daemonReadiness{}
	h := healthzHandler(ready, t.Logf)

	// Before all listeners/dependencies are live, health must reject rather
	// than treating process existence as readiness.
	first := httptest.NewRecorder()
	h.ServeHTTP(first, httptest.NewRequest(http.MethodGet, "/healthz", nil))
	if first.Code != http.StatusServiceUnavailable {
		t.Fatalf("unready /healthz status=%d, want 503", first.Code)
	}

	ready.ready.Store(true)
	second := httptest.NewRecorder()
	h.ServeHTTP(second, httptest.NewRequest(http.MethodGet, "/healthz", nil))
	if second.Code != http.StatusNoContent {
		t.Fatalf("ready /healthz status=%d, want 204", second.Code)
	}
}

func TestBootFatalLineIsCanonicalJSON(t *testing.T) {
	var record dlog.Record
	if err := json.Unmarshal(bytes.TrimSpace(bootFatalLine("state root unavailable")), &record); err != nil {
		t.Fatalf("bootstrap emergency is not JSON: %v", err)
	}
	if record.Runtime != dlog.RuntimeDaemon || record.Level != dlog.LevelError ||
		record.Operation != "daemon.bootstrap.fatal" || record.Message != "state root unavailable" ||
		record.PID <= 0 || record.Context == nil {
		t.Fatalf("bootstrap emergency=%#v", record)
	}
}

func TestLogDaemonProcessExitLogsCleanExit(t *testing.T) {
	// Arrange
	var durable bytes.Buffer
	logger := dlog.New(&durable, io.Discard, false)

	// Act
	logDaemonProcessExit(logger)

	// Assert
	var record dlog.Record
	if err := json.Unmarshal(bytes.TrimSpace(durable.Bytes()), &record); err != nil {
		t.Fatalf("exit trace is not JSON: %v: %q", err, durable.String())
	}
	if record.Operation != "exit" || record.Message != "claude-repld exiting cleanly" {
		t.Fatalf("exit trace = %#v, want operation=exit clean message", record)
	}
}

// TestLogDaemonProcessExitLogsThenRepanics proves the exit trace narrates a
// panic without recovering it: logDaemonProcessExit must remain deferred
// directly (not wrapped) for its own recover() to observe the panic, so this
// drives it through a real deferred panic rather than calling it directly.
func TestLogDaemonProcessExitLogsThenRepanics(t *testing.T) {
	// Arrange
	var durable bytes.Buffer
	logger := dlog.New(&durable, io.Discard, false)
	var recovered any

	// Act
	func() {
		defer func() { recovered = recover() }()
		func() {
			defer logDaemonProcessExit(logger)
			panic("invariant violated")
		}()
	}()

	// Assert
	if recovered != "invariant violated" {
		t.Fatalf("re-panicked value = %v, want the original panic to survive the trace", recovered)
	}
	var record dlog.Record
	if err := json.Unmarshal(bytes.TrimSpace(durable.Bytes()), &record); err != nil {
		t.Fatalf("panic exit trace is not JSON: %v: %q", err, durable.String())
	}
	if record.Message != "claude-repld exiting: panic: invariant violated" {
		t.Fatalf("panic exit trace = %#v", record)
	}
}

func TestUDSShimLoggerPersistsDaemonOwnedDiagnosticsToWorkspaceTarget(t *testing.T) {
	workspace := dlog.Workspace{Directory: t.TempDir(), ID: "ws-test"}
	manager := dlog.NewTargetManager()
	target, err := manager.OpenWorkspace(workspace)
	if err != nil {
		t.Fatal(err)
	}
	defer manager.Close()
	var terminal bytes.Buffer
	logger := &udsShimLogger{workspace: workspace, daemon: dlog.New(target, &terminal, true), terminal: &terminal, sessionID: "s1"}
	logger.Log("malformed stderr: %s", "bad")
	logger.LogVerbose("stdout scan: %s", "late")
	logger.MirrorShimRecord(`{"runtime":"shim","verbosity":"normal"}`)
	contents, err := os.ReadFile(target.Name())
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(string(contents), `"operation":"shim.stderr"`) || !strings.Contains(string(contents), `"workspace_id":"ws-test"`) || !strings.Contains(string(contents), "malformed stderr: bad") || !strings.Contains(string(contents), "stdout scan: late") || !strings.Contains(terminal.String(), `{"runtime":"shim"`) {
		t.Fatalf("workspace diagnostic records=%q", contents)
	}
}

func TestUDSShimLoggerReportsWorkspacePersistenceFailureToTerminal(t *testing.T) {
	workspace := dlog.Workspace{Directory: t.TempDir(), ID: "ws-test"}
	var terminal bytes.Buffer
	logger := &udsShimLogger{workspace: workspace, daemon: dlog.New(testFailingWriter{}, &terminal, false), terminal: &terminal, sessionID: "s1"}
	logger.Log("malformed stderr")
	if !strings.Contains(terminal.String(), "workspace shim diagnostic persistence failed") {
		t.Fatalf("terminal=%q", terminal.String())
	}
}

// maintenanceTickWait bounds the rendezvous with a completed maintenance pass.
// It is a FAILURE bound, never a delay: the receive completes the instant the
// pass finishes, and this only decides how long a stalled maintenance loop
// takes to fail the test instead of hanging it.
const maintenanceTickWait = 10 * time.Second

func TestWorkspaceLogMaintenanceTicksAndStopIsIdempotent(t *testing.T) {
	manager, err := dlog.NewTargetManagerWithCap(8)
	if err != nil {
		t.Fatal(err)
	}
	workspace := dlog.Workspace{Directory: t.TempDir(), ID: "ws-test"}
	target, err := manager.OpenWorkspaceRuntime(workspace, dlog.RuntimeShim)
	if err != nil {
		t.Fatal(err)
	}
	before, err := target.Stat()
	if err != nil {
		t.Fatal(err)
	}
	if _, err := target.WriteString("12345678"); err != nil {
		t.Fatal(err)
	}
	var terminal bytes.Buffer
	// THE TICK ANNOUNCES ITSELF. This used to sample the target's size in a
	// sleep loop, which measured the clock rather than the work: the pass runs
	// on its own goroutine, and the injected completion signal is the only
	// thing that can say it FINISHED one.
	ticked := make(chan struct{}, 1)
	stop := startWorkspaceLogMaintenanceAtInterval(
		manager, dlog.New(io.Discard, &terminal, false), &terminal, false, time.Millisecond,
		func() {
			select {
			case ticked <- struct{}{}:
			default:
			}
		},
	)
	select {
	case <-ticked:
	case <-time.After(maintenanceTickWait):
		stop()
		t.Fatalf("no maintenance pass completed within %s; terminal=%q", maintenanceTickWait, terminal.String())
	}
	stop()
	stop()
	info, statErr := target.Stat()
	if statErr != nil {
		t.Fatal(statErr)
	}
	if info.Size() != 0 {
		t.Fatalf("maintenance did not truncate target at cap: size=%d terminal=%q", info.Size(), terminal.String())
	}
	if !os.SameFile(before, info) {
		t.Fatalf("maintenance replaced target inode: before=%v after=%v", before, info)
	}
	if err := manager.Close(); err != nil {
		t.Fatal(err)
	}
}

func TestWorkspaceLogMaintenancePanicsWhenAllReportingChannelsFail(t *testing.T) {
	manager, err := dlog.NewTargetManagerWithCap(8)
	if err != nil {
		t.Fatal(err)
	}
	defer manager.Close()
	workspace := dlog.Workspace{Directory: t.TempDir(), ID: "ws-test"}
	target, err := manager.OpenWorkspaceRuntime(workspace, dlog.RuntimeDaemon)
	if err != nil {
		t.Fatal(err)
	}
	if _, err := target.WriteString("12345678"); err != nil {
		t.Fatal(err)
	}
	link := filepath.Join(workspace.Directory, ".claude", "emacs", "daemon.log")
	if err := os.Remove(link); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(link, []byte("hostile regular file"), 0o600); err != nil {
		t.Fatal(err)
	}
	defer func() {
		recovered := recover()
		if recovered == nil || !strings.Contains(fmt.Sprint(recovered), "workspace log maintenance reporting failed") {
			t.Fatalf("maintenance panic=%v", recovered)
		}
	}()
	maintainWorkspaceLogTargets(manager, dlog.New(io.Discard, io.Discard, false), testFailingWriter{}, false)
}

func TestWebappHandlerServesIndexWhenPresent(t *testing.T) {
	dir := t.TempDir()
	if err := os.WriteFile(filepath.Join(dir, "index.html"), []byte("<!doctype html>SPA"), 0o644); err != nil {
		t.Fatal(err)
	}
	warned := false
	h := webappHandler(dir, func(string, ...any) { warned = true })
	if warned {
		t.Fatal("did not expect a warning when index.html exists")
	}
	rec := httptest.NewRecorder()
	h.ServeHTTP(rec, httptest.NewRequest(http.MethodGet, "/", nil))
	if rec.Code != http.StatusOK {
		t.Fatalf("got status %d, want 200", rec.Code)
	}
	if !strings.Contains(rec.Body.String(), "SPA") {
		t.Fatalf("body %q missing index.html content", rec.Body.String())
	}
}

func TestWebappHandlerDiagnosesMissingIndex(t *testing.T) {
	dir := t.TempDir() // exists, but no index.html
	warned := false
	h := webappHandler(dir, func(string, ...any) { warned = true })
	if !warned {
		t.Fatal("expected a startup warning when index.html is missing")
	}
	rec := httptest.NewRecorder()
	h.ServeHTTP(rec, httptest.NewRequest(http.MethodGet, "/", nil))
	if rec.Code != http.StatusServiceUnavailable {
		t.Fatalf("got status %d, want 503", rec.Code)
	}
	if strings.Contains(rec.Body.String(), "404 page not found") {
		t.Fatal("must not serve the bare Go 404 body")
	}
	if !strings.Contains(rec.Body.String(), "webapp assets not found") {
		t.Fatalf("body %q missing the diagnostic message", rec.Body.String())
	}
}

func TestWebappHandlerSelfCorrectsWhenIndexAppears(t *testing.T) {
	dir := t.TempDir() // starts without index.html
	h := webappHandler(dir, func(string, ...any) {})
	// Assets get built after the daemon started.
	if err := os.WriteFile(filepath.Join(dir, "index.html"), []byte("<!doctype html>LATE"), 0o644); err != nil {
		t.Fatal(err)
	}
	rec := httptest.NewRecorder()
	h.ServeHTTP(rec, httptest.NewRequest(http.MethodGet, "/", nil))
	if rec.Code != http.StatusOK {
		t.Fatalf("got status %d, want 200 after index.html appeared", rec.Code)
	}
	if !strings.Contains(rec.Body.String(), "LATE") {
		t.Fatalf("body %q missing late index.html content", rec.Body.String())
	}
}

func TestWebappHandlerRefusesToCacheTheDocumentButNotItsAssets(t *testing.T) {
	// Arrange — a build whose entry point names one fingerprinted bundle.
	dir := t.TempDir()
	if err := os.WriteFile(filepath.Join(dir, "index.html"), []byte("<!doctype html>"), 0o644); err != nil {
		t.Fatal(err)
	}
	if err := os.Mkdir(filepath.Join(dir, "assets"), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(dir, "assets", "index-abc123.js"), []byte("//"), 0o644); err != nil {
		t.Fatal(err)
	}
	h := webappHandler(dir, func(string, ...any) {})

	for _, tc := range []struct {
		name    string
		path    string
		noStore bool
		reason  string
	}{
		{"root document", "/", true, "the entry point is the only thing naming which build to run"},
		{"explicit document", "/index.html", true, "the same document reached by its own name"},
		{"fingerprinted asset", "/assets/index-abc123.js", false, "the URL addresses exactly one build"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			rec := httptest.NewRecorder()
			h.ServeHTTP(rec, httptest.NewRequest(http.MethodGet, tc.path, nil))

			// Assert
			got := rec.Header().Get("Cache-Control")
			if tc.noStore && !strings.Contains(got, "no-store") {
				t.Fatalf("Cache-Control %q for %s must forbid storing: %s", got, tc.path, tc.reason)
			}
			if !tc.noStore && got != "" {
				t.Fatalf("Cache-Control %q for %s must be left to the client: %s", got, tc.path, tc.reason)
			}
		})
	}
}

func TestLaunchedBinaryMTimeMatchesExecutableStat(t *testing.T) {
	// Arrange — the running test binary IS an executable on disk, so
	// launchedBinaryMTime must report exactly its stat mtime.
	exe, err := os.Executable()
	if err != nil {
		t.Skipf("os.Executable unavailable in this environment: %v", err)
	}
	info, err := os.Stat(exe)
	if err != nil {
		t.Fatalf("stat %q: %v", exe, err)
	}
	// Act
	var durable, terminal bytes.Buffer
	got := launchedBinaryMTime(dlog.New(&durable, &terminal, false))
	// Assert
	if want := info.ModTime().Unix(); got != want {
		t.Fatalf("launchedBinaryMTime() = %d, want %d (mtime of %q)", got, want, exe)
	}
	if got <= 0 {
		t.Fatalf("launchedBinaryMTime() = %d, want a positive Unix mtime", got)
	}
}

func TestParseAccounts(t *testing.T) {
	tests := []struct {
		name    string
		raw     string
		want    []server.Account
		wantErr bool
	}{
		{
			name: "empty flag is an unconfigured roster, not an error",
			raw:  "",
			want: nil,
		},
		{
			name: "one pair",
			raw:  "work=/home/u/.claude-chesscom",
			want: []server.Account{{Label: "work", ConfigDir: "/home/u/.claude-chesscom"}},
		},
		{
			name: "empty dir names the CLI default root",
			raw:  "personal=",
			want: []server.Account{{Label: "personal", ConfigDir: ""}},
		},
		{
			name: "two pairs keep roster order",
			raw:  "personal=,work=/home/u/.claude-chesscom",
			want: []server.Account{
				{Label: "personal", ConfigDir: ""},
				{Label: "work", ConfigDir: "/home/u/.claude-chesscom"},
			},
		},
		{
			name:    "pair without an equals sign is malformed",
			raw:     "personal",
			wantErr: true,
		},
		{
			name:    "empty label is malformed",
			raw:     "=/home/u/.claude",
			wantErr: true,
		},
		{
			name:    "duplicate label is rejected",
			raw:     "work=/a,work=/b",
			wantErr: true,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := parseAccounts(tt.raw)

			// Assert
			if tt.wantErr {
				if err == nil {
					t.Fatalf("parseAccounts(%q) = %v, want error", tt.raw, got)
				}
				return
			}
			if err != nil {
				t.Fatalf("parseAccounts(%q): %v", tt.raw, err)
			}
			if len(got) != len(tt.want) {
				t.Fatalf("parseAccounts(%q) = %v, want %v", tt.raw, got, tt.want)
			}
			for i := range got {
				if got[i] != tt.want[i] {
					t.Errorf("account[%d] = %v, want %v", i, got[i], tt.want[i])
				}
			}
		})
	}
}
