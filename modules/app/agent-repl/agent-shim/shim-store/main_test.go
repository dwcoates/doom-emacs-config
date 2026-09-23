package main

import (
	"agentrepl/shim-store/internal/testclose"
	"bytes"
	"encoding/json"
	"errors"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"agentrepl/shim-store/internal/logging"
	"agentrepl/shim-store/internal/pprofsurface"
	"agentrepl/shim-store/internal/server"
)

func TestMain(m *testing.M) {
	// Nothing in this process reaches a vendor, and the suite states so rather
	// than relying on that remaining true.
	if err := os.Setenv("AGENT_REPL_FORBID_VENDOR_CALLS", "1"); err != nil {
		panic(err)
	}
	os.Exit(m.Run())
}

// storeRecords decodes the canonical JSONL a logger wrote.
func storeRecords(t *testing.T, sink *bytes.Buffer) []storeLogRecord {
	t.Helper()
	return decodeStoreRecords(t, sink)
}

func hasOperation(records []storeLogRecord, operation string) bool {
	for _, rec := range records {
		if rec.Operation == operation {
			return true
		}
	}
	return false
}

func TestSocketDefaultFallsBackToTheCacheDir(t *testing.T) {
	// Arrange.
	t.Setenv(server.EnvSocket, "")

	// Act.
	got := socketDefault("/cache/agent-repl")

	// Assert.
	if want := filepath.Join("/cache/agent-repl", "sock", "store.sock"); got != want {
		t.Fatalf("socketDefault = %q, want %q", got, want)
	}
}

func TestSocketDefaultPrefersTheEnvironment(t *testing.T) {
	// Arrange. A test harness points every participant at a private store
	// through this variable rather than editing command lines.
	t.Setenv(server.EnvSocket, "/private/store.sock")

	// Act.
	got := socketDefault("/cache/agent-repl")

	// Assert.
	if got != "/private/store.sock" {
		t.Fatalf("socketDefault = %q, want the environment's value", got)
	}
}

func TestDefaultCacheDirHonorsXdgCacheHome(t *testing.T) {
	// Arrange.
	t.Setenv("XDG_CACHE_HOME", "/xdg")

	// Act.
	got := defaultCacheDir()

	// Assert.
	if want := filepath.Join("/xdg", "agent-repl"); got != want {
		t.Fatalf("defaultCacheDir = %q, want %q", got, want)
	}
}

func TestRunWithLoggerOpensThePprofSurfaceBeforeTheDatabase(t *testing.T) {
	// Arrange. The database path is a DIRECTORY, so db.Open must fail — which
	// makes the profiling record's presence proof that the surface was already
	// bound when the wedged open happened.
	root := t.TempDir()
	unopenable := filepath.Join(root, "events.db")
	if err := os.Mkdir(unopenable, 0o755); err != nil {
		t.Fatalf("stage an unopenable database: %v", err)
	}
	sink := &bytes.Buffer{}
	log := logging.New(sink, &bytes.Buffer{}, true)

	// Act.
	err := runWithLogger(shortSocketPath(t), unopenable, storePprofSock(t), 0, log)

	// Assert.
	if err == nil {
		t.Fatal("runWithLogger = nil, want the database open to fail")
	}
	if !hasOperation(storeRecords(t, sink), "store.pprof.enabled") {
		t.Fatalf("records = %+v, want the pprof surface recorded before the database failed", storeRecords(t, sink))
	}
}

func TestRunWithLoggerRefusesAnUnsafePprofAddressBeforeTouchingTheDatabase(t *testing.T) {
	// Arrange. A wildcard bind would publish the store's stacks and heap.
	root := t.TempDir()
	sink := &bytes.Buffer{}
	log := logging.New(sink, &bytes.Buffer{}, true)

	// Act.
	err := runWithLogger(shortSocketPath(t), filepath.Join(root, "events.db"), "0.0.0.0:6061", 0, log)

	// Assert.
	if err == nil {
		t.Fatal("runWithLogger = nil, want the wildcard bind refused")
	}
	if _, statErr := os.Stat(filepath.Join(root, "events.db")); statErr == nil {
		t.Fatal("the database was created despite the refused profiling surface")
	}
}

func TestOpenLoggerReturnsABootstrapErrorBeforeThePersistentSinkExists(t *testing.T) {
	// Arrange.
	t.Setenv("AGENT_REPL_LOG_LEVEL", "info")
	parent := t.TempDir()
	blocked := filepath.Join(parent, "blocked")
	if err := os.WriteFile(blocked, []byte("not a directory"), 0o600); err != nil {
		t.Fatalf("stage a non-directory: %v", err)
	}

	// Act.
	_, _, err := openLogger(filepath.Join(parent, "store.sock"), filepath.Join(parent, "events.db"), filepath.Join(blocked, "store.log"))

	// Assert.
	if err == nil {
		t.Fatal("openLogger succeeded with a non-directory parent")
	}
	if !isBootstrapError(err) {
		t.Fatalf("error %T = %v, want a bootstrap error", err, err)
	}
}

func TestOpenLoggerRejectsAnInvalidLogLevelBeforeCreatingTheSink(t *testing.T) {
	// Arrange.
	t.Setenv("AGENT_REPL_LOG_LEVEL", "verbose")
	root := t.TempDir()
	logPath := filepath.Join(root, "log", "shim-store.log")

	// Act.
	_, _, err := openLogger(filepath.Join(root, "store.sock"), filepath.Join(root, "events.db"), logPath)

	// Assert.
	if err == nil {
		t.Fatal("openLogger succeeded with an invalid AGENT_REPL_LOG_LEVEL")
	}
	if !isBootstrapError(err) {
		t.Fatalf("error %T = %v, want a bootstrap error", err, err)
	}
	if _, statErr := os.Stat(filepath.Dir(logPath)); !os.IsNotExist(statErr) {
		t.Fatalf("log directory stat = %v, want no state created", statErr)
	}
}

func TestOpenLoggerCreatesTheDurableSink(t *testing.T) {
	// Arrange.
	t.Setenv("AGENT_REPL_LOG_LEVEL", "info")
	root := t.TempDir()
	logPath := filepath.Join(root, "log", "shim-store.log")

	// Act.
	log, closeLog, err := openLogger(filepath.Join(root, "sock", "store.sock"), filepath.Join(root, "store", "events.db"), logPath)
	if err != nil {
		t.Fatalf("openLogger = %v, want nil", err)
	}
	defer closeLog()
	log.Log(logging.Fields{Operation: "test"}, "hello")

	// Assert.
	body, err := os.ReadFile(logPath)
	if err != nil {
		t.Fatalf("read the log: %v", err)
	}
	if !strings.Contains(string(body), `"operation":"test"`) {
		t.Fatalf("log = %q, want the record persisted", body)
	}
}

func TestOpenLoggerLeavesTheDatabaseDirectoryToTheDatabase(t *testing.T) {
	// Arrange. Creating the --db parent here would make an unopenable database
	// a BOOTSTRAP failure, ahead of the profiling surface that exists to make
	// exactly that failure diagnosable.
	t.Setenv("AGENT_REPL_LOG_LEVEL", "info")
	root := t.TempDir()
	dbPath := filepath.Join(root, "store", "events.db")

	// Act.
	_, closeLog, err := openLogger(filepath.Join(root, "sock", "store.sock"), dbPath, filepath.Join(root, "log", "shim-store.log"))
	if err != nil {
		t.Fatalf("openLogger = %v, want nil", err)
	}
	defer closeLog()

	// Assert.
	if _, statErr := os.Stat(filepath.Dir(dbPath)); !os.IsNotExist(statErr) {
		t.Fatalf("stat %q = %v, want the database directory left uncreated", filepath.Dir(dbPath), statErr)
	}
}

func TestHoldPprofForDiagnosisReturnsWhenTheFailedBootIsProfiled(t *testing.T) {
	// Arrange. The surface outlives the boot failure it exists to explain.
	sink := &bytes.Buffer{}
	log := logging.New(sink, &bytes.Buffer{}, true)
	surface, err := pprofsurface.Open("127.0.0.1:0")
	if err != nil {
		t.Fatalf("opening the surface: %v", err)
	}
	served := make(chan error, 1)
	go func() { served <- surface.Serve() }()
	defer func() {
		if closeErr := surface.Close(); closeErr != nil {
			t.Errorf("close surface: %v", closeErr)
		}
		if serveErr := <-served; serveErr != nil && !errors.Is(serveErr, http.ErrServerClosed) {
			t.Errorf("serve: %v", serveErr)
		}
	}()

	// Act. The profile request is the signal; nothing waits on elapsed time.
	response, err := http.Get("http://" + surface.Address() + pprofsurface.Path)
	if err != nil {
		t.Fatalf("GET the profiling index: %v", err)
	}
	testclose.OrFail(t, response.Body)
	holdPprofForDiagnosis(surface, log)

	// Assert.
	if !strings.Contains(sink.String(), "the failed boot was profiled") {
		t.Fatalf("log = %q, want the profiled-then-exiting record", sink.String())
	}
}

func TestHoldPprofForDiagnosisHoldsNothingWhenTheSurfaceIsOff(t *testing.T) {
	// Arrange. Off is the shipped state, and an ordinary failed boot may not
	// pay a grace for a surface nobody asked for.
	sink := &bytes.Buffer{}
	log := logging.New(sink, &bytes.Buffer{}, true)

	// Act.
	holdPprofForDiagnosis(nil, log)

	// Assert.
	if strings.Contains(sink.String(), "holding the profiling surface open") {
		t.Fatalf("log = %q, want no hold recorded", sink.String())
	}
}

func TestReportFatalWritesABootstrapFailureAsJSON(t *testing.T) {
	// Arrange. This is the one path that may report before the logger exists.
	var stderr bytes.Buffer

	// Act.
	reportFatal(bootstrapError{errors.New("opening log: permission denied")}, &stderr)

	// Assert.
	var record map[string]any
	if err := json.Unmarshal(stderr.Bytes(), &record); err != nil {
		t.Fatalf("bootstrap failure is not JSON: %v: %q", err, stderr.String())
	}
	if record["operation"] != "store.bootstrap" || record["level"] != "error" {
		t.Fatalf("record = %#v, want the store.bootstrap error record", record)
	}
}

func TestReportFatalStaysSilentForAPostBootstrapFailure(t *testing.T) {
	// Arrange. Everything after bootstrap already reached the canonical log.
	var stderr bytes.Buffer

	// Act.
	reportFatal(errors.New("runtime failure"), &stderr)

	// Assert.
	if stderr.Len() != 0 {
		t.Fatalf("stderr = %q, want nothing: the canonical logger owns this failure", stderr.String())
	}
}

func TestLogProcessExitNamesACleanExit(t *testing.T) {
	// Arrange.
	var file, stderr bytes.Buffer
	log := logging.New(&file, &stderr, false).With(logging.Fields{Component: "store"})
	var err error

	// Act.
	logProcessExit(log, &err)

	// Assert.
	record := decodeStoreRecords(t, &file)[0]
	if record.Operation != "exit" || record.Level != "info" || record.Message != "shim-store exiting cleanly" {
		t.Fatalf("record = %#v, want a clean exit trace", record)
	}
}

func TestLogProcessExitNamesAFailedExit(t *testing.T) {
	// Arrange.
	var file, stderr bytes.Buffer
	log := logging.New(&file, &stderr, false).With(logging.Fields{Component: "store"})
	err := errors.New("accept failed")

	// Act.
	logProcessExit(log, &err)

	// Assert.
	record := decodeStoreRecords(t, &file)[0]
	if record.Level != "error" || record.Message != "shim-store exiting: accept failed" {
		t.Fatalf("record = %#v, want the failure named in the exit trace", record)
	}
}

// TestLogProcessExitLogsThenRepanics proves the exit trace narrates a panic
// without recovering it: logProcessExit must stay deferred directly (not
// wrapped) for its own recover() to observe the panic, so this drives it
// through a real deferred panic rather than a plain call.
func TestLogProcessExitLogsThenRepanics(t *testing.T) {
	// Arrange.
	var file, stderr bytes.Buffer
	log := logging.New(&file, &stderr, false).With(logging.Fields{Component: "store"})
	var recovered any

	// Act.
	func() {
		defer func() { recovered = recover() }()
		func() {
			var err error
			defer logProcessExit(log, &err)
			panic("invariant violated")
		}()
	}()

	// Assert.
	if recovered != "invariant violated" {
		t.Fatalf("re-panicked value = %v, want the original panic to survive the trace", recovered)
	}
	record := decodeStoreRecords(t, &file)[0]
	if record.Level != "error" || record.Message != "shim-store exiting: panic: invariant violated" {
		t.Fatalf("record = %#v, want the panic narrated", record)
	}
}

func TestJoinCloseLeavesACleanRunCleanWhenTheCloseSucceeds(t *testing.T) {
	// Arrange.
	var err error

	// Act.
	joinClose(&err, func() error { return nil })

	// Assert.
	if err != nil {
		t.Fatalf("err = %v, want nil", err)
	}
}

func TestJoinCloseFailsACleanRunWhoseCloseFailed(t *testing.T) {
	// Arrange.
	var err error
	closeErr := errors.New("the close failed")

	// Act.
	joinClose(&err, func() error { return closeErr })

	// Assert.
	if !errors.Is(err, closeErr) {
		t.Fatalf("err = %v, want the close failure", err)
	}
}

func TestJoinCloseKeepsTheRunsOwnFailureAlongsideAFailedClose(t *testing.T) {
	// Arrange.
	runErr := errors.New("the run failed")
	err := runErr
	closeErr := errors.New("the close failed")

	// Act.
	joinClose(&err, func() error { return closeErr })

	// Assert.
	if !errors.Is(err, runErr) || !errors.Is(err, closeErr) {
		t.Fatalf("err = %v, want both the run's failure and the close failure", err)
	}
}
