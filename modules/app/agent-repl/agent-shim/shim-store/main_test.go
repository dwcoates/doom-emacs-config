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

	"agentrepl/shim-store/internal/logging"
)

func TestOpenLoggerReturnsBootstrapErrorBeforePersistentSinkExists(t *testing.T) {
	parent := t.TempDir()
	blocked := filepath.Join(parent, "blocked")
	if err := os.WriteFile(blocked, []byte("not a directory"), 0o600); err != nil {
		t.Fatal(err)
	}

	_, _, err := openLogger(filepath.Join(parent, "store.sock"), filepath.Join(parent, "events.db"), filepath.Join(blocked, "store.log"))
	if err == nil {
		t.Fatal("openLogger succeeded with a non-directory parent")
	}
	if !isBootstrapError(err) {
		t.Fatalf("error %T = %v, want bootstrap error", err, err)
	}
	var stderr bytes.Buffer
	reportFatal(err, &stderr)
	var record map[string]any
	if decodeErr := json.Unmarshal(stderr.Bytes(), &record); decodeErr != nil {
		t.Fatalf("bootstrap failure is not JSON: %v: %q", decodeErr, stderr.String())
	}
	if record["operation"] != "store.bootstrap" || record["level"] != "error" {
		t.Fatalf("bootstrap failure record = %#v", record)
	}
	stderr.Reset()
	reportFatal(errors.New("runtime failure"), &stderr)
	if stderr.Len() != 0 {
		t.Fatalf("post-bootstrap failure bypassed canonical logger: %q", stderr.String())
	}
}

func TestRunHealthCheckAlwaysWritesOneResultForMissingSocket(t *testing.T) {
	root := t.TempDir()
	var stdout, stderr bytes.Buffer
	exitCode := runHealthCheck(filepath.Join(root, "missing.sock"), filepath.Join(root, "shim-store.log"), "doctor-123", time.Second, &stdout, &stderr)
	if exitCode != 10 {
		t.Fatalf("runHealthCheck exit = %d, want 10", exitCode)
	}
	if strings.Count(stdout.String(), "\n") != 1 {
		t.Fatalf("stdout must contain exactly one JSON object: %q", stdout.String())
	}
	var result struct {
		RequestID    string `json:"request_id"`
		FailureClass string `json:"failure_class"`
		Healthy      bool   `json:"healthy"`
	}
	if err := json.Unmarshal(stdout.Bytes(), &result); err != nil {
		t.Fatalf("stdout is not Result JSON: %v: %q", err, stdout.String())
	}
	if result.RequestID != "doctor-123" || result.FailureClass != "missing_socket" || result.Healthy {
		t.Fatalf("Result = %+v, want correlated missing_socket failure", result)
	}
}

func TestRunHealthCheckWritesResultWhenLoggerBootstrapFails(t *testing.T) {
	root := t.TempDir()
	blocked := filepath.Join(root, "blocked")
	if err := os.WriteFile(blocked, []byte("not a directory"), 0o600); err != nil {
		t.Fatal(err)
	}
	var stdout, stderr bytes.Buffer
	exitCode := runHealthCheck(filepath.Join(root, "missing.sock"), filepath.Join(blocked, "shim-store.log"), "doctor-123", time.Second, &stdout, &stderr)
	if exitCode != 17 {
		t.Fatalf("runHealthCheck exit = %d, want 17", exitCode)
	}
	var result struct {
		RequestID    string `json:"request_id"`
		FailureClass string `json:"failure_class"`
	}
	if err := json.Unmarshal(stdout.Bytes(), &result); err != nil {
		t.Fatalf("logger bootstrap failure omitted Result JSON: %v: %q", err, stdout.String())
	}
	if result.RequestID != "doctor-123" || result.FailureClass != "client_failure" {
		t.Fatalf("Result = %+v, want correlated client_failure", result)
	}
}

func TestLogProcessExitNamesCleanOrErrorExit(t *testing.T) {
	// Arrange
	for _, tc := range []struct {
		name        string
		err         error
		wantLevel   string
		wantMessage string
	}{
		{name: "clean", err: nil, wantLevel: "info", wantMessage: "shim-store exiting cleanly"},
		{name: "error", err: errors.New("accept failed"), wantLevel: "error", wantMessage: "shim-store exiting: accept failed"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			var file, stderr bytes.Buffer
			log := logging.New(&file, &stderr, false).With(logging.Fields{Component: "store"})

			// Act
			logProcessExit(log, &tc.err)

			// Assert
			var record struct {
				Level     string `json:"level"`
				Operation string `json:"operation"`
				Message   string `json:"message"`
			}
			if err := json.Unmarshal(file.Bytes(), &record); err != nil {
				t.Fatalf("exit trace is not JSON: %v: %q", err, file.String())
			}
			if record.Operation != "exit" || record.Level != tc.wantLevel || record.Message != tc.wantMessage {
				t.Fatalf("exit trace = %#v, want operation=exit level=%q message=%q", record, tc.wantLevel, tc.wantMessage)
			}
		})
	}
}

// TestLogProcessExitLogsThenRepanics proves the exit trace narrates a panic
// without recovering it: logProcessExit must remain deferred directly (not
// wrapped) for its own recover() to observe the panic, so this drives it
// through a real deferred panic rather than calling it as a plain function.
func TestLogProcessExitLogsThenRepanics(t *testing.T) {
	// Arrange
	var file, stderr bytes.Buffer
	log := logging.New(&file, &stderr, false).With(logging.Fields{Component: "store"})
	var recovered any

	// Act
	func() {
		defer func() { recovered = recover() }()
		func() {
			var err error
			defer logProcessExit(log, &err)
			panic("invariant violated")
		}()
	}()

	// Assert
	if recovered != "invariant violated" {
		t.Fatalf("re-panicked value = %v, want the original panic to survive the trace", recovered)
	}
	var record struct {
		Level   string `json:"level"`
		Message string `json:"message"`
	}
	if err := json.Unmarshal(file.Bytes(), &record); err != nil {
		t.Fatalf("panic exit trace is not JSON: %v: %q", err, file.String())
	}
	if record.Level != "error" || record.Message != "shim-store exiting: panic: invariant violated" {
		t.Fatalf("panic exit trace = %#v", record)
	}
}

func TestRunLoggedRecordsPostBootstrapErrorExactlyOnce(t *testing.T) {
	var file, stderr bytes.Buffer
	log := logging.New(&file, &stderr, false).With(logging.Fields{
		Component:    "store",
		DatabasePath: "/tmp/events.db",
		Socket:       "/tmp/store.sock",
	})
	want := errors.New("accept failed")

	err := runLogged(log, "serve", func() error { return want })
	if !errors.Is(err, want) {
		t.Fatalf("runLogged error = %v, want %v", err, want)
	}
	for sink, got := range map[string]string{"file": file.String(), "stderr": stderr.String()} {
		if count := strings.Count(got, "runtime operation failed: accept failed"); count != 1 {
			t.Fatalf("%s error record count = %d, output=%q", sink, count, got)
		}
		var record struct {
			Operation string         `json:"operation"`
			Level     string         `json:"level"`
			Context   map[string]any `json:"context"`
		}
		if err := json.Unmarshal([]byte(got), &record); err != nil {
			t.Fatalf("%s record is not JSON: %v", sink, err)
		}
		if record.Operation != "serve" || record.Context["db"] != "/tmp/events.db" || record.Context["socket"] != "/tmp/store.sock" {
			t.Fatalf("%s missing canonical context: %#v", sink, record)
		}
		// Both callers end the process; an omitted level would persist as info
		// and hide a fatal serve failure from every warning sweep.
		if record.Level != "error" {
			t.Fatalf("%s runtime-failure level = %q, want error", sink, record.Level)
		}
	}
}
