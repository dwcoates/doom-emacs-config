package main

import (
	"bytes"
	"encoding/json"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"

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
			Context   map[string]any `json:"context"`
		}
		if err := json.Unmarshal([]byte(got), &record); err != nil {
			t.Fatalf("%s record is not JSON: %v", sink, err)
		}
		if record.Operation != "serve" || record.Context["db"] != "/tmp/events.db" || record.Context["socket"] != "/tmp/store.sock" {
			t.Fatalf("%s missing canonical context: %#v", sink, record)
		}
	}
}
