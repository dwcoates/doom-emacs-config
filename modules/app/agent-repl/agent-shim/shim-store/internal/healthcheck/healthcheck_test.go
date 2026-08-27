package healthcheck

import (
	"bytes"
	"encoding/json"
	"io"
	"strings"
	"testing"
	"time"

	"agentrepl/shim-store/internal/logging"
)

func discardLogger() *logging.Logger { return logging.New(io.Discard, io.Discard, false) }

func TestProbeRefusesRatherThanInferringHealthFromTheSocket(t *testing.T) {
	// Arrange: a well-formed request. The old probe would have dialed; the
	// protocol it dialed for is gone, and a socket that merely exists is NOT
	// health.
	config := Config{SocketPath: "/tmp/store.sock", RequestID: "doctor-1", Timeout: time.Second}

	// Act
	result, exitCode := Probe(config, discardLogger())

	// Assert
	if exitCode != ExitClientFailure {
		t.Fatalf("exit = %d, want ExitClientFailure (%d)", exitCode, ExitClientFailure)
	}
	if result.Healthy {
		t.Fatal("the probe reported healthy without a protocol to prove it with")
	}
}

func TestProbeRefusalNamesTheDeletedProtocol(t *testing.T) {
	// Arrange: an operator reading doctor's output must not be sent hunting a
	// socket that is fine.
	config := Config{SocketPath: "/tmp/store.sock", RequestID: "doctor-1", Timeout: time.Second}

	// Act
	result, _ := Probe(config, discardLogger())

	// Assert
	if result.Reason != ProbeUnavailableReason {
		t.Fatalf("reason = %q, want the unavailability account", result.Reason)
	}
	if !strings.Contains(result.Reason, "HealthCheck/HealthStatus") {
		t.Fatalf("reason = %q, want it to name the deleted messages", result.Reason)
	}
}

func TestProbeCorrelatesItsRefusalWithTheRequestID(t *testing.T) {
	// Arrange
	config := Config{SocketPath: "/tmp/store.sock", RequestID: "doctor-42", Timeout: time.Second}

	// Act
	result, _ := Probe(config, discardLogger())

	// Assert
	if result.RequestID != "doctor-42" {
		t.Fatalf("request_id = %q, want the caller's correlation id echoed", result.RequestID)
	}
}

func TestProbeRefusalIsLoggedOnceAtError(t *testing.T) {
	// Arrange
	var logs bytes.Buffer
	log := logging.New(&logs, io.Discard, false)

	// Act
	Probe(Config{SocketPath: "/tmp/store.sock", RequestID: "doctor-1", Timeout: time.Second}, log)

	// Assert
	records := decodeRecords(t, logs.Bytes())
	if len(records) != 1 {
		t.Fatalf("records = %d, want exactly one", len(records))
	}
	if records[0].Operation != "health-check" || records[0].Level != "error" {
		t.Fatalf("record = %+v, want the canonical health-check error", records[0])
	}
}

func TestProbeRejectsAnEmptySocketPath(t *testing.T) {
	// Arrange / Act
	result, exitCode := Probe(Config{RequestID: "doctor-1", Timeout: time.Second}, discardLogger())

	// Assert: usage is still reported as usage rather than masked by the
	// protocol gap.
	if exitCode != ExitUsage || result.FailureClass != "usage" {
		t.Fatalf("(exit, class) = (%d, %q), want usage", exitCode, result.FailureClass)
	}
}

func TestProbeRejectsAnEmptyRequestID(t *testing.T) {
	// Arrange / Act
	result, exitCode := Probe(Config{SocketPath: "/tmp/store.sock", Timeout: time.Second}, discardLogger())

	// Assert
	if exitCode != ExitUsage || result.FailureClass != "usage" {
		t.Fatalf("(exit, class) = (%d, %q), want usage", exitCode, result.FailureClass)
	}
}

func TestProbeRejectsANonPositiveTimeout(t *testing.T) {
	// Arrange / Act
	result, exitCode := Probe(Config{SocketPath: "/tmp/store.sock", RequestID: "doctor-1"}, discardLogger())

	// Assert
	if exitCode != ExitUsage || result.FailureClass != "usage" {
		t.Fatalf("(exit, class) = (%d, %q), want usage", exitCode, result.FailureClass)
	}
}

type record struct {
	Operation string `json:"operation"`
	Level     string `json:"level"`
	Message   string `json:"message"`
}

func decodeRecords(t *testing.T, logs []byte) []record {
	t.Helper()
	var out []record
	for _, line := range bytes.Split(bytes.TrimSpace(logs), []byte("\n")) {
		if len(line) == 0 {
			continue
		}
		var r record
		if err := json.Unmarshal(line, &r); err != nil {
			t.Fatalf("health log line is not JSON: %v (%s)", err, line)
		}
		out = append(out, r)
	}
	return out
}
