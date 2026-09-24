package main

import (
	"bytes"
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"

	sharedlogging "agentrepl/logging"
	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/shim-claude-sidecar/internal/testclose"
)

// sidecarPprofSock keeps a socket path inside the platform's sun_path limit.
func sidecarPprofSock(t *testing.T) string {
	t.Helper()
	dir, err := os.MkdirTemp("", "cp")
	if err != nil {
		t.Fatalf("mkdtemp: %v", err)
	}
	t.Cleanup(func() { _ = os.RemoveAll(dir) })
	return filepath.Join(dir, "pprof.sock")
}

// sidecarPprofLogger is at debug so the "off" record reaches the sink and the
// gating assertion does not depend on the suite's environment.
func sidecarPprofLogger() (*logging.Bound, *bytes.Buffer) {
	durable := &bytes.Buffer{}
	logf := logging.NewAtLevel(&bytes.Buffer{}, durable, sharedlogging.LevelDebug).
		With(logging.Context{Component: "sidecar"})
	return logf, durable
}

type sidecarPprofRecord struct {
	Operation string `json:"operation"`
	Level     string `json:"level"`
	Message   string `json:"message"`
}

func decodeSidecarPprofRecords(t *testing.T, durable *bytes.Buffer) []sidecarPprofRecord {
	t.Helper()
	var records []sidecarPprofRecord
	for _, line := range strings.Split(strings.TrimSpace(durable.String()), "\n") {
		if line == "" {
			continue
		}
		var record sidecarPprofRecord
		if err := json.Unmarshal([]byte(line), &record); err != nil {
			t.Fatalf("decode %q: %v", line, err)
		}
		records = append(records, record)
	}
	return records
}

func TestOpenPprofSurfaceGating(t *testing.T) {
	tests := []struct {
		name          string
		addr          func(t *testing.T) string
		wantListening bool
		wantOperation string
	}{
		{
			name:          "off by default",
			addr:          func(*testing.T) string { return "" },
			wantListening: false,
			wantOperation: "pprof.disabled",
		},
		{
			name:          "on with an explicit socket",
			addr:          sidecarPprofSock,
			wantListening: true,
			wantOperation: "pprof.enabled",
		},
		{
			name:          "on with an explicit loopback port",
			addr:          func(*testing.T) string { return "127.0.0.1:0" },
			wantListening: true,
			wantOperation: "pprof.enabled",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			logf, durable := sidecarPprofLogger()

			// Act.
			surface, err := openPprofSurface(tc.addr(t), logf)
			if err != nil {
				t.Fatalf("openPprofSurface = %v, want nil", err)
			}
			t.Cleanup(func() {
				if closeErr := surface.Close(); closeErr != nil {
					t.Errorf("close surface: %v", closeErr)
				}
			})

			// Assert.
			if (surface != nil) != tc.wantListening {
				t.Fatalf("surface listening = %v, want %v", surface != nil, tc.wantListening)
			}
			records := decodeSidecarPprofRecords(t, durable)
			if len(records) != 1 || records[0].Operation != tc.wantOperation {
				t.Fatalf("records = %+v, want exactly one %s", records, tc.wantOperation)
			}
		})
	}
}

func TestOpenPprofSurfaceRecordsAnEnabledSurfaceAtWarn(t *testing.T) {
	// Arrange.
	logf, durable := sidecarPprofLogger()

	// Act.
	surface, err := openPprofSurface(sidecarPprofSock(t), logf)
	if err != nil {
		t.Fatalf("openPprofSurface = %v, want nil", err)
	}
	t.Cleanup(func() {
		if closeErr := surface.Close(); closeErr != nil {
			t.Errorf("close surface: %v", closeErr)
		}
	})

	// Assert. An exposed profiling surface is not routine, and its record must
	// name the socket a client dials.
	record := decodeSidecarPprofRecords(t, durable)[0]
	if record.Level != "warn" || !strings.Contains(record.Message, surface.Address()) {
		t.Fatalf("record = %+v, want warn naming %q", record, surface.Address())
	}
}

func TestOpenPprofSurfaceRefusesAnUnsafeAddress(t *testing.T) {
	// Arrange.
	logf, _ := sidecarPprofLogger()

	// Act.
	surface, err := openPprofSurface("0.0.0.0:6062", logf)

	// Assert.
	if err == nil {
		testclose.OrFail(t, surface)
		t.Fatal("openPprofSurface on a wildcard bind = nil error, want a loud refusal")
	}
}

func TestRunWithLoggerRefusesAnUnsafePprofAddressBeforeStarting(t *testing.T) {
	// Arrange.
	logf, durable := sidecarPprofLogger()

	// Act. The stop channel is never sent on: a refusal must return before
	// the sidecar runs at all.
	err := runWithLogger(Options{PprofAddr: "0.0.0.0:6062"}, logf, make(chan os.Signal))

	// Assert.
	if err == nil {
		t.Fatal("runWithLogger with a wildcard pprof bind = nil error, want a loud refusal")
	}
	for _, record := range decodeSidecarPprofRecords(t, durable) {
		if record.Operation == "start" {
			t.Fatalf("records = %+v, want the refusal before the start record", decodeSidecarPprofRecords(t, durable))
		}
	}
}

func TestClosePprofSurfaceOnAnOffSurfaceRecordsNothing(t *testing.T) {
	// Arrange.
	logf, durable := sidecarPprofLogger()

	// Act.
	closePprofSurface(nil, logf)

	// Assert.
	if records := decodeSidecarPprofRecords(t, durable); len(records) != 0 {
		t.Fatalf("records = %+v, want none", records)
	}
}
