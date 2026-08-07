package main

import (
	"bytes"
	"encoding/json"
	"io"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"agentrepl/shim-store/internal/logging"
)

// storePprofSock keeps a socket path inside the platform's sun_path limit.
func storePprofSock(t *testing.T) string {
	t.Helper()
	dir, err := os.MkdirTemp("", "sp")
	if err != nil {
		t.Fatalf("mkdtemp: %v", err)
	}
	t.Cleanup(func() { _ = os.RemoveAll(dir) })
	return filepath.Join(dir, "pprof.sock")
}

func storePprofLogger() (*logging.Logger, *bytes.Buffer) {
	durable := &bytes.Buffer{}
	// Verbose enabled so the "off" record reaches the sink and the gating
	// assertion does not depend on the suite's environment.
	return logging.New(durable, io.Discard, true), durable
}

type storeLogRecord struct {
	Operation string `json:"operation"`
	Level     string `json:"level"`
	Message   string `json:"message"`
}

func decodeStoreRecords(t *testing.T, durable *bytes.Buffer) []storeLogRecord {
	t.Helper()
	var records []storeLogRecord
	for _, line := range strings.Split(strings.TrimSpace(durable.String()), "\n") {
		if line == "" {
			continue
		}
		var record storeLogRecord
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
			wantOperation: "store.pprof.disabled",
		},
		{
			name:          "on with an explicit socket",
			addr:          storePprofSock,
			wantListening: true,
			wantOperation: "store.pprof.enabled",
		},
		{
			name:          "on with an explicit loopback port",
			addr:          func(*testing.T) string { return "127.0.0.1:0" },
			wantListening: true,
			wantOperation: "store.pprof.enabled",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			log, durable := storePprofLogger()

			// Act.
			surface, err := openPprofSurface(tc.addr(t), log)
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
			records := decodeStoreRecords(t, durable)
			if len(records) != 1 || records[0].Operation != tc.wantOperation {
				t.Fatalf("records = %+v, want exactly one %s", records, tc.wantOperation)
			}
		})
	}
}

func TestOpenPprofSurfaceRecordsAnEnabledSurfaceAtWarn(t *testing.T) {
	// Arrange.
	log, durable := storePprofLogger()

	// Act.
	surface, err := openPprofSurface(storePprofSock(t), log)
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
	record := decodeStoreRecords(t, durable)[0]
	if record.Level != "warn" || !strings.Contains(record.Message, surface.Address()) {
		t.Fatalf("record = %+v, want warn naming %q", record, surface.Address())
	}
}

func TestOpenPprofSurfaceRefusesAnUnsafeAddress(t *testing.T) {
	// Arrange.
	log, _ := storePprofLogger()

	// Act.
	surface, err := openPprofSurface("0.0.0.0:6061", log)

	// Assert.
	if err == nil {
		surface.Close()
		t.Fatal("openPprofSurface on a wildcard bind = nil error, want a loud refusal")
	}
}
