package main

import (
	"bytes"
	"encoding/json"
	"io"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"claude-repld/internal/dlog"
)

// pprofTestSock keeps a socket path inside the platform's sun_path limit.
func pprofTestSock(t *testing.T) string {
	t.Helper()
	dir, err := os.MkdirTemp("", "dp")
	if err != nil {
		t.Fatalf("mkdtemp: %v", err)
	}
	t.Cleanup(func() { _ = os.RemoveAll(dir) })
	return filepath.Join(dir, "pprof.sock")
}

func pprofTestLogger() (*dlog.Logger, *bytes.Buffer) {
	durable := &bytes.Buffer{}
	// Verbose terminal output is enabled so the "off" record is observable in
	// the durable sink regardless of the environment the suite runs under.
	return dlog.New(durable, io.Discard, true), durable
}

func decodeRecords(t *testing.T, durable *bytes.Buffer) []dlog.Record {
	t.Helper()
	var records []dlog.Record
	for _, line := range strings.Split(strings.TrimSpace(durable.String()), "\n") {
		if line == "" {
			continue
		}
		var record dlog.Record
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
			wantOperation: "daemon.pprof.disabled",
		},
		{
			name:          "on with an explicit socket",
			addr:          pprofTestSock,
			wantListening: true,
			wantOperation: "daemon.pprof.enabled",
		},
		{
			name:          "on with an explicit loopback port",
			addr:          func(*testing.T) string { return "127.0.0.1:0" },
			wantListening: true,
			wantOperation: "daemon.pprof.enabled",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			logger, durable := pprofTestLogger()

			// Act.
			surface, err := openPprofSurface(tc.addr(t), logger)
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
			records := decodeRecords(t, durable)
			if len(records) != 1 || records[0].Operation != tc.wantOperation {
				t.Fatalf("records = %+v, want exactly one %s", records, tc.wantOperation)
			}
		})
	}
}

func TestOpenPprofSurfaceNamesTheAddressItsClientMustTarget(t *testing.T) {
	// Arrange.
	logger, durable := pprofTestLogger()

	// Act.
	surface, err := openPprofSurface("127.0.0.1:0", logger)
	if err != nil {
		t.Fatalf("openPprofSurface = %v, want nil", err)
	}
	t.Cleanup(func() {
		if closeErr := surface.Close(); closeErr != nil {
			t.Errorf("close surface: %v", closeErr)
		}
	})

	// Assert. A surface whose record does not name the resolved address is
	// unusable: the kernel chose the port, not the operator.
	record := decodeRecords(t, durable)[0]
	if record.Context["address"] != surface.Address() || record.Context["url"] != surface.URL() {
		t.Fatalf("record context = %+v, want the resolved address %q and url %q",
			record.Context, surface.Address(), surface.URL())
	}
	if record.Level != dlog.LevelWarn {
		t.Fatalf("record level = %s, want warn: an exposed profiling surface is not routine", record.Level)
	}
}

func TestOpenPprofSurfaceRefusesAnUnsafeAddress(t *testing.T) {
	// Arrange.
	logger, _ := pprofTestLogger()

	// Act. An operator who asked for profiles and silently got none is the
	// failure this refusal exists to prevent.
	surface, err := openPprofSurface("0.0.0.0:6060", logger)

	// Assert.
	if err == nil {
		surface.Close()
		t.Fatal("openPprofSurface on a wildcard bind = nil error, want a loud refusal")
	}
}
