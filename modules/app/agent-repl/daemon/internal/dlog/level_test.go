package dlog

import (
	"encoding/json"
	"path/filepath"
	"testing"
)

func TestLogLevelFiltersPersistenceAndTerminalMirror(t *testing.T) {
	tests := []struct {
		name    string
		setting string
		want    map[string]bool
	}{
		{name: "debug", setting: LevelDebug, want: map[string]bool{LevelDebug: true, LevelInfo: true, LevelWarn: true, LevelError: true}},
		{name: "info", setting: LevelInfo, want: map[string]bool{LevelInfo: true, LevelWarn: true, LevelError: true}},
		{name: "warn", setting: LevelWarn, want: map[string]bool{LevelWarn: true, LevelError: true}},
		{name: "error", setting: LevelError, want: map[string]bool{LevelError: true}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			terminal := newCollectingWriter()
			path := filepath.Join(t.TempDir(), "logs", "daemon.run.log")
			s, err := openSurfaces(path, tc.setting, terminal)
			if err != nil {
				t.Fatalf("openSurfaces: %v", err)
			}

			// Act.
			log := s.Global()
			log.Debug("daemon.level.debug", "debug", nil)
			log.Info("daemon.level.info", "info", nil)
			log.Warn("daemon.level.warn", "warn", nil)
			log.Error("daemon.level.error", "error", nil)
			if err := s.Close(); err != nil {
				t.Fatalf("Close: %v", err)
			}

			// Assert.
			persisted := readRecords(t, path)
			mirrored := recordsFromLines(t, terminal.all())
			for _, level := range []string{LevelDebug, LevelInfo, LevelWarn, LevelError} {
				operation := "daemon.level." + level
				if got := hasOperation(persisted, operation); got != tc.want[level] {
					t.Errorf("persisted %s = %t, want %t", level, got, tc.want[level])
				}
				if got := hasOperation(mirrored, operation); got != tc.want[level] {
					t.Errorf("mirrored %s = %t, want %t", level, got, tc.want[level])
				}
			}
		})
	}
}

func recordsFromLines(t *testing.T, lines [][]byte) []map[string]any {
	t.Helper()
	out := make([]map[string]any, 0, len(lines))
	for _, line := range lines {
		var record map[string]any
		if err := json.Unmarshal(line, &record); err != nil {
			t.Fatalf("parse mirrored record %q: %v", line, err)
		}
		out = append(out, record)
	}
	return out
}

func TestEmptyLogLevelDefaultsToInfo(t *testing.T) {
	// Arrange / Act.
	got, err := parseLevel("")

	// Assert.
	if err != nil {
		t.Fatalf("parseLevel: %v", err)
	}
	if got != thresholdInfo {
		t.Fatalf("threshold = %v, want info", got)
	}
}

func TestInvalidLogLevelIsRefused(t *testing.T) {
	// Arrange / Act.
	_, err := parseLevel("verbose")

	// Assert.
	if err == nil {
		t.Fatal("parseLevel accepted an unknown level")
	}
}

func TestOpenSurfacesRefusesAnInvalidProcessLogLevel(t *testing.T) {
	// Arrange.
	t.Setenv(LevelEnvironment, "verbose")
	path := filepath.Join(t.TempDir(), "logs", "daemon.run.log")

	// Act.
	surfaces, err := OpenSurfaces(path)

	// Assert.
	if err == nil {
		_ = surfaces.Close()
		t.Fatal("OpenSurfaces accepted an invalid process log level")
	}
}
