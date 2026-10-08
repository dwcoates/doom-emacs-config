package dlog

import (
	"encoding/json"
	"io"
	"path/filepath"
	"testing"
	"time"

	"agentrepl/logging"
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
	if got != logging.LevelInfo {
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

// windowClock is a settable clock: a level window's end is reached by moving
// it, never by sleeping.
type windowClock struct{ at time.Time }

func (c *windowClock) now() time.Time { return c.at }

func openDebugWindow(t *testing.T, clock *windowClock) (*surfaces, string) {
	t.Helper()
	sel, err := logging.SelectLevel(LevelDebug, "1000300", clock.at)
	if err != nil {
		t.Fatal(err)
	}
	path := filepath.Join(t.TempDir(), "logs", "daemon.run.log")
	s, err := openSurfacesWindow(path, logging.NewWindow(sel, clock.now), io.Discard)
	if err != nil {
		t.Fatalf("openSurfacesWindow: %v", err)
	}
	return s, path
}

func TestLevelWindowAdmitsDebugBeforeItEnds(t *testing.T) {
	// Arrange.
	clock := &windowClock{at: time.Unix(1000000, 0)}
	s, path := openDebugWindow(t, clock)

	// Act.
	s.Global().Debug("daemon.level.inside", "inside", nil)
	if err := s.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Assert.
	if !hasOperation(readRecords(t, path), "daemon.level.inside") {
		t.Fatal("a debug record inside the window was dropped")
	}
}

func TestLevelWindowRecordsItsEndAtInfo(t *testing.T) {
	// Arrange.
	clock := &windowClock{at: time.Unix(1000000, 0)}
	s, path := openDebugWindow(t, clock)
	clock.at = time.Unix(1000300, 0)

	// Act.
	s.Global().Debug("daemon.level.after", "after", nil)
	if err := s.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Assert.
	records := readRecords(t, path)
	if hasOperation(records, "daemon.level.after") {
		t.Error("a debug record passed after the window ended")
	}
	var revert map[string]any
	for _, rec := range records {
		if rec["operation"] == levelWindowOperation {
			revert = rec
		}
	}
	if revert == nil || revert["level"] != LevelInfo {
		t.Fatalf("revert record = %v, want one at info", revert)
	}
	if ctx, _ := revert["context"].(map[string]any); ctx["outcome"] != "window_ended" {
		t.Fatalf("revert context = %v, want outcome window_ended", revert["context"])
	}
}

func TestOpenSurfacesStartsALeftoverDebugLevelAtInfo(t *testing.T) {
	// Arrange. A debug level with no window is a leftover, never a setting.
	t.Setenv(LevelEnvironment, LevelDebug)
	t.Setenv(logging.UntilEnvironment, "")
	path := filepath.Join(t.TempDir(), "logs", "daemon.run.log")

	// Act.
	opened, err := OpenSurfaces(path)
	if err != nil {
		t.Fatalf("OpenSurfaces: %v", err)
	}
	opened.Global().Debug("daemon.level.leftover", "dropped", nil)
	if err := opened.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Assert.
	records := readRecords(t, path)
	if hasOperation(records, "daemon.level.leftover") {
		t.Error("a leftover debug level admitted a debug record")
	}
	if !hasOperation(records, levelWindowOperation) {
		t.Error("the ignored level was not stated")
	}
}

func TestOpenSurfacesRefusesAMalformedLevelWindow(t *testing.T) {
	// Arrange.
	t.Setenv(LevelEnvironment, LevelDebug)
	t.Setenv(logging.UntilEnvironment, "soon")
	path := filepath.Join(t.TempDir(), "logs", "daemon.run.log")

	// Act.
	opened, err := OpenSurfaces(path)

	// Assert.
	if err == nil {
		_ = opened.Close()
		t.Fatal("OpenSurfaces accepted a malformed level window")
	}
}
