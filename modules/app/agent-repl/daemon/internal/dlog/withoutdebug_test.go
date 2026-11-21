package dlog

import (
	"testing"

	"agentrepl/logging/logsite"
)

func TestWithoutDebugDropsOnlyDebug(t *testing.T) {
	// Arrange
	base := NewTestLogger()
	log := WithoutDebug(base)

	// Act
	log.Debug("daemon.test.debug", "dropped", nil)
	log.Info("daemon.test.info", "kept", nil)
	log.Warn("daemon.test.warn", "kept", nil)
	log.Error("daemon.test.error", "kept", nil)

	// Assert
	var got []string
	for _, r := range base.Records() {
		got = append(got, r.Level)
	}
	if len(got) != 3 || got[0] != LevelInfo || got[1] != LevelWarn || got[2] != LevelError {
		t.Fatalf("levels = %v, want info, warn and error only", got)
	}
}

func TestWithoutDebugKeepsDroppingDebugAfterWith(t *testing.T) {
	// Arrange
	base := NewTestLogger()

	// Act
	bound := WithoutDebug(base).With(Context{"workspace_id": "w1"})
	bound.Debug("daemon.test.debug", "dropped", nil)
	bound.Info("daemon.test.info", "kept", nil)

	// Assert
	records := base.Records()
	if len(records) != 1 || records[0].Level != LevelInfo {
		t.Fatalf("records = %+v, want the info record alone", records)
	}
}

func TestWithoutDebugIsNeverEnabledForDebug(t *testing.T) {
	tests := []struct {
		level string
		want  bool
	}{
		{level: LevelDebug, want: false},
		{level: LevelInfo, want: true},
		{level: LevelError, want: true},
	}
	for _, tt := range tests {
		t.Run(tt.level, func(t *testing.T) {
			// Arrange
			log := WithoutDebug(NewTestLogger())

			// Act
			got := Enabled(log, tt.level)

			// Assert
			if got != tt.want {
				t.Fatalf("Enabled(%s) = %v, want %v", tt.level, got, tt.want)
			}
		})
	}
}

func TestWithoutDebugForwardsOnlyNonDebugMessages(t *testing.T) {
	tests := []struct {
		name  string
		level string
		kept  bool
	}{
		{name: "debug is dropped", level: LevelDebug, kept: false},
		{name: "info is kept", level: LevelInfo, kept: true},
		{name: "error is kept", level: LevelError, kept: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			base := NewTestLogger()

			// Act.
			WithoutDebug(base).Log(tc.level, "daemon.test.forwarded", logsite.Msg("a forwarded record"), nil)

			// Assert.
			if got := len(base.Records()) == 1; got != tc.kept {
				t.Fatalf("records = %+v, want kept %v", base.Records(), tc.kept)
			}
		})
	}
}

func TestWithoutDebugReplaysAHeldRecordOnlyAboveDebug(t *testing.T) {
	tests := []struct {
		name  string
		level string
		kept  bool
	}{
		{name: "a debug replay is dropped", level: LevelDebug, kept: false},
		{name: "a warn replay is kept, keyed by its call", level: LevelWarn, kept: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			base := NewTestLogger()

			// Act.
			WithoutDebug(base).logAt(tc.level, "daemon.test.replay", "held", "pkg.F", nil)

			// Assert.
			records := base.Records()
			if (len(records) == 1) != tc.kept {
				t.Fatalf("records = %+v, want kept %v", records, tc.kept)
			}
			if tc.kept && records[0].Site != logsite.Key(RuntimeDaemon, "pkg.F", "held") {
				t.Fatalf("site = %s, want the held call's", records[0].Site)
			}
		})
	}
}
