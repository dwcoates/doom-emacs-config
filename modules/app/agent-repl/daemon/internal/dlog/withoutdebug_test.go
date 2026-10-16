package dlog

import "testing"

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
