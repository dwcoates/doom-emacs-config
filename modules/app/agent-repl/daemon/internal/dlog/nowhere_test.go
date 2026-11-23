package dlog

import (
	"testing"

	"agentrepl/logging/logsite"
)

func TestNowhereTakesEveryCallAndRecordsNothing(t *testing.T) {
	tests := []struct {
		name string
		emit func(Logger)
	}{
		{name: "debug", emit: func(l Logger) { l.Debug("daemon.test.nowhere", "dropped", nil) }},
		{name: "info", emit: func(l Logger) { l.Info("daemon.test.nowhere", "dropped", nil) }},
		{name: "warn", emit: func(l Logger) { l.Warn("daemon.test.nowhere", "dropped", nil) }},
		{name: "error", emit: func(l Logger) { l.Error("daemon.test.nowhere", "dropped", nil) }},
		{name: "a forwarded message", emit: func(l Logger) {
			l.Log(LevelError, "daemon.test.nowhere", logsite.Msg("dropped"), nil)
		}},
		{name: "a derived logger", emit: func(l Logger) {
			l.With(Context{"k": "v"}).Info("daemon.test.nowhere", "dropped", nil)
		}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			log := Nowhere()

			// Act.
			got := func() (got any) {
				defer func() { got = recover() }()
				tc.emit(log)
				return nil
			}()

			// Assert.
			if got != nil {
				t.Fatalf("Nowhere panicked: %v", got)
			}
		})
	}
}

func TestNowhereDerivesNowhere(t *testing.T) {
	// Act.
	derived := Nowhere().With(Context{"k": "v"})

	// Assert.
	if _, ok := derived.(nowhere); !ok {
		t.Fatalf("With answered %T, want the discarding logger", derived)
	}
}
