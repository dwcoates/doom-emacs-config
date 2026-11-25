package harness

import (
	"errors"
	"strings"
	"testing"
	"time"

	"agent-repld/integration/grantedclock"
)

func TestRequireGrantedClockRefusesARunThatCannotReadIt(t *testing.T) {
	readable := func() (time.Duration, error) { return 0, nil }
	tests := []struct {
		name     string
		source   func() (grantedclock.Stall, error)
		wantFail string
	}{
		{name: "a readable stall lets the run start",
			source: func() (grantedclock.Stall, error) { return readable, nil }},
		{name: "a source that cannot be opened refuses the run",
			source:   func() (grantedclock.Stall, error) { return nil, errors.New("no hw.tbfrequency") },
			wantFail: "the CPU stall cannot be read, and every harness bound is kept on it: no hw.tbfrequency"},
		{name: "a stall that cannot be read refuses the run",
			source: func() (grantedclock.Stall, error) {
				return func() (time.Duration, error) { return 0, errors.New("EPERM") }, nil
			},
			wantFail: "the CPU stall cannot be read, and every harness bound is kept on it: EPERM"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			rec := NewRecordingTB(t, "run")

			// Act
			completed := Within(func() { requireGrantedClockFrom(rec, tt.source) })

			// Assert
			if got := rec.Failures(); completed != (tt.wantFail == "") || !strings.Contains(got, tt.wantFail) || (tt.wantFail == "" && got != "") {
				t.Fatalf("requireGrantedClockFrom() completed %v with failures %q, want %q", completed, got, tt.wantFail)
			}
		})
	}
}
