package main

import (
	"testing"

	"agent-repld/internal/cpugov"
)

func TestHostOnBatteryReadsTheGovernorsPowerSource(t *testing.T) {
	tests := []struct {
		name    string
		fixed   string
		want    bool
		wantErr bool
	}{
		{name: "a fixed battery", fixed: "battery", want: true},
		{name: "a fixed AC", fixed: "ac", want: false},
		{name: "a misspelled source is refused", fixed: "mains", wantErr: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			t.Setenv(cpugov.FixedPowerEnv, tt.fixed)

			// Act
			got, err := hostOnBattery()

			// Assert
			if (err != nil) != tt.wantErr || got != tt.want {
				t.Fatalf("hostOnBattery = (%v, %v), want (%v, error %v)", got, err, tt.want, tt.wantErr)
			}
		})
	}
}
