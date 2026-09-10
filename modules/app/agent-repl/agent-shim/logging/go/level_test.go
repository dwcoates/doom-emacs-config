package logging

import "testing"

func TestParseLevelAcceptsEveryDeclaredLevel(t *testing.T) {
	tests := []struct {
		name  string
		value string
		want  Level
	}{
		{name: "debug", value: "debug", want: LevelDebug},
		{name: "info", value: "info", want: LevelInfo},
		{name: "warn", value: "warn", want: LevelWarn},
		{name: "error", value: "error", want: LevelError},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: tc.value names one declared threshold.

			// Act: parse the configured value.
			got, err := ParseLevel(tc.value)

			// Assert: the exact ordered level is returned.
			if err != nil {
				t.Fatalf("ParseLevel(%q): %v", tc.value, err)
			}
			if got != tc.want {
				t.Errorf("ParseLevel(%q) = %d, want %d", tc.value, got, tc.want)
			}
		})
	}
}

func TestParseLevelDefaultsAnUnsetValueToInfo(t *testing.T) {
	// Arrange: the environment supplied no value.

	// Act: parse the empty setting.
	got, err := ParseLevel("")

	// Assert: ordinary lifecycle records remain enabled and debug stays off.
	if err != nil {
		t.Fatalf("ParseLevel(empty): %v", err)
	}
	if got != LevelInfo {
		t.Errorf("ParseLevel(empty) = %d, want info", got)
	}
}

func TestParseLevelRefusesAnUnknownValue(t *testing.T) {
	// Arrange: the setting names no declared severity.

	// Act: parse the malformed setting.
	_, err := ParseLevel("trace")

	// Assert: bootstrap receives a concrete refusal.
	if err == nil {
		t.Fatal("ParseLevel(trace) = nil error, want refusal")
	}
}

func TestLevelAllowsRecordsAtOrAboveItsThreshold(t *testing.T) {
	tests := []struct {
		name      string
		threshold Level
		candidate string
		want      bool
	}{
		{name: "below", threshold: LevelWarn, candidate: "info", want: false},
		{name: "equal", threshold: LevelWarn, candidate: "warn", want: true},
		{name: "above", threshold: LevelWarn, candidate: "error", want: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: tc.threshold is the configured minimum.

			// Act: ask whether one candidate record is enabled.
			got := tc.threshold.Allows(tc.candidate)

			// Assert: ordering alone decides the result.
			if got != tc.want {
				t.Errorf("threshold %d Allows(%q) = %t, want %t", tc.threshold, tc.candidate, got, tc.want)
			}
		})
	}
}

func TestLevelAllowsPanicsOnAnUnknownRecordLevel(t *testing.T) {
	// Arrange: a call site supplied an undeclared record severity.
	defer func() {
		// Assert: filtering cannot hide the invalid record.
		if recover() == nil {
			t.Fatal("Allows(trace) did not panic")
		}
	}()

	// Act: filter the malformed record.
	LevelError.Allows("trace")
}
