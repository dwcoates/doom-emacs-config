package footer

import (
	"testing"

	"agent-repld/internal/wsm"
)

func TestAllowanceWindowLiveVerdict(t *testing.T) {
	tests := []struct {
		name   string
		window allowanceWindow
		nowS   int64
		want   wsm.AllowanceVerdict
	}{
		{name: "a verdict before its own reset stands",
			window: allowanceWindow{verdict: wsm.VerdictRejected, verdictResetsAtS: 100, resetsAtS: 500}, nowS: 99, want: wsm.VerdictRejected},
		{name: "a verdict at its own reset lapses",
			window: allowanceWindow{verdict: wsm.VerdictRejected, verdictResetsAtS: 100, resetsAtS: 500}, nowS: 100, want: wsm.VerdictNone},
		{name: "a verdict past its own reset lapses beside a later figure",
			window: allowanceWindow{verdict: wsm.VerdictAllowedWarning, verdictResetsAtS: 100, resetsAtS: 500}, nowS: 200, want: wsm.VerdictNone},
		{name: "a verdict with no reset of its own stands until the figures' reset",
			window: allowanceWindow{verdict: wsm.VerdictAllowed, resetsAtS: 500}, nowS: 499, want: wsm.VerdictAllowed},
		{name: "a verdict with no reset of its own lapses at the figures' reset",
			window: allowanceWindow{verdict: wsm.VerdictAllowed, resetsAtS: 500}, nowS: 500, want: wsm.VerdictNone},
		{name: "no verdict is none",
			window: allowanceWindow{resetsAtS: 500}, nowS: 1, want: wsm.VerdictNone},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := tc.window.liveVerdict(tc.nowS)

			// Assert
			if got != tc.want {
				t.Fatalf("liveVerdict(%d) = %q, want %q", tc.nowS, got, tc.want)
			}
		})
	}
}

func TestAllowanceWindowObserveVerdict(t *testing.T) {
	tests := []struct {
		name       string
		window     allowanceWindow
		resetsAtMs int64
		wantResetS int64
	}{
		{name: "the event's own reset scopes the verdict",
			window: allowanceWindow{figured: true, resetsAtS: 500}, resetsAtMs: 100_000, wantResetS: 100},
		{name: "an event naming no reset takes the drawn figures' reset",
			window: allowanceWindow{figured: true, resetsAtS: 500}, resetsAtMs: 0, wantResetS: 500},
		{name: "an event naming no reset on an unfigured window leaves the reset unknown",
			window: allowanceWindow{}, resetsAtMs: 0, wantResetS: 0},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			w := tc.window

			// Act
			w.observeVerdict(wsm.VerdictRejected, tc.resetsAtMs)

			// Assert
			if w.verdict != wsm.VerdictRejected || w.verdictResetsAtS != tc.wantResetS {
				t.Fatalf("window = %+v, want rejected scoped to reset %d", w, tc.wantResetS)
			}
		})
	}
}
