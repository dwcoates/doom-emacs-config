package cachelapse

import (
	"strings"
	"testing"
	"time"

	"agent-repld/internal/envc"
)

func TestIntervalFromEnv(t *testing.T) {
	tests := []struct {
		name    string
		forbid  string
		value   string
		want    time.Duration
		wantErr string
	}{
		{name: "a live daemon with no seam beats on the production interval", forbid: "", value: "", want: KeepaliveInterval},
		{name: "a live daemon handed the seam refuses to boot", forbid: "", value: "100ms", wantErr: "test-run seam"},
		{name: "a test-run daemon with no seam beats on the production interval", forbid: "1", value: "", want: KeepaliveInterval},
		{name: "a test-run daemon honors the seam", forbid: "1", value: "100ms", want: 100 * time.Millisecond},
		{name: "a value that is not a duration is refused", forbid: "1", value: "soon", wantErr: "is not a duration"},
		{name: "a zero duration is refused", forbid: "1", value: "0s", wantErr: "is not a positive duration"},
		{name: "a negative duration is refused", forbid: "1", value: "-1s", wantErr: "is not a positive duration"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			t.Setenv(envc.EnvForbidVendorCalls, tc.forbid)
			getenv := func(key string) string {
				if key == EnvTestKeepaliveInterval {
					return tc.value
				}
				return ""
			}

			// Act
			got, err := IntervalFromEnv(envc.Load(), getenv)

			// Assert
			if tc.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
					t.Fatalf("IntervalFromEnv = %v, %v; want an error containing %q", got, err, tc.wantErr)
				}
				return
			}
			if err != nil || got != tc.want {
				t.Fatalf("IntervalFromEnv = %v, %v; want %v", got, err, tc.want)
			}
		})
	}
}
