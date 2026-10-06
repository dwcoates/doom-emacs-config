package wsm

import (
	"testing"

	"claude-repld/internal/envc"
)

func TestUnsyncedFromEnv(t *testing.T) {
	tests := []struct {
		name    string
		forbid  string
		value   string
		want    bool
		wantErr bool
	}{
		{name: "a live daemon with no flag stays durable", forbid: "", value: "", want: false},
		{name: "a live daemon handed the flag refuses to boot", forbid: "", value: "1", wantErr: true},
		{name: "a test-run daemon honors the flag", forbid: "1", value: "1", want: true},
		{name: "a test-run daemon with no flag stays durable", forbid: "1", value: "", want: false},
		{name: "a malformed flag is refused even under the vendor guard", forbid: "1", value: "yes", wantErr: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			t.Setenv(envc.EnvForbidVendorCalls, tc.forbid)
			getenv := func(key string) string {
				if key == EnvTestUnsyncedWrites {
					return tc.value
				}
				return ""
			}

			// Act
			got, err := UnsyncedFromEnv(envc.Load(), getenv)

			// Assert
			if (err != nil) != tc.wantErr {
				t.Fatalf("UnsyncedFromEnv error = %v, wantErr %v", err, tc.wantErr)
			}
			if got != tc.want {
				t.Fatalf("UnsyncedFromEnv = %v, want %v", got, tc.want)
			}
		})
	}
}
