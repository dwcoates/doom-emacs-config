package persistentwifi

import (
	"strings"
	"testing"
)

func TestNewHost(t *testing.T) {
	tests := []struct {
		name    string
		cfg     Config
		runner  Runner
		wantErr string
	}{
		{name: "macOS is driven by its tools", cfg: Config{Platform: "darwin", Tools: tools}, runner: newFakeRunner(t)},
		{name: "a platform with no host is refused", cfg: Config{Platform: "windows"}, runner: newFakeRunner(t), wantErr: `no host drives persistent wifi on "windows"`},
		{name: "an unnamed platform is refused", cfg: Config{}, runner: newFakeRunner(t), wantErr: `no host drives persistent wifi on ""`},
		{name: "a host without a runner is refused", cfg: Config{Platform: "darwin"}, wantErr: "a runner is required"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			host, err := NewHost(tt.cfg, tt.runner, nil, "")

			// Assert
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("NewHost() = (%v, %v), want an error naming %q", host, err, tt.wantErr)
				}
				return
			}
			if err != nil || host == nil {
				t.Fatalf("NewHost() = (%v, %v), want a host", host, err)
			}
		})
	}
}
