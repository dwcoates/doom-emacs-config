package suites

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestPrepareWebappDist(t *testing.T) {
	tests := []struct {
		name    string
		script  string
		wantErr string
	}{
		{name: "a fresh or freshly built dist", script: "#!/bin/sh\n[ \"$1\" = ensure ] || exit 9\necho \"$1\" > \"$(dirname \"$0\")/called\"\n"},
		{name: "a failed build names the module and carries its output", script: "#!/bin/sh\necho vite exploded >&2\nexit 1\n", wantErr: "vite exploded"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			module := t.TempDir()
			bin := filepath.Join(module, "e2e", "sandbox", "bin")
			write(t, filepath.Join(bin, "webapp-dist.sh"), tt.script, 0o755)

			// Act
			err := PrepareWebappDist(module)

			// Assert
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) || !strings.Contains(err.Error(), "build the webapp dist of "+module) {
					t.Fatalf("err = %v, want one naming %s and carrying %q", err, module, tt.wantErr)
				}
				return
			}
			if err != nil {
				t.Fatal(err)
			}
			got, readErr := os.ReadFile(filepath.Join(bin, "called"))
			if readErr != nil || string(got) != "ensure\n" {
				t.Fatalf("webapp-dist.sh was run with %q (%v), want ensure", got, readErr)
			}
		})
	}
}
