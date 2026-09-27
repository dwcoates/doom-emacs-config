package deploy

import (
	"context"
	"os"
	"path/filepath"
	"testing"
)

// standInDaemon writes a stand-in daemon binary whose body is script, so the
// question is asked of a real process without building the daemon.
func standInDaemon(t *testing.T, script string) string {
	t.Helper()
	bin := filepath.Join(t.TempDir(), "claude-repld")
	if err := os.WriteFile(bin, []byte("#!/bin/sh\n"+script), 0o755); err != nil {
		t.Fatalf("write the stand-in: %v", err)
	}
	return bin
}

func TestBinaryLayout(t *testing.T) {
	tests := []struct {
		name    string
		script  string
		want    int
		wantErr bool
	}{
		{name: "a binary answers its layout", script: "[ \"$1\" = -layout-version ] && echo 12\n", want: 12},
		{name: "a binary that fails is not read as any layout", script: "echo refused >&2; exit 2\n", wantErr: true},
		{name: "an answer that is not a number is refused", script: "echo eleven\n", wantErr: true},
		{name: "a non-positive answer is refused", script: "echo 0\n", wantErr: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			bin := standInDaemon(t, tt.script)

			// Act
			got, err := BinaryLayout(context.Background(), bin)

			// Assert
			if (err != nil) != tt.wantErr || got != tt.want {
				t.Fatalf("BinaryLayout = (%d, %v), want (%d, error %v)", got, err, tt.want, tt.wantErr)
			}
		})
	}
}
