package deploy

import (
	"context"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"claude-repld/internal/wsm"
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

func TestBinaryMigrationKind(t *testing.T) {
	tests := []struct {
		name    string
		script  string
		want    wsm.MigrationKind
		wantErr bool
	}{
		{name: "a binary answers additive for the running layout", script: "[ \"$1\" = -migration-kind-from=13 ] && echo additive\n", want: wsm.MigrationAdditive},
		{name: "a binary answers breaking", script: "echo breaking\n", want: wsm.MigrationBreaking},
		{name: "a binary that fails is not read as any kind", script: "echo refused >&2; exit 2\n", wantErr: true},
		{name: "an answer that names no kind is refused", script: "echo maybe\n", wantErr: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			bin := standInDaemon(t, tt.script)

			// Act
			got, err := BinaryMigrationKind(context.Background(), bin, 13)

			// Assert
			if (err != nil) != tt.wantErr || (!tt.wantErr && got != tt.want) {
				t.Fatalf("BinaryMigrationKind = (%v, %v), want (%v, error %v)", got, err, tt.want, tt.wantErr)
			}
		})
	}
}

func TestAskBinary(t *testing.T) {
	tests := []struct {
		name    string
		script  string
		want    string
		wantErr string
	}{
		{name: "the answer is the trimmed stdout, asked with the question", script: "echo \"  $1  \"\n", want: "-the-question"},
		{name: "a failure carries the question and the stderr", script: "echo nope >&2; exit 2\n", wantErr: "nope"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			bin := standInDaemon(t, tt.script)

			// Act
			got, err := askBinary(context.Background(), bin, "-the-question")

			// Assert
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) || !strings.Contains(err.Error(), "-the-question") {
					t.Fatalf("askBinary error = %v, want it to carry %q and the question", err, tt.wantErr)
				}
				return
			}
			if err != nil || got != tt.want {
				t.Fatalf("askBinary = (%q, %v), want %q", got, err, tt.want)
			}
		})
	}
}

// EVERY QUESTION TO THE STAGED BINARY GOES THROUGH askBinary: a question
// spelled with its own exec would drift from the bound and the error shape.
func TestTheBinaryQuestionsShareAskBinary(t *testing.T) {
	// Arrange
	src, err := os.ReadFile("layout.go")
	if err != nil {
		t.Fatalf("read layout.go: %v", err)
	}

	// Act
	execs := strings.Count(string(src), "exec.CommandContext(")

	// Assert
	if execs != 1 {
		t.Fatalf("layout.go runs %d processes by hand, want the one inside askBinary", execs)
	}
}
