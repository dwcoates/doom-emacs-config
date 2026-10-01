package harness

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestBinaryMode(t *testing.T) {
	tests := []struct {
		name     string
		env      map[string]string
		wantMode BuildMode
		wantDir  string
		wantErr  string
	}{
		{name: "nothing set builds here", env: nil, wantMode: BuildHere},
		{name: "prebuild builds into the shared dir", env: map[string]string{PrebuildEnv: "/s"}, wantMode: BuildInto, wantDir: "/s"},
		{name: "prebuilt reads the shared dir", env: map[string]string{PrebuiltEnv: "/s"}, wantMode: UsePrebuilt, wantDir: "/s"},
		{name: "both set is refused", env: map[string]string{PrebuildEnv: "/a", PrebuiltEnv: "/b"}, wantErr: "both set"},
		{name: "prebuilt under coverage is refused", env: map[string]string{PrebuiltEnv: "/s", CoverageEnvVar: "/c"}, wantErr: "uninstrumented"},
		{name: "prebuild under coverage is refused", env: map[string]string{PrebuildEnv: "/s", CoverageEnvVar: "/c"}, wantErr: "uninstrumented"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			getenv := func(k string) string { return tt.env[k] }

			// Act
			mode, dir, err := BinaryMode(getenv)

			// Assert
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("err = %v, want it to mention %q", err, tt.wantErr)
				}
				return
			}
			if err != nil || mode != tt.wantMode || dir != tt.wantDir {
				t.Fatalf("BinaryMode = %v, %q, %v; want %v, %q", mode, dir, err, tt.wantMode, tt.wantDir)
			}
		})
	}
}

func TestSharedBinary(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, dir string)
		wantErr string
	}{
		{
			name: "a built binary is answered",
			arrange: func(t *testing.T, dir string) {
				if err := os.MkdirAll(filepath.Join(dir, "harness"), 0o755); err != nil {
					t.Fatal(err)
				}
				if err := os.WriteFile(filepath.Join(dir, "harness", "fakeshim"), []byte("x"), 0o755); err != nil {
					t.Fatal(err)
				}
			},
		},
		{name: "a missing binary is refused", arrange: func(*testing.T, string) {}, wantErr: "holds no harness/fakeshim"},
		{
			name: "a directory in its place is refused",
			arrange: func(t *testing.T, dir string) {
				if err := os.MkdirAll(filepath.Join(dir, "harness", "fakeshim"), 0o755); err != nil {
					t.Fatal(err)
				}
			},
			wantErr: "is a directory",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			dir := t.TempDir()
			tt.arrange(t, dir)

			// Act
			got, err := SharedBinary(dir, "harness", "fakeshim")

			// Assert
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("err = %v, want it to mention %q", err, tt.wantErr)
				}
				return
			}
			if err != nil || filepath.Base(got) != "fakeshim" {
				t.Fatalf("SharedBinary = %q, %v", got, err)
			}
		})
	}
}
