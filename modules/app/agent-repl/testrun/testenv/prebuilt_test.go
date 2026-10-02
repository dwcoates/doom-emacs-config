package testenv

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
		{name: "nothing set builds here", wantMode: BuildHere},
		{name: "prebuild fills the shared directory", env: map[string]string{Prebuild: "/s"}, wantMode: BuildInto, wantDir: "/s"},
		{name: "prebuilt reads the shared directory", env: map[string]string{Prebuilt: "/s"}, wantMode: UsePrebuilt, wantDir: "/s"},
		{name: "both modes are refused", env: map[string]string{Prebuild: "/a", Prebuilt: "/b"}, wantErr: "both set"},
		{name: "prebuilt under coverage is refused", env: map[string]string{Prebuilt: "/s", Coverage: "/c"}, wantErr: "uninstrumented"},
		{name: "prebuild under coverage is refused", env: map[string]string{Prebuild: "/s", Coverage: "/c"}, wantErr: "uninstrumented"},
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
		arrange func(t *testing.T, path string)
		wantErr string
	}{
		{name: "file", arrange: func(t *testing.T, path string) {
			if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
				t.Fatal(err)
			}
			if err := os.WriteFile(path, []byte("x"), 0o755); err != nil {
				t.Fatal(err)
			}
		}},
		{name: "missing", arrange: func(*testing.T, string) {}, wantErr: "holds no harness/tool"},
		{name: "directory", arrange: func(t *testing.T, path string) {
			if err := os.MkdirAll(path, 0o755); err != nil {
				t.Fatal(err)
			}
		}, wantErr: "is a directory"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			dir := t.TempDir()
			path := filepath.Join(dir, "harness", "tool")
			tt.arrange(t, path)

			// Act
			got, err := SharedBinary(dir, "harness", "tool")

			// Assert
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("err = %v, want it to mention %q", err, tt.wantErr)
				}
				return
			}
			want, wantErr := filepath.EvalSymlinks(path)
			if err != nil || wantErr != nil || got != want {
				t.Fatalf("SharedBinary = %q, %v; want %q (%v)", got, err, want, wantErr)
			}
		})
	}
}
