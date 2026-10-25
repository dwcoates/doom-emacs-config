package main

import (
	"maps"
	"os"
	"path/filepath"
	"testing"
)

func TestOpenProfileSession(t *testing.T) {
	tests := []struct {
		name   string
		active *string
		want   map[string]bool
	}{
		{"no open session keeps the root's own files", nil, map[string]bool{"active": true, "armed": true}},
		{"an open session is kept too", ptr("20261012T192845Z\n"), map[string]bool{"active": true, "armed": true, "20261012T192845Z": true}},
		{"an empty active file names no session", ptr("\n"), map[string]bool{"active": true, "armed": true}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			root := t.TempDir()
			if tt.active != nil {
				if err := os.WriteFile(filepath.Join(root, "active"), []byte(*tt.active), 0o644); err != nil {
					t.Fatal(err)
				}
			}

			// Act.
			got, err := openProfileSession(root)

			// Assert.
			if err != nil || !maps.Equal(got, tt.want) {
				t.Fatalf("openProfileSession = (%v, %v), want %v", got, err, tt.want)
			}
		})
	}
}

func TestOpenProfileSessionRefusesAnUnreadableActiveFile(t *testing.T) {
	// Arrange: the active file is a directory.
	root := t.TempDir()
	if err := os.Mkdir(filepath.Join(root, "active"), 0o755); err != nil {
		t.Fatal(err)
	}

	// Act.
	_, err := openProfileSession(root)

	// Assert.
	if err == nil {
		t.Fatal("openProfileSession = nil error, want the read failure")
	}
}

func TestStorageAgedDirsHonorTheirWritersOverrides(t *testing.T) {
	// Arrange.
	env := map[string]string{envProfileRoot: "/p", envTestPasses: "/t"}

	// Act.
	dirs, err := storageAgedDirs(func(k string) string { return env[k] })

	// Assert.
	if err != nil || len(dirs) != 2 || dirs[0].Dir != "/p" || dirs[1].Dir != "/t" {
		t.Fatalf("storageAgedDirs = (%+v, %v), want the overrides", dirs, err)
	}
}

func TestStorageAgedDirsDefaultUnderTheCache(t *testing.T) {
	// Arrange.
	home := t.TempDir()
	t.Setenv("HOME", home)

	// Act.
	dirs, err := storageAgedDirs(func(string) string { return "" })

	// Assert.
	cache := filepath.Join(home, ".cache", "agent-repl")
	if err != nil || dirs[0].Dir != filepath.Join(cache, "profiles") || dirs[1].Dir != filepath.Join(cache, "test-passes") {
		t.Fatalf("storageAgedDirs = (%+v, %v), want both under %s", dirs, err, cache)
	}
}

func ptr(s string) *string { return &s }
