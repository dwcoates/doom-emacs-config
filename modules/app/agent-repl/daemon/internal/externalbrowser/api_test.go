package externalbrowser_test

import (
	"os"
	"path/filepath"
	"testing"

	"claude-repld/internal/dlog"
	"claude-repld/internal/externalbrowser"
)

func TestNewRejectsMissingLogger(t *testing.T) {
	// Arrange.
	cfg := externalbrowser.Config{LauncherCmd: "/bin/true"}

	// Act.
	_, err := externalbrowser.New(cfg)

	// Assert.
	if err == nil {
		t.Fatal("New() = nil error, want a refusal for the missing logger")
	}
}

func TestNewAcceptsAFullyDefaultedConfig(t *testing.T) {
	// Arrange.
	cfg := externalbrowser.Config{Logger: dlog.NewTestLogger()}

	// Act.
	got, err := externalbrowser.New(cfg)

	// Assert.
	if err != nil || got == nil {
		t.Fatalf("New() = (%v, %v), want an opener and no error", got, err)
	}
}

func TestDefaultLauncherConfiguredAtSeesAnExecutable(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), "launcher")
	if err := os.WriteFile(path, []byte("#!/bin/sh\n"), 0o700); err != nil {
		t.Fatalf("WriteFile() = %v", err)
	}

	// Act.
	got := externalbrowser.DefaultLauncherConfiguredAt(path)

	// Assert.
	if !got {
		t.Fatal("DefaultLauncherConfiguredAt() = false, want true for a present launcher")
	}
}

func TestDefaultLauncherConfiguredAtRejectsAnAbsentPath(t *testing.T) {
	// Arrange: the host where the pinned browser was never installed.
	path := filepath.Join(t.TempDir(), "not-installed")

	// Act.
	got := externalbrowser.DefaultLauncherConfiguredAt(path)

	// Assert.
	if got {
		t.Fatal("DefaultLauncherConfiguredAt() = true, want false for an absent launcher")
	}
}

func TestDefaultLauncherConfiguredAtRejectsADirectory(t *testing.T) {
	// Arrange: the .app BUNDLE rather than the executable inside it, which is
	// a directory and cannot be handed a url.
	dir := filepath.Join(t.TempDir(), "Google Chrome.app")
	if err := os.Mkdir(dir, 0o755); err != nil {
		t.Fatalf("Mkdir() = %v", err)
	}

	// Act.
	got := externalbrowser.DefaultLauncherConfiguredAt(dir)

	// Assert.
	if got {
		t.Fatal("DefaultLauncherConfiguredAt() = true, want false for a directory")
	}
}

func TestDefaultLauncherConfiguredAsksAboutThePinnedBinary(t *testing.T) {
	// Arrange: the composition root's question is exactly this one applied to
	// the pinned default, whatever this particular host has installed.
	want := externalbrowser.DefaultLauncherConfiguredAt(externalbrowser.DefaultBinary)

	// Act.
	got := externalbrowser.DefaultLauncherConfigured()

	// Assert.
	if got != want {
		t.Fatalf("DefaultLauncherConfigured() = %v, want %v (the pinned binary's own answer)", got, want)
	}
}
