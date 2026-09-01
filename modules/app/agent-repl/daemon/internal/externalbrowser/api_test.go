package externalbrowser_test

import (
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
