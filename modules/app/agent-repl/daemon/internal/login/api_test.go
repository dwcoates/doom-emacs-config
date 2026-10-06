package login_test

import (
	"testing"

	"claude-repld/internal/dlog"
	"claude-repld/internal/envc"
	"claude-repld/internal/ids"
	"claude-repld/internal/login"
)

func TestNewRejectsAMissingConfigDirResolver(t *testing.T) {
	// Arrange.
	guard := envc.NewVendorGuard(envc.Load())

	// Act.
	_, err := login.New(guard, "/bin/true", nil, dlog.NewTestLogger(), newFakeObserver())

	// Assert.
	if err == nil {
		t.Fatal("New() = nil error, want a refusal (a login is keyed by its account root)")
	}
}

func TestNewRejectsAMissingLogger(t *testing.T) {
	// Arrange.
	guard := envc.NewVendorGuard(envc.Load())
	route := func(ids.WorkspaceID) (string, error) { return "/roots/default", nil }

	// Act.
	_, err := login.New(guard, "/bin/true", route, nil, newFakeObserver())

	// Assert.
	if err == nil {
		t.Fatal("New() = nil error, want a refusal for the missing logger")
	}
}

func TestResizeWinsize(t *testing.T) {
	// Arrange.
	r := login.Resize{Rows: 50, Cols: 200}

	// Act.
	got := r.Winsize()

	// Assert.
	if got.Rows != 50 || got.Cols != 200 {
		t.Fatalf("Winsize() = %dx%d, want 50x200", got.Rows, got.Cols)
	}
}

func TestNewRejectsAMissingObserver(t *testing.T) {
	// Arrange.
	route := func(ids.WorkspaceID) (string, error) { return "/roots/default", nil }

	// Act.
	_, err := login.New(envc.NewVendorGuard(envc.Load()), "/bin/true", route, dlog.NewTestLogger(), nil)

	// Assert.
	if err == nil {
		t.Fatal("New() = nil error, want a refusal for the missing observer")
	}
}
