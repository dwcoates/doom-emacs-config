package main

import (
	"context"
	"errors"
	"testing"

	"claude-repld/internal/account"
	"claude-repld/internal/ids"
)

// routeAll is an account resolver that routes every directory to one root.
type routeAll struct {
	account.Resolver
	root string
}

func (r routeAll) ConfigDirFor(string) string { return r.root }

func TestTitleConfigDirsAnswersTheWorkspacesAccountRoot(t *testing.T) {
	// Arrange.
	dirs := titleConfigDirs{
		workspaceDir: func(context.Context, ids.WorkspaceID) (string, error) { return "/tree/w1", nil },
		accounts:     routeAll{root: "/acct"},
	}

	// Act.
	got, err := dirs.ConfigDirFor(context.Background(), "w1")

	// Assert.
	if err != nil || got != "/acct" {
		t.Fatalf("ConfigDirFor = %q, %v; want /acct", got, err)
	}
}

func TestTitleConfigDirsReadsOnTheCallersContext(t *testing.T) {
	// Arrange: the lookup answers the context it was handed.
	dirs := titleConfigDirs{
		workspaceDir: func(ctx context.Context, _ ids.WorkspaceID) (string, error) { return "", ctx.Err() },
		accounts:     routeAll{root: "/acct"},
	}
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act.
	_, err := dirs.ConfigDirFor(ctx, "w1")

	// Assert.
	if !errors.Is(err, context.Canceled) {
		t.Fatalf("err = %v, want the caller's ended context", err)
	}
}
