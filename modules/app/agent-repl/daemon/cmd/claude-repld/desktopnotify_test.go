package main

import (
	"context"
	"errors"
	"testing"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// namesDB answers one workspace record, or one error.
type namesDB struct {
	wsm.DB
	record wsm.Workspace
	err    error
}

func (d namesDB) Workspace(context.Context, ids.WorkspaceID) (wsm.Workspace, error) {
	return d.record, d.err
}

func TestWorkspaceNamesAnswersTheRegistryName(t *testing.T) {
	// Arrange
	names := workspaceNames{db: namesDB{record: wsm.Workspace{Name: "fix-flaky-reconnect"}}}

	// Act
	got, err := names.WorkspaceName(context.Background(), "ws1")

	// Assert
	if err != nil || got != "fix-flaky-reconnect" {
		t.Fatalf("WorkspaceName = (%q, %v), want the registry name", got, err)
	}
}

func TestWorkspaceNamesSurfacesAFailedRead(t *testing.T) {
	// Arrange
	cause := errors.New("no such workspace")
	names := workspaceNames{db: namesDB{err: cause}}

	// Act
	_, err := names.WorkspaceName(context.Background(), "ws1")

	// Assert
	if !errors.Is(err, cause) {
		t.Fatalf("WorkspaceName error = %v, want the read's cause", err)
	}
}

// recordingClicks records the clicks relayed to it.
type recordingClicks struct{ got []ids.WorkspaceID }

func (c *recordingClicks) NotificationClicked(ws ids.WorkspaceID) { c.got = append(c.got, ws) }

func TestClickForwarderRelaysToTheBoundServer(t *testing.T) {
	// Arrange
	target := &recordingClicks{}
	f := &clickForwarder{}
	f.bind(target)

	// Act
	f.NotificationClicked("ws1")

	// Assert
	if len(target.got) != 1 || target.got[0] != "ws1" {
		t.Fatalf("relayed %v, want [ws1]", target.got)
	}
}

func TestClickForwarderRefusesAClickBeforeTheServerIsBound(t *testing.T) {
	// Arrange
	f := &clickForwarder{}

	// Assert
	defer func() {
		if recover() == nil {
			t.Fatal("an unbound forwarder accepted a click")
		}
	}()

	// Act
	f.NotificationClicked("ws1")
}

func TestResolveBannerBackendTakesTheOverride(t *testing.T) {
	// Arrange
	t.Setenv("AGENT_REPL_NOTIFIER_CMD", "/tmp/fake-banner")
	log := dlog.NewTestLogger()

	// Act
	backend, err := resolveBannerBackend(log)

	// Assert
	if err != nil {
		t.Fatalf("resolveBannerBackend: %v", err)
	}
	if backend.Program() != "/tmp/fake-banner" {
		t.Fatalf("program = %q, want the override", backend.Program())
	}
}
