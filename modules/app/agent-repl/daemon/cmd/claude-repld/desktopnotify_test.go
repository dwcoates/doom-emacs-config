package main

import (
	"context"
	"errors"
	"os"
	"strings"
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

func TestResolveChimeTakesTheOverride(t *testing.T) {
	// Arrange
	t.Setenv("AGENT_REPL_CHIME_CMD", "/tmp/fake-chime")
	log := dlog.NewTestLogger()

	// Act
	chime, err := resolveChime(log)

	// Assert
	if err != nil {
		t.Fatalf("resolveChime: %v", err)
	}
	if chime.Program() != "/tmp/fake-chime" {
		t.Fatalf("program = %q, want the override", chime.Program())
	}
}

// fakeProgram is a resolved program for resolveNotifyProgram.
type fakeProgram struct{ bin string }

func (p fakeProgram) Program() string { return p.bin }

func TestResolveNotifyProgramRecordsTheResolvedProgram(t *testing.T) {
	// Arrange
	t.Setenv("AGENT_REPL_TEST_OVERRIDE", "/tmp/override")
	log := dlog.NewTestLogger()

	// Act
	got, err := resolveNotifyProgram(log, "chime", "AGENT_REPL_TEST_OVERRIDE", func(_, override string) (fakeProgram, error) {
		return fakeProgram{bin: override}, nil
	})

	// Assert
	if err != nil || got.bin != "/tmp/override" {
		t.Fatalf("resolveNotifyProgram = (%+v, %v), want the override", got, err)
	}
	r := findRecord(t, log.Records(), "resolved the desktop chime program")
	if r.Level != "info" || r.Context["role"] != "chime" || r.Context["program"] != "/tmp/override" {
		t.Fatalf("record = %+v, want the role and program at info", r)
	}
}

func TestResolveNotifyProgramRecordsAMissingProgram(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	cause := errors.New("not installed")

	// Act
	_, err := resolveNotifyProgram(log, "banner", "AGENT_REPL_TEST_UNSET", func(_, _ string) (fakeProgram, error) {
		return fakeProgram{}, cause
	})

	// Assert
	if !errors.Is(err, cause) {
		t.Fatalf("err = %v, want the build's cause", err)
	}
	r := findRecord(t, log.Records(), "no desktop banner program; it will not run")
	if r.Level != "error" || r.Context["role"] != "banner" || r.Context["cause"] != "not installed" || r.Context["override_env"] != "AGENT_REPL_TEST_UNSET" {
		t.Fatalf("record = %+v, want the role, cause and override env at error", r)
	}
}

// TestNotifyProgramsResolveThroughOneHelper fails a resolver that hand-rolls
// its boot record instead of asking resolveNotifyProgram.
func TestNotifyProgramsResolveThroughOneHelper(t *testing.T) {
	// Arrange
	src, err := os.ReadFile("desktopnotify.go")
	if err != nil {
		t.Fatalf("read desktopnotify.go: %v", err)
	}

	// Act
	asks := strings.Count(string(src), "return resolveNotifyProgram(")
	records := strings.Count(string(src), "log.Error(opDesktopNotify")

	// Assert
	if asks != 2 || records != 1 {
		t.Fatalf("resolveNotifyProgram( x%d (want 2), log.Error(opDesktopNotify x%d (want 1)", asks, records)
	}
}
