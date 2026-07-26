package server

import (
	"context"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/frontend"
	"claude-repld/internal/registry"
)

// --- SessionLocator -------------------------------------------------------

func TestSessionLocatorPicksNewestNonTerminalForWorkspace(t *testing.T) {
	// Arrange — two live sessions on /w (older + newer) plus a terminal one
	// and one on a different workspace.
	reg := openTestRegistry(t)
	put := func(id, cwd, created string, terminal bool) {
		if err := reg.Put(registry.Record{SessionID: id, CWD: cwd, CreatedAt: created, Terminal: terminal}); err != nil {
			t.Fatalf("put %s: %v", id, err)
		}
	}
	put("s_old", "/w", "2026-07-23T10:00:00Z", false)
	put("s_new", "/w", "2026-07-23T12:00:00Z", false)
	put("s_dead", "/w", "2026-07-23T13:00:00Z", true)
	put("s_other", "/other", "2026-07-23T14:00:00Z", false)
	loc := &SessionLocator{Reg: reg}

	// Act
	got, ok := loc.Locate("/w")

	// Assert — the newest NON-terminal record on /w wins.
	if !ok || got != "s_new" {
		t.Fatalf("Locate(/w) = %q,%v; want s_new,true", got, ok)
	}
}

func TestSessionLocatorMissForUnknownWorkspace(t *testing.T) {
	// Arrange
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{SessionID: "s1", CWD: "/w", CreatedAt: "2026-07-23T10:00:00Z"}); err != nil {
		t.Fatalf("put: %v", err)
	}
	loc := &SessionLocator{Reg: reg}

	// Act
	_, ok := loc.Locate("/nope")

	// Assert
	if ok {
		t.Fatal("Locate for an unknown workspace should miss")
	}
}

// --- ShimSpawner ----------------------------------------------------------

func TestShimSpawnerDoesNotSpawnWhenTheShimIsConnected(t *testing.T) {
	// Arrange — the listener already has this session's shim, so there is
	// nothing to bring up.
	reg := openTestRegistry(t)
	spawned := 0
	sp := NewShimSpawner(reg,
		func(string) bool { return true },
		func(string, CreateOpts) (func() error, error) { spawned++; return nil, nil },
		nil)

	// Act
	err := sp.EnsureShim(context.Background(), "s1")

	// Assert
	if err != nil {
		t.Fatalf("EnsureShim: %v", err)
	}
	if spawned != 0 {
		t.Fatalf("a connected shim must not be re-spawned (spawned=%d)", spawned)
	}
}

func TestShimSpawnerSpawnsFromRegistryRecordWhenNothingIsAlive(t *testing.T) {
	// Arrange — nothing connected and no lock held; the record supplies the
	// spawn's CreateOpts.
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{
		SessionID: "s1", CWD: "/w", Model: "haiku",
		ConfigDir: "/cfg", ClaudeSessionID: "cli-uuid",
	}); err != nil {
		t.Fatalf("put: %v", err)
	}
	var gotOpts CreateOpts
	sp := NewShimSpawner(reg,
		func(string) bool { return false },
		func(_ string, opts CreateOpts) (func() error, error) {
			gotOpts = opts
			return nil, nil
		},
		nil)

	// Act
	err := sp.EnsureShim(context.Background(), "s1")

	// Assert — CreateOpts reconstructed from the record (resume = the CLI uuid).
	if err != nil {
		t.Fatalf("EnsureShim: %v", err)
	}
	if gotOpts.CWD != "/w" || gotOpts.Model != "haiku" ||
		gotOpts.ConfigDir != "/cfg" || gotOpts.Resume != "cli-uuid" {
		t.Fatalf("spawn opts = %+v", gotOpts)
	}
}

func TestShimSpawnerErrorsWhenNoRecordToSpawnFrom(t *testing.T) {
	// Arrange — no listener AND no registry record: nothing to reconstruct
	// CreateOpts from, so it is a loud error.
	reg := openTestRegistry(t)
	sp := NewShimSpawner(reg,
		func(string) bool { return false },
		func(string, CreateOpts) (func() error, error) { return nil, nil },
		nil)

	// Act
	err := sp.EnsureShim(context.Background(), "ghost")

	// Assert
	if err == nil {
		t.Fatal("spawning a session with no registry record must error")
	}
}

// --- PushForwarder --------------------------------------------------------

func TestPushForwarderDropsUntilTargetSet(t *testing.T) {
	// Arrange — a push before SetTarget loud-logs and drops (no panic, no
	// swallow-into-a-fake-target).
	var logged int
	f := &PushForwarder{Logf: func(string, ...any) { logged++ }}

	// Act — no target yet.
	f.PushWorkspaceState(&frontendv1.WorkspaceState{Workspace: "/w"})

	// Assert — the miss was reported, not silently swallowed.
	if logged != 1 {
		t.Fatalf("pre-target push should log once; logged=%d", logged)
	}
}

func TestPushForwarderForwardsAfterTargetSet(t *testing.T) {
	// Arrange — a real frontend.Server target with one connected client.
	var logged int
	f := &PushForwarder{Logf: func(string, ...any) { logged++ }}
	srv := frontend.New(frontend.Config{
		Logf:    func(string, ...any) {},
		State:   staticState{},
		Handler: nopHandler{},
	})
	t.Cleanup(func() { _ = srv.Close() })
	f.SetTarget(srv)

	// Act — after SetTarget, a push reaches the server (no miss logged). We
	// assert indirectly: the miss counter stays zero because the target load
	// is non-nil.
	f.PushWorkspaceState(&frontendv1.WorkspaceState{Workspace: "/w"})

	// Assert
	if logged != 0 {
		t.Fatalf("post-target push must not log a miss; logged=%d", logged)
	}
}

// --- test doubles for frontend.Server construction ------------------------

type staticState struct{}

func (staticState) Snapshot() *frontendv1.StateSnapshot { return &frontendv1.StateSnapshot{} }

type nopHandler struct{}

func (nopHandler) SubmitPrompt(context.Context, string, string, *frontendv1.SubmitPromptCmd) error {
	return nil
}
func (nopHandler) Interrupt(context.Context, string, string, *frontendv1.InterruptCmd) error {
	return nil
}
func (nopHandler) AnswerPermission(context.Context, string, string, *frontendv1.PermissionAnswerCmd) error {
	return nil
}
func (nopHandler) MergeWorkspace(context.Context, string, string, *frontendv1.MergeWorkspaceCmd) error {
	return nil
}
func (nopHandler) CloseWorkspace(context.Context, string, string, *frontendv1.CloseWorkspaceCmd) error {
	return nil
}
func (nopHandler) OpenWorkspace(context.Context, string, string, *frontendv1.OpenWorkspaceCmd) error {
	return nil
}
func (nopHandler) Resync(context.Context, string, string, *frontendv1.ResyncCmd) error { return nil }
func (nopHandler) CreateSession(context.Context, string, string, *frontendv1.CreateSessionCmd) error {
	return nil
}
func (nopHandler) DeleteSession(context.Context, string, string, *frontendv1.DeleteSessionCmd) error {
	return nil
}
func (nopHandler) Shutdown(context.Context, string, string, *frontendv1.ShutdownCmd) error {
	return nil
}
func (nopHandler) ClientLog(context.Context, string, string, *frontendv1.ClientLogCmd) error {
	return nil
}
func (nopHandler) ForceQueueEntry(context.Context, string, string, *frontendv1.QueueForceCmd) error {
	return nil
}
func (nopHandler) AcceptQueueEntry(context.Context, string, string, *frontendv1.QueueAcceptCmd) error {
	return nil
}
func (nopHandler) CancelQueueEntry(context.Context, string, string, *frontendv1.QueueCancelCmd) error {
	return nil
}
func (nopHandler) PaintAck(context.Context, string, string, *frontendv1.PaintAckCmd) error {
	return nil
}
