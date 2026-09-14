// startlive_e2e_test.go — a bring-up against a vendor that announces its
// `system:init` only once a first turn reaches it.
//
// THIS IS THE REAL VENDOR'S SHAPE, not a hypothetical one. Grounded
// 2026-09-13 against claude 2.1.220 AND 2.1.270, driven exactly as the shim
// drives them (`--input-format stream-json`): the child answers control
// requests (`supportedModels`, `supportedCommands`, `setPermissionMode`)
// within ~300ms and emits no `init` at all until an input message arrives,
// while feeding one turn produces `init` in ~600ms in the same directory.
//
// The shim's `StartSession` used to wait for `init` before it would accept a
// prompt, so every real bring-up deadlocked and failed at the 45s bound. The
// contract now settles a start on a PROVEN-LIVE SIGNAL — one control
// round-trip answered inside its own bound — and learns the facts `init`
// carries when `init` arrives with the first turn.
//
// The mocked vendor reproduces the shape through the whole-process env lever
// `AGENT_REPL_FAKE_INIT_TIMING=after-first-turn` (agent-shim/claude/shim's
// src/fake/index.ts), which is safe here only because NewWorld gives each
// test its own daemon+store+sidecar+shim world.
package e2e

import (
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"connectrpc.com/connect"

	"claude-repld/integration/harness"
)

// slInitAfterFirstTurn is the world every test here opens: one whose shim's
// mocked vendor withholds its init until a first user message reaches it.
func slInitAfterFirstTurn(t *testing.T) *World {
	t.Helper()
	return NewWorld(t, WorldOpts{DaemonOpts: harness.Opts{
		ExtraEnv: []string{"AGENT_REPL_FAKE_INIT_TIMING=after-first-turn"},
	}})
}

func TestStartSettlesWithoutInit(t *testing.T) {
	t.Parallel()
	// Arrange: a world whose vendor says nothing until it is prompted.
	w := slInitAfterFirstTurn(t)
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)

	// Act: the bring-up, which under the old contract could only time out.
	resp, err := w.Client().OpenWorkspace(w.Ctx(), connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: ws}))
	if err != nil {
		t.Fatalf("OpenWorkspace = error %v, want a typed OpenWorkspaceResponse", err)
	}

	// Assert: the session came up on the control round-trip alone.
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("OpenWorkspace = %v, want success: a start settles on a proven-live control "+
			"round-trip, so a vendor that has not yet announced itself must not refuse the bring-up",
			resp.Msg)
	}
}

func TestFirstTurnCarriesTheInit(t *testing.T) {
	t.Parallel()
	// Arrange: the same world, brought up with no init in sight.
	w := slInitAfterFirstTurn(t)
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)

	// Act: the first prompt, which is what makes the vendor announce itself.
	turn := SubmitPrompt(t, w, ws, "!prose-streamed")

	// Assert: the turn the init rode in on is served like any other, so the
	// facts landing on an already-started session cost the turn nothing.
	row := AwaitTurnEnded(t, w, ws, turn)
	if row.GetTurnEnded() == nil {
		t.Fatalf("the first turn's row = %v, want a terminal", row)
	}
}
