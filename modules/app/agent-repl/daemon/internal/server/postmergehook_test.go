package server

import (
	"context"
	"strings"
	"testing"

	"claude-repld/internal/progress"
	"claude-repld/internal/workspace/merge"
)

// stubPostMergeHook is the do-nothing merge.PostMergeHook the server's own
// coordinator fixtures take. The post-merge handoff has its own tests in
// internal/workspace/postmerge; here it only has to exist.
type stubPostMergeHook struct{}

func (stubPostMergeHook) AfterMerged(context.Context, merge.Request) error { return nil }

// Live makes the package's PromptRouter fake answer the liveness fact the
// post-merge parent handoff needs. It reports every workspace live, so a
// wiring test never depends on which workspaces a fake decided to know about.
func (f *fakePrompts) Live(string) bool { return true }

// PostprocessingPrompt makes the package's workspace-creation fake answer the
// creation-time postprocessing lookup. Its records are empty, which is the
// ordinary case: most workspaces are created without one.
func (f *fakeWorkspaceCreation) PostprocessingPrompt(string) (string, error) { return "", nil }

// promptRouterOnly carries the PromptRouter method set and NOTHING else: an
// embedded interface contributes exactly its own methods, so this models a
// controller fleet wired in without the liveness fact.
type promptRouterOnly struct{ PromptRouter }

// creationBridgeOnly is the same narrowing for the workspace-creation bridge.
type creationBridgeOnly struct{ WorkspaceCreationBridge }

func TestBuildPostMergeHookRefusesAPromptRouterWithoutLiveness(t *testing.T) {
	// Arrange — a PromptRouter that carries no liveness fact at all.
	cfg := AgentShimConfig{
		Prompts:           promptRouterOnly{},
		WorkspaceCreation: newFakeWorkspaceCreation(),
	}

	// Act.
	hook, err := buildPostMergeHook(cfg, t.Logf)

	// Assert — a hard construction error, never a silently disabled handoff.
	if err == nil {
		t.Fatalf("buildPostMergeHook() error = nil, want a refusal")
	}
	if hook != nil {
		t.Fatalf("buildPostMergeHook() hook = %v, want none", hook)
	}
	if !strings.Contains(err.Error(), "live session") {
		t.Fatalf("buildPostMergeHook() error = %v, want it to name the missing liveness fact", err)
	}
}

func TestBuildPostMergeHookRefusesACreationBridgeWithoutPostprocessing(t *testing.T) {
	// Arrange — a creation bridge that cannot resolve a postprocessing prompt.
	cfg := AgentShimConfig{
		Prompts:           &fakePrompts{},
		WorkspaceCreation: creationBridgeOnly{newFakeWorkspaceCreation()},
	}

	// Act.
	hook, err := buildPostMergeHook(cfg, t.Logf)

	// Assert.
	if err == nil {
		t.Fatalf("buildPostMergeHook() error = nil, want a refusal")
	}
	if hook != nil {
		t.Fatalf("buildPostMergeHook() hook = %v, want none", hook)
	}
	if !strings.Contains(err.Error(), "postprocessing") {
		t.Fatalf("buildPostMergeHook() error = %v, want it to name the missing postprocessing source", err)
	}
}

func TestBuildPostMergeHookBindsBothDerivedDependencies(t *testing.T) {
	// Arrange — the ordinary wiring: a router that answers liveness and a
	// bridge that answers postprocessing.
	cfg := AgentShimConfig{
		Prompts:           &fakePrompts{},
		WorkspaceCreation: newFakeWorkspaceCreation(),
	}

	// Act.
	hook, err := buildPostMergeHook(cfg, t.Logf)

	// Assert.
	if err != nil {
		t.Fatalf("buildPostMergeHook() error = %v", err)
	}
	if hook == nil {
		t.Fatalf("buildPostMergeHook() hook = nil, want a notifier")
	}
}

func TestWireAgentShimBindsThePostMergeHook(t *testing.T) {
	// Arrange — the assembled surface, which must construct at all only
	// because the post-merge hook resolved.
	reg := openTestRegistry(t)

	// Act.
	shim, err := WireAgentShim(AgentShimConfig{
		Resumes:           &fakeResumes{},
		SSM:               openTestSSM(t, reg),
		Progress:          progress.New(progress.Options{Logf: func(string, ...any) {}}),
		Prompts:           &fakePrompts{},
		Turns:             &fakePrompts{},
		Lifecycle:         &fakeLifecycle{},
		SessionCommands:   &SessionCommandBinding{},
		WorkspaceCreation: newFakeWorkspaceCreation(),
		MergeLease:        stubMergeLease{},
		MergeQueue:        newTestMergeQueue(t),
		LogVerbosef:       t.Logf,
	})

	// Assert.
	if err != nil {
		t.Fatalf("WireAgentShim: %v", err)
	}
	defer shim.Close()
	if shim.MergeCoordinator == nil {
		t.Fatalf("WireAgentShim built no merge coordinator")
	}
}
