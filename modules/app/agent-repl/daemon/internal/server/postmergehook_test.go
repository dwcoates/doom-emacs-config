package server

import (
	"context"
	"errors"
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

func (stubPostMergeHook) AfterAction(merge.Request) (string, error) { return "", nil }

// Live makes the package's PromptRouter fake answer the liveness fact the
// post-merge parent handoff needs. It reports every workspace live, so a
// wiring test never depends on which workspaces a fake decided to know about.
func (f *fakePrompts) Live(string) bool { return true }

// PostprocessingPrompt makes the package's workspace-creation fake answer the
// creation-time postprocessing lookup. Its records are empty, which is the
// ordinary case: most workspaces are created without one.
func (f *fakeWorkspaceCreation) PostprocessingPrompt(string) (string, error) { return "", nil }

// BeforeWSMergePrompt makes the same fake answer the creation-time before-merge
// lookup, which comes out of the same records for the same reason.
func (f *fakeWorkspaceCreation) BeforeWSMergePrompt(string) (string, error) { return "", nil }

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

// recordingHook is a merge.PostMergeHook that remembers it ran and can be told
// to fail.
type recordingHook struct {
	ran int
	err error
	// prompt is the after-action text this hook reports, and lookupErr the
	// failure of reporting it. Both default to the common case: no prompt.
	prompt    string
	lookupErr error
}

func (h *recordingHook) AfterAction(merge.Request) (string, error) {
	return h.prompt, h.lookupErr
}

func (h *recordingHook) AfterMerged(context.Context, merge.Request) error {
	h.ran++
	return h.err
}

func TestFanOutPostMergeHookRunsEveryBoundHook(t *testing.T) {
	// Arrange.
	first, second := &recordingHook{}, &recordingHook{}
	fan := &fanOutPostMergeHook{logf: t.Logf, hooks: []namedHook{
		{name: "parent-handoff", hook: first},
		{name: "self-reload", hook: second},
	}}

	// Act.
	err := fan.AfterMerged(context.Background(), merge.Request{Workspace: "/ws"})

	// Assert.
	if err != nil {
		t.Fatalf("AfterMerged() error = %v", err)
	}
	if first.ran != 1 || second.ran != 1 {
		t.Fatalf("hook runs = (%d, %d), want both run exactly once", first.ran, second.ran)
	}
}

// A failing notifier must never cancel the redeploy: chaining them is how a
// merge lands with neither the parent told nor the stack rebuilt.
func TestFanOutPostMergeHookRunsTheSecondHookAfterTheFirstFails(t *testing.T) {
	// Arrange.
	first := &recordingHook{err: errors.New("parent unreachable")}
	second := &recordingHook{}
	fan := &fanOutPostMergeHook{logf: t.Logf, hooks: []namedHook{
		{name: "parent-handoff", hook: first},
		{name: "self-reload", hook: second},
	}}

	// Act.
	err := fan.AfterMerged(context.Background(), merge.Request{Workspace: "/ws"})

	// Assert.
	if err == nil {
		t.Fatalf("AfterMerged() error = nil, want the first hook's failure surfaced")
	}
	if second.ran != 1 {
		t.Fatalf("second hook runs = %d, want it run despite the first hook failing", second.ran)
	}
}

func TestFanOutPostMergeHookSurfacesEveryFailureIndependently(t *testing.T) {
	// Arrange — both halves fail, for different reasons.
	fan := &fanOutPostMergeHook{logf: t.Logf, hooks: []namedHook{
		{name: "parent-handoff", hook: &recordingHook{err: errors.New("parent unreachable")}},
		{name: "self-reload", hook: &recordingHook{err: errors.New("spawn refused")}},
	}}

	// Act.
	err := fan.AfterMerged(context.Background(), merge.Request{Workspace: "/ws"})

	// Assert.
	if err == nil {
		t.Fatalf("AfterMerged() error = nil, want both failures surfaced")
	}
	for _, want := range []string{"parent-handoff: parent unreachable", "self-reload: spawn refused"} {
		if !strings.Contains(err.Error(), want) {
			t.Fatalf("AfterMerged() error = %v, want it to name %q", err, want)
		}
	}
}

// A test binary lives in the build cache, so it has no checkout to redeploy and
// must never arm the trigger that bounces the live stack.
func TestBuildSelfReloadTriggerIsDisabledForABinaryOutsideACheckout(t *testing.T) {
	// Arrange — this very test binary.
	var records []string
	logf := func(format string, _ ...any) { records = append(records, format) }

	// Act.
	trigger, armed, err := buildSelfReloadTrigger(logf)

	// Assert.
	if err != nil {
		t.Fatalf("buildSelfReloadTrigger() error = %v", err)
	}
	if armed || trigger != nil {
		t.Fatalf("buildSelfReloadTrigger armed the redeploy from a test binary")
	}
	if len(records) != 1 || !strings.Contains(records[0], "self-merge redeploy DISABLED") {
		t.Fatalf("buildSelfReloadTrigger records = %v, want a loud disabled record", records)
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
		SessionDeaths:     stubSessionDeaths{},
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
