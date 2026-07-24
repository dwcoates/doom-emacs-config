package server

import (
	"context"
	"errors"
	"path/filepath"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/registry"
	"claude-repld/internal/workspace/merge"

	"google.golang.org/protobuf/types/known/structpb"
)

// --- fakes ----------------------------------------------------------------

type fakePrompts struct {
	prompted   []string
	interrupts []bool
	perms      []string
	err        error
}

func (f *fakePrompts) SubmitPrompt(_ context.Context, ws, text, _ string) error {
	f.prompted = append(f.prompted, ws+":"+text)
	return f.err
}
func (f *fakePrompts) Interrupt(_ context.Context, _ string, hard bool) error {
	f.interrupts = append(f.interrupts, hard)
	return f.err
}
func (f *fakePrompts) AnswerPermission(_ context.Context, _, permReqID string, _ bool, _ string, _ *structpb.Struct) error {
	f.perms = append(f.perms, permReqID)
	return f.err
}

type fakeMerges struct {
	merged  []string
	resumed []string
}

func (f *fakeMerges) Merge(_ context.Context, ws string) error {
	f.merged = append(f.merged, ws)
	return nil
}
func (f *fakeMerges) Resume(_ context.Context, ws string) error {
	f.resumed = append(f.resumed, ws)
	return nil
}

type fakeLifecycle struct {
	closed []string
	opened []string
}

func (f *fakeLifecycle) Close(_ context.Context, ws string) error {
	f.closed = append(f.closed, ws)
	return nil
}
func (f *fakeLifecycle) Open(_ context.Context, ws string) error {
	f.opened = append(f.opened, ws)
	return nil
}

func newTestHandler(t *testing.T) (*commandHandler, *fakePrompts, *fakeMerges, *fakeLifecycle) {
	t.Helper()
	p, m, l := &fakePrompts{}, &fakeMerges{}, &fakeLifecycle{}
	h, err := newCommandHandler(p, m, l, nil)
	if err != nil {
		t.Fatalf("newCommandHandler: %v", err)
	}
	return h, p, m, l
}

// --- dispatch tests -------------------------------------------------------

func TestCommandHandlerSubmitPromptRoutesToPrompts(t *testing.T) {
	// Arrange
	h, p, _, _ := newTestHandler(t)
	// Act
	err := h.SubmitPrompt(context.Background(), "ws1", "r1", &frontendv1.SubmitPromptCmd{Text: "hi"})
	// Assert
	if err != nil {
		t.Fatalf("err: %v", err)
	}
	if len(p.prompted) != 1 || p.prompted[0] != "ws1:hi" {
		t.Fatalf("prompted = %v", p.prompted)
	}
}

func TestCommandHandlerInterruptRoutesToPrompts(t *testing.T) {
	// Arrange
	h, p, _, _ := newTestHandler(t)
	// Act
	_ = h.Interrupt(context.Background(), "ws1", "r1", &frontendv1.InterruptCmd{Hard: true})
	// Assert
	if len(p.interrupts) != 1 || !p.interrupts[0] {
		t.Fatalf("interrupts = %v", p.interrupts)
	}
}

func TestCommandHandlerPermissionRoutesToPrompts(t *testing.T) {
	// Arrange
	h, p, _, _ := newTestHandler(t)
	// Act
	_ = h.AnswerPermission(context.Background(), "ws1", "r1", &frontendv1.PermissionAnswerCmd{PermissionRequestId: "perm-9"})
	// Assert
	if len(p.perms) != 1 || p.perms[0] != "perm-9" {
		t.Fatalf("perms = %v", p.perms)
	}
}

func TestCommandHandlerMergeRoutesToMerge(t *testing.T) {
	// Arrange
	h, _, m, _ := newTestHandler(t)
	// Act — no conflict_resolved_continue -> Merge.
	_ = h.MergeWorkspace(context.Background(), "ws1", "r1", &frontendv1.MergeWorkspaceCmd{})
	// Assert
	if len(m.merged) != 1 || m.merged[0] != "ws1" || len(m.resumed) != 0 {
		t.Fatalf("merged=%v resumed=%v", m.merged, m.resumed)
	}
}

func TestCommandHandlerMergeResolvedContinueRoutesToResume(t *testing.T) {
	// Arrange
	h, _, m, _ := newTestHandler(t)
	// Act — conflict_resolved_continue -> Resume.
	_ = h.MergeWorkspace(context.Background(), "ws1", "r1", &frontendv1.MergeWorkspaceCmd{ConflictResolvedContinue: true})
	// Assert
	if len(m.resumed) != 1 || m.resumed[0] != "ws1" || len(m.merged) != 0 {
		t.Fatalf("merged=%v resumed=%v", m.merged, m.resumed)
	}
}

func TestCommandHandlerCloseOpenRouteToLifecycle(t *testing.T) {
	// Arrange
	h, _, _, l := newTestHandler(t)
	// Act
	_ = h.CloseWorkspace(context.Background(), "ws1", "r1", &frontendv1.CloseWorkspaceCmd{})
	_ = h.OpenWorkspace(context.Background(), "ws2", "r2", &frontendv1.OpenWorkspaceCmd{})
	// Assert
	if len(l.closed) != 1 || l.closed[0] != "ws1" || len(l.opened) != 1 || l.opened[0] != "ws2" {
		t.Fatalf("closed=%v opened=%v", l.closed, l.opened)
	}
}

func TestCommandHandlerPromptErrorSurfaces(t *testing.T) {
	// Arrange — the prompt router fails.
	p := &fakePrompts{err: errors.New("no live shim")}
	h, err := newCommandHandler(p, &fakeMerges{}, &fakeLifecycle{}, nil)
	if err != nil {
		t.Fatalf("newCommandHandler: %v", err)
	}
	// Act
	got := h.SubmitPrompt(context.Background(), "ws1", "r1", &frontendv1.SubmitPromptCmd{Text: "hi"})
	// Assert — the error is surfaced, never swallowed.
	if got == nil {
		t.Fatal("want the prompt router error surfaced")
	}
}

func TestNewCommandHandlerRejectsNilDeps(t *testing.T) {
	// Arrange / Act / Assert
	if _, err := newCommandHandler(nil, &fakeMerges{}, &fakeLifecycle{}, nil); err == nil {
		t.Fatal("want error for nil PromptRouter")
	}
}

// --- snapshot provider ----------------------------------------------------

type fakeSessions struct{ views []*frontendv1.SessionView }

func (f fakeSessions) SessionViews() []*frontendv1.SessionView { return f.views }

func TestSnapshotProviderCombinesSSMAndSessions(t *testing.T) {
	// Arrange — an SSM with one workspace transition, plus a session view.
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{SessionID: "s1", CWD: "/w"}); err != nil {
		t.Fatalf("put: %v", err)
	}
	shim, err := WireAgentShim(AgentShimConfig{
		SSMDBPath: filepath.Join(t.TempDir(), "state.db"),
		Resolver:  NewRegistryResolver(reg),
		Prompts:   &fakePrompts{},
		MergeDirs: fakeMergeDirs{},
		Lifecycle: &fakeLifecycle{},
		Sessions:  fakeSessions{views: []*frontendv1.SessionView{{Workspace: "/w", SessionId: "s1", Model: "haiku"}}},
	})
	if err != nil {
		t.Fatalf("WireAgentShim: %v", err)
	}
	defer shim.Close()
	// Drive a merge transition so a workspace state exists.
	if err := shim.SSM.ApplyMergeTransition("/w", "merging", "test"); err != nil {
		t.Fatalf("apply merge transition: %v", err)
	}
	// Act
	provider := &ssmSnapshotProvider{ssm: shim.SSM, sessions: fakeSessions{views: []*frontendv1.SessionView{{Workspace: "/w", SessionId: "s1", Model: "haiku"}}}}
	snap := provider.Snapshot()
	// Assert
	if len(snap.GetWorkspaces()) != 1 || snap.GetWorkspaces()[0].GetWorkspace() != "/w" {
		t.Fatalf("workspaces = %v", snap.GetWorkspaces())
	}
	if len(snap.GetSessions()) != 1 || snap.GetSessions()[0].GetModel() != "haiku" {
		t.Fatalf("sessions = %v", snap.GetSessions())
	}
}

// --- merge runner ---------------------------------------------------------

type fakeMergeDirs struct{ err error }

func (f fakeMergeDirs) Resolve(workspace string) (merge.Request, error) {
	if f.err != nil {
		return merge.Request{}, f.err
	}
	return merge.Request{Workspace: workspace, SourceBranch: "b", SourceDir: "/s", TargetDir: "/t"}, nil
}

func TestMergeRunnerResolverErrorSurfaces(t *testing.T) {
	// Arrange — a resolver that cannot map the workspace to dirs.
	eng, err := merge.NewEngine(merge.Config{Logf: func(string, ...any) {}, Sink: noopSink{}})
	if err != nil {
		t.Fatalf("engine: %v", err)
	}
	runner := mergeRunner{engine: eng, resolver: fakeMergeDirs{err: errors.New("dirs unknown")}}
	// Act
	got := runner.Merge(context.Background(), "ws1")
	// Assert — the resolver failure surfaces, never a silent no-op merge.
	if got == nil {
		t.Fatal("want the dir-resolution error surfaced")
	}
}

type noopSink struct{}

func (noopSink) RecordMergeTransition(string, merge.Phase, string) error { return nil }

// --- wire assembly --------------------------------------------------------

func TestWireAgentShimRejectsNilResolver(t *testing.T) {
	// Arrange / Act / Assert
	if _, err := WireAgentShim(AgentShimConfig{MergeDirs: fakeMergeDirs{}}); err == nil {
		t.Fatal("want error for nil Resolver")
	}
}

func TestWireAgentShimMergeTransitionReachesSSM(t *testing.T) {
	// Arrange
	reg := openTestRegistry(t)
	shim, err := WireAgentShim(AgentShimConfig{
		SSMDBPath: filepath.Join(t.TempDir(), "state.db"),
		Resolver:  NewRegistryResolver(reg),
		Prompts:   &fakePrompts{},
		MergeDirs: fakeMergeDirs{},
		Lifecycle: &fakeLifecycle{},
	})
	if err != nil {
		t.Fatalf("WireAgentShim: %v", err)
	}
	defer shim.Close()
	// Act — the merge Engine's sink is the SSM, so a transition it emits lands
	// in the SSM's per-workspace log.
	if err := shim.Merge.MarkQueued("/w", "queued behind another merge"); err != nil {
		t.Fatalf("mark queued: %v", err)
	}
	// Assert
	cur, found, err := shim.SSM.Current("/w")
	if err != nil {
		t.Fatalf("current: %v", err)
	}
	if !found || cur.GetState() != frontendv1.RenderState_RENDER_STATE_MERGE_QUEUED {
		t.Fatalf("state found=%v state=%v, want MERGE_QUEUED", found, cur.GetState())
	}
}
