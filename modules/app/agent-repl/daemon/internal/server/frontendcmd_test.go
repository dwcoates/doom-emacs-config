package server

import (
	"context"
	"errors"
	"path/filepath"
	"testing"
	"time"

	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/registry"
	"claude-repld/internal/ssm"
	"claude-repld/internal/workspace/merge"

	"google.golang.org/protobuf/types/known/structpb"
)

// openTestSSM opens an isolated SSM over a temp db bound to reg, closed at
// test end. main opens the SSM in production; WireAgentShim now takes it
// injected, so the tests open their own.
func openTestSSM(t *testing.T, reg *registry.Registry) *ssm.Manager {
	t.Helper()
	mgr, err := ssm.Open(ssm.Options{
		DBPath:   filepath.Join(t.TempDir(), "state.db"),
		Resolver: NewRegistryResolver(reg),
	})
	if err != nil {
		t.Fatalf("open ssm: %v", err)
	}
	t.Cleanup(func() { _ = mgr.Close() })
	return mgr
}

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

// fakeSessionCmds records createSession/deleteSession routing and can inject an
// error (the SessionCreateDeleter dispatch fake).
type fakeSessionCmds struct {
	created []CreateOpts
	deleted []string
	err     error
}

func (f *fakeSessionCmds) CreateSession(_ context.Context, opts CreateOpts) (string, error) {
	f.created = append(f.created, opts)
	return "s_test", f.err
}
func (f *fakeSessionCmds) DeleteSession(id string) error {
	f.deleted = append(f.deleted, id)
	return f.err
}

func newTestHandler(t *testing.T) (*commandHandler, *fakePrompts, *fakeMerges, *fakeLifecycle) {
	t.Helper()
	p, m, l := &fakePrompts{}, &fakeMerges{}, &fakeLifecycle{}
	h, err := newCommandHandler(p, m, l, nil, &fakeSessionCmds{}, nil, nil)
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
	h, err := newCommandHandler(p, &fakeMerges{}, &fakeLifecycle{}, nil, &fakeSessionCmds{}, nil, nil)
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
	if _, err := newCommandHandler(nil, &fakeMerges{}, &fakeLifecycle{}, nil, &fakeSessionCmds{}, nil, nil); err == nil {
		t.Fatal("want error for nil PromptRouter")
	}
}

func TestNewCommandHandlerRejectsNilSessions(t *testing.T) {
	// Arrange / Act / Assert — the session-lifecycle binding is required.
	if _, err := newCommandHandler(&fakePrompts{}, &fakeMerges{}, &fakeLifecycle{}, nil, nil, nil, nil); err == nil {
		t.Fatal("want error for nil SessionCreateDeleter")
	}
}

func TestCommandHandlerCreateSessionRoutesToSessions(t *testing.T) {
	// Arrange
	sc := &fakeSessionCmds{}
	h, err := newCommandHandler(&fakePrompts{}, &fakeMerges{}, &fakeLifecycle{}, nil, sc, nil, nil)
	if err != nil {
		t.Fatalf("newCommandHandler: %v", err)
	}
	// Act
	err = h.CreateSession(context.Background(), "/w", "r1", &frontendv1.CreateSessionCmd{Cwd: "/w", Model: "haiku"})
	// Assert — the create routes with its opts, never a silent drop.
	if err != nil {
		t.Fatalf("err: %v", err)
	}
	if len(sc.created) != 1 || sc.created[0].CWD != "/w" || sc.created[0].Model != "haiku" {
		t.Fatalf("created = %v", sc.created)
	}
}

func TestCommandHandlerDeleteSessionRoutesToSessions(t *testing.T) {
	// Arrange
	sc := &fakeSessionCmds{}
	h, err := newCommandHandler(&fakePrompts{}, &fakeMerges{}, &fakeLifecycle{}, nil, sc, nil, nil)
	if err != nil {
		t.Fatalf("newCommandHandler: %v", err)
	}
	// Act
	err = h.DeleteSession(context.Background(), "/w", "r1", &frontendv1.DeleteSessionCmd{SessionId: "s_9"})
	// Assert
	if err != nil {
		t.Fatalf("err: %v", err)
	}
	if len(sc.deleted) != 1 || sc.deleted[0] != "s_9" {
		t.Fatalf("deleted = %v", sc.deleted)
	}
}

func TestCommandHandlerShutdownRoutesToShutdownFunc(t *testing.T) {
	// Arrange — a shutdown func that signals when invoked.
	fired := make(chan struct{}, 1)
	newHandlerWithShutdown := func(shutdown func()) *commandHandler {
		h, err := newCommandHandler(&fakePrompts{}, &fakeMerges{}, &fakeLifecycle{}, nil, &fakeSessionCmds{}, shutdown, nil)
		if err != nil {
			t.Fatalf("newCommandHandler: %v", err)
		}
		return h
	}
	h := newHandlerWithShutdown(func() { fired <- struct{}{} })

	// Act — the shutdown command routes to the same func POST /shutdown drives.
	if err := h.Shutdown(context.Background(), "/w", "r1", &frontendv1.ShutdownCmd{}); err != nil {
		t.Fatalf("Shutdown: %v", err)
	}

	// Assert — the graceful teardown was requested (async).
	select {
	case <-fired:
	case <-time.After(time.Second):
		t.Fatal("shutdown command did not invoke the graceful teardown func")
	}
}

func TestCommandHandlerShutdownUnconfiguredErrors(t *testing.T) {
	// Arrange — no shutdown func wired.
	h, err := newCommandHandler(&fakePrompts{}, &fakeMerges{}, &fakeLifecycle{}, nil, &fakeSessionCmds{}, nil, nil)
	if err != nil {
		t.Fatalf("newCommandHandler: %v", err)
	}

	// Act / Assert — an unconfigured shutdown is a loud error, never a silent no-op.
	if got := h.Shutdown(context.Background(), "/w", "r1", &frontendv1.ShutdownCmd{}); got == nil {
		t.Fatal("want a loud error when shutdown is unconfigured")
	}
}

func TestCommandHandlerCreateSessionErrorSurfaces(t *testing.T) {
	// Arrange — the session core fails.
	sc := &fakeSessionCmds{err: errors.New("bring up shim failed")}
	h, err := newCommandHandler(&fakePrompts{}, &fakeMerges{}, &fakeLifecycle{}, nil, sc, nil, nil)
	if err != nil {
		t.Fatalf("newCommandHandler: %v", err)
	}
	// Act / Assert — the error is surfaced, never swallowed.
	if got := h.CreateSession(context.Background(), "/w", "r1", &frontendv1.CreateSessionCmd{Cwd: "/w"}); got == nil {
		t.Fatal("want the create error surfaced")
	}
}

// --- snapshot provider ----------------------------------------------------

type fakeSessions struct{ views []*frontendv1.SessionView }

func (f fakeSessions) SessionViews() []*frontendv1.SessionView { return f.views }

type fakeInits struct{ inits []*frontendv1.SessionInitView }

func (f fakeInits) SessionInits() []*frontendv1.SessionInitView { return f.inits }

func TestSnapshotProviderIncludesSessionInits(t *testing.T) {
	// Arrange — a snapshot provider with a SessionInitSource (S9).
	provider := &ssmSnapshotProvider{
		inits: fakeInits{inits: []*frontendv1.SessionInitView{
			{Workspace: "/w", SessionId: "s1", Init: &datav1.SystemInit{Model: "haiku"}},
		}},
	}

	// Act.
	snap := provider.Snapshot()

	// Assert — the retained inits ride on the connect snapshot.
	if len(snap.GetInits()) != 1 || snap.GetInits()[0].GetInit().GetModel() != "haiku" {
		t.Fatalf("inits = %v", snap.GetInits())
	}
}

func TestSnapshotProviderCombinesSSMAndSessions(t *testing.T) {
	// Arrange — an SSM with one workspace transition, plus a session view.
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{SessionID: "s1", CWD: "/w"}); err != nil {
		t.Fatalf("put: %v", err)
	}
	shim, err := WireAgentShim(AgentShimConfig{
		SSM:             openTestSSM(t, reg),
		Prompts:         &fakePrompts{},
		MergeDirs:       fakeMergeDirs{},
		Lifecycle:       &fakeLifecycle{},
		Sessions:        fakeSessions{views: []*frontendv1.SessionView{{Workspace: "/w", SessionId: "s1", Model: "haiku"}}},
		SessionCommands: &SessionCommandBinding{},
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

func TestWireAgentShimRejectsNilSSM(t *testing.T) {
	// Arrange / Act / Assert — the SSM is now injected; a nil one is a
	// construction error rather than a nil-deref later.
	if _, err := WireAgentShim(AgentShimConfig{MergeDirs: fakeMergeDirs{}}); err == nil {
		t.Fatal("want error for nil SSM")
	}
}

func TestWireAgentShimMergeTransitionReachesSSM(t *testing.T) {
	// Arrange
	reg := openTestRegistry(t)
	shim, err := WireAgentShim(AgentShimConfig{
		SSM:             openTestSSM(t, reg),
		Prompts:         &fakePrompts{},
		MergeDirs:       fakeMergeDirs{},
		Lifecycle:       &fakeLifecycle{},
		SessionCommands: &SessionCommandBinding{},
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
