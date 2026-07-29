package server

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/progress"
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
		Logf:     func(string, ...any) {},
	})
	if err != nil {
		t.Fatalf("open ssm: %v", err)
	}
	t.Cleanup(func() { _ = mgr.Close() })
	return mgr
}

// --- fakes ----------------------------------------------------------------

type fakePrompts struct {
	prompted []string
	// promptRequestIDs records the request id each submit was routed WITH: it
	// is what the daemon keys the prompt receipt on, so dropping it would be
	// invisible to a test that only watched the text.
	promptRequestIDs []string
	interrupts       []string
	perms            []string
	err              error
	// turnActive is what this fake reports as the workspace's observed turn
	// state, so one double serves as both the prompt router and the interrupt
	// gate's turn source (the production wiring binds one driver to both).
	turnActive bool
	turnErr    error
}

// TurnActive makes the prompt double the gate's turn source.
func (f *fakePrompts) TurnActive(string) (bool, error) { return f.turnActive, f.turnErr }

// fakeLiveTasks is the gate's live-task source: a canned count, plus the
// explicit "this workspace is unknown" miss.
type fakeLiveTasks struct {
	count   int64
	unknown bool
}

func (f fakeLiveTasks) LiveTasks(string) (int64, bool) {
	if f.unknown {
		return 0, false
	}
	return f.count, true
}

// newGatedHandler builds a handler whose interrupt confirm gate is wired with
// the given turn state and live-task count.
func newGatedHandler(t *testing.T, turnActive bool, tasks LiveTaskSource) (*commandHandler, *fakePrompts) {
	t.Helper()
	p := &fakePrompts{turnActive: turnActive}
	h, err := newCommandHandler(p, &fakeMerges{}, &fakeLifecycle{}, nil, &fakeSessionCmds{}, nil, nil, nil,
		CommandHandlerConfig{Interrupt: InterruptGateConfig{Turns: p, LiveTasks: tasks}})
	if err != nil {
		t.Fatalf("newCommandHandler: %v", err)
	}
	return h, p
}

func (f *fakePrompts) SubmitPrompt(_ context.Context, ws, requestID, text, _ string) error {
	f.prompted = append(f.prompted, ws+":"+text)
	f.promptRequestIDs = append(f.promptRequestIDs, requestID)
	return f.err
}
func (f *fakePrompts) Interrupt(_ context.Context, ws string) error {
	f.interrupts = append(f.interrupts, ws)
	return f.err
}
func (f *fakePrompts) AnswerPermission(_ context.Context, _, permReqID string, _ bool, _ string, _ *structpb.Struct) error {
	f.perms = append(f.perms, permReqID)
	return f.err
}
func (f *fakePrompts) SetModel(_ context.Context, _ string, _ string) (string, error) {
	return "opus", f.err
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

type fakeHealthRouter struct {
	status    *corev1.HealthStatus
	err       error
	workspace string
	sessionID string
	requestID string
}

func (f *fakeHealthRouter) Health(_ context.Context, workspace, sessionID, requestID string) (*corev1.HealthStatus, error) {
	f.workspace, f.sessionID, f.requestID = workspace, sessionID, requestID
	return f.status, f.err
}

type fakeDaemonHealth struct {
	healthy bool
	reason  string
}

func (f fakeDaemonHealth) DaemonHealth() (bool, string) { return f.healthy, f.reason }

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

type hostActionCompletion struct {
	actionID string
	ok       bool
	failure  string
}

// fakeWorkspaceCreation is the fully durable host-work seam used by server
// tests. The two typed channels make an invalid host-work union impossible,
// matching the production bridge contract.
type fakeWorkspaceCreation struct {
	snapshot     WorkspaceHostWorkSnapshot
	available    chan *frontendv1.WorkspaceAvailable
	actions      chan *frontendv1.HostAction
	materialized []string
	completions  []hostActionCompletion
}

type fakeHostWorkPublisher struct {
	available []*frontendv1.WorkspaceAvailable
	actions   []*frontendv1.HostAction
}

func (f *fakeHostWorkPublisher) PushWorkspaceAvailable(available *frontendv1.WorkspaceAvailable) {
	f.available = append(f.available, available)
}

func (f *fakeHostWorkPublisher) PushHostAction(action *frontendv1.HostAction) {
	f.actions = append(f.actions, action)
}

func newFakeWorkspaceCreation() *fakeWorkspaceCreation {
	return &fakeWorkspaceCreation{
		available: make(chan *frontendv1.WorkspaceAvailable, 8),
		actions:   make(chan *frontendv1.HostAction, 8),
	}
}

func (f *fakeWorkspaceCreation) MarkWorkspaceMaterialized(_ context.Context, jobID string) error {
	f.materialized = append(f.materialized, jobID)
	return nil
}

func (f *fakeWorkspaceCreation) CompleteHostAction(_ context.Context, actionID string, ok bool, failure string) error {
	f.completions = append(f.completions, hostActionCompletion{actionID: actionID, ok: ok, failure: failure})
	return nil
}

func (f *fakeWorkspaceCreation) SnapshotHostWork() WorkspaceHostWorkSnapshot { return f.snapshot }

func (f *fakeWorkspaceCreation) SubscribeWorkspaceAvailable() (<-chan *frontendv1.WorkspaceAvailable, func()) {
	return f.available, func() {}
}

func (f *fakeWorkspaceCreation) SubscribeHostActions() (<-chan *frontendv1.HostAction, func()) {
	return f.actions, func() {}
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
	h, err := newCommandHandler(p, m, l, nil, &fakeSessionCmds{}, nil, nil, nil)
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
	err := h.SubmitPrompt(context.Background(), "/ws1", "r1", &frontendv1.SubmitPromptCmd{Text: "hi"})
	// Assert
	if err != nil {
		t.Fatalf("err: %v", err)
	}
	if len(p.prompted) != 1 || p.prompted[0] != "/ws1:hi" {
		t.Fatalf("prompted = %v", p.prompted)
	}
}

// TestCommandHandlerSubmitPromptCarriesTheRequestID pins the correlation the
// prompt receipt is built on: the driver cannot key a bubble on an id the
// handler kept to itself.
func TestCommandHandlerSubmitPromptCarriesTheRequestID(t *testing.T) {
	// Arrange
	h, p, _, _ := newTestHandler(t)
	// Act
	if err := h.SubmitPrompt(context.Background(), "/ws1", "r1", &frontendv1.SubmitPromptCmd{Text: "hi"}); err != nil {
		t.Fatalf("err: %v", err)
	}
	// Assert
	if len(p.promptRequestIDs) != 1 || p.promptRequestIDs[0] != "r1" {
		t.Fatalf("routed request ids = %v, want [r1]", p.promptRequestIDs)
	}
}

func TestCommandHandlerInterruptRoutesToPrompts(t *testing.T) {
	// Arrange — a turn is live, which the gate never challenges.
	h, p := newGatedHandler(t, true, fakeLiveTasks{})
	// Act
	_ = h.Interrupt(context.Background(), "/ws1", "r1", &frontendv1.InterruptCmd{})
	// Assert
	if len(p.interrupts) != 1 || p.interrupts[0] != "/ws1" {
		t.Fatalf("interrupts = %v", p.interrupts)
	}
}

func TestCommandHandlerPermissionRoutesToPrompts(t *testing.T) {
	// Arrange
	h, p, _, _ := newTestHandler(t)
	// Act
	_ = h.AnswerPermission(context.Background(), "/ws1", "r1", &frontendv1.PermissionAnswerCmd{PermissionRequestId: "perm-9"})
	// Assert
	if len(p.perms) != 1 || p.perms[0] != "perm-9" {
		t.Fatalf("perms = %v", p.perms)
	}
}

func TestCommandHandlerDaemonHealthReturnsExplicitReadiness(t *testing.T) {
	// Arrange.
	h, err := newCommandHandler(&fakePrompts{}, &fakeMerges{}, &fakeLifecycle{}, nil, &fakeSessionCmds{}, nil, nil, nil,
		CommandHandlerConfig{Health: HealthConfig{Daemon: fakeDaemonHealth{healthy: false, reason: "frontend UDS not listening"}}})
	if err != nil {
		t.Fatalf("newCommandHandler: %v", err)
	}

	// Act.
	view, err := h.DaemonHealth(context.Background(), "", "health-1", &frontendv1.DaemonHealthCmd{})

	// Assert.
	if err != nil || view.GetHealthy() || view.GetRequestId() != "health-1" || view.GetReason() != "frontend UDS not listening" {
		t.Fatalf("DaemonHealth = (%+v, %v)", view, err)
	}
}

func TestCommandHandlerSessionHealthForwardsExactIdentity(t *testing.T) {
	// Arrange.
	router := &fakeHealthRouter{status: &corev1.HealthStatus{RequestId: "health-2", Healthy: true, Component: "claude-shim"}}
	h, err := newCommandHandler(&fakePrompts{}, &fakeMerges{}, &fakeLifecycle{}, nil, &fakeSessionCmds{}, nil, nil, nil,
		CommandHandlerConfig{Health: HealthConfig{Router: router}})
	if err != nil {
		t.Fatalf("newCommandHandler: %v", err)
	}

	// Act.
	view, err := h.SessionHealth(context.Background(), "/ws", "health-2", &frontendv1.SessionHealthCmd{SessionId: "s1"})

	// Assert.
	if err != nil || !view.GetHealthy() || router.workspace != "/ws" || router.sessionID != "s1" || router.requestID != "health-2" {
		t.Fatalf("SessionHealth = (%+v, %v), routed=(%q,%q,%q)", view, err, router.workspace, router.sessionID, router.requestID)
	}
}

func TestCommandHandlerSessionHealthMakesMissingShimExplicitlyUnhealthy(t *testing.T) {
	// Arrange.
	router := &fakeHealthRouter{err: errors.New("shim not connected")}
	h, err := newCommandHandler(&fakePrompts{}, &fakeMerges{}, &fakeLifecycle{}, nil, &fakeSessionCmds{}, nil, nil, nil,
		CommandHandlerConfig{Health: HealthConfig{Router: router}})
	if err != nil {
		t.Fatalf("newCommandHandler: %v", err)
	}

	// Act.
	view, err := h.SessionHealth(context.Background(), "/ws", "health-3", &frontendv1.SessionHealthCmd{SessionId: "s1"})

	// Assert: a missing live shim is the false assertion Emacs waits for, not a
	// successful command with no health frame.
	if err != nil || view.GetHealthy() || !strings.Contains(view.GetReason(), "shim not connected") {
		t.Fatalf("SessionHealth = (%+v, %v)", view, err)
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
	h, err := newCommandHandler(p, &fakeMerges{}, &fakeLifecycle{}, nil, &fakeSessionCmds{}, nil, nil, nil)
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
	if _, err := newCommandHandler(nil, &fakeMerges{}, &fakeLifecycle{}, nil, &fakeSessionCmds{}, nil, nil, nil); err == nil {
		t.Fatal("want error for nil PromptRouter")
	}
}

func TestNewCommandHandlerRejectsNilSessions(t *testing.T) {
	// Arrange / Act / Assert — the session-lifecycle binding is required.
	if _, err := newCommandHandler(&fakePrompts{}, &fakeMerges{}, &fakeLifecycle{}, nil, nil, nil, nil, nil); err == nil {
		t.Fatal("want error for nil SessionCreateDeleter")
	}
}

func TestCommandHandlerCreateSessionRoutesToSessions(t *testing.T) {
	// Arrange
	sc := &fakeSessionCmds{}
	// The create acks only on an established session, so an opts-passthrough
	// assertion needs a shim that answers healthy.
	h := establishHandler(t, sc, &probeHealthRouter{healthy: true})
	// Act
	err := h.CreateSession(context.Background(), "/w", "r1", &frontendv1.CreateSessionCmd{Cwd: "/w"})
	// Assert — the create routes with its opts, never a silent drop.
	if err != nil {
		t.Fatalf("err: %v", err)
	}
	if len(sc.created) != 1 || sc.created[0].CWD != "/w" || sc.created[0].Model != "" {
		t.Fatalf("created = %v", sc.created)
	}
}

func TestCommandHandlerCarriesTheUngatedConsent(t *testing.T) {
	// Arrange — the consent is what admits a session with no permission gate,
	// so dropping it in the dispatch would turn every such create into a
	// refusal (or, worse, admit one nobody consented to).
	sc := &fakeSessionCmds{}
	// The create acks only on an established session, so an opts-passthrough
	// assertion needs a shim that answers healthy.
	h := establishHandler(t, sc, &probeHealthRouter{healthy: true})
	// Act
	err := h.CreateSession(context.Background(), "/w", "r1", &frontendv1.CreateSessionCmd{
		Cwd:            "/w",
		PermissionMode: "bypassPermissions",
		AllowUngated:   true,
	})
	// Assert
	if err != nil {
		t.Fatalf("err: %v", err)
	}
	if len(sc.created) != 1 || !sc.created[0].AllowUngated {
		t.Fatalf("created = %+v, want allow_ungated carried through", sc.created)
	}
}

func TestCommandHandlerWithholdsAnUnsetUngatedConsent(t *testing.T) {
	// Arrange
	sc := &fakeSessionCmds{}
	// The create acks only on an established session, so an opts-passthrough
	// assertion needs a shim that answers healthy.
	h := establishHandler(t, sc, &probeHealthRouter{healthy: true})
	// Act
	err := h.CreateSession(context.Background(), "/w", "r1", &frontendv1.CreateSessionCmd{Cwd: "/w"})
	// Assert — an ordinary create never fabricates the consent.
	if err != nil {
		t.Fatalf("err: %v", err)
	}
	if len(sc.created) != 1 || sc.created[0].AllowUngated {
		t.Fatalf("created = %+v, want allow_ungated false", sc.created)
	}
}

func TestCommandHandlerDeleteSessionRoutesToSessions(t *testing.T) {
	// Arrange
	sc := &fakeSessionCmds{}
	h, err := newCommandHandler(&fakePrompts{}, &fakeMerges{}, &fakeLifecycle{}, nil, sc, nil, nil, nil)
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

// newHandlerWithShutdown builds a command handler over a shutdown func that
// reports the stop-shims mode it was called with.
func newHandlerWithShutdown(t *testing.T, fired chan bool) *commandHandler {
	t.Helper()
	h, err := newCommandHandler(&fakePrompts{}, &fakeMerges{}, &fakeLifecycle{}, nil, &fakeSessionCmds{},
		func(stopShims bool) { fired <- stopShims }, nil, nil)
	if err != nil {
		t.Fatalf("newCommandHandler: %v", err)
	}
	return h
}

func TestCommandHandlerShutdownRoutesToShutdownFunc(t *testing.T) {
	// Arrange — a shutdown func that signals when invoked.
	fired := make(chan bool, 1)
	h := newHandlerWithShutdown(t, fired)

	// Act — the shutdown command routes to the same func SIGTERM drives.
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

// AN UNQUALIFIED SHUTDOWN PRESERVES THE SHIMS. This is the default the whole
// preserve-on-bounce contract rests on, so it is pinned at the command seam
// rather than left implied by the zero value.
func TestCommandHandlerShutdownDefaultsToPreservingShims(t *testing.T) {
	// Arrange.
	fired := make(chan bool, 1)
	h := newHandlerWithShutdown(t, fired)

	// Act.
	if err := h.Shutdown(context.Background(), "/w", "r1", &frontendv1.ShutdownCmd{}); err != nil {
		t.Fatalf("Shutdown: %v", err)
	}

	// Assert.
	select {
	case stopShims := <-fired:
		if stopShims {
			t.Fatal("an unqualified shutdown asked for stop-shims; the default must PRESERVE them")
		}
	case <-time.After(time.Second):
		t.Fatal("shutdown command did not invoke the graceful teardown func")
	}
}

// The stop-shims MODE reaches the teardown, which is what a bundle-changing
// deploy relies on.
func TestCommandHandlerShutdownCarriesStopShims(t *testing.T) {
	// Arrange.
	fired := make(chan bool, 1)
	h := newHandlerWithShutdown(t, fired)

	// Act.
	if err := h.Shutdown(context.Background(), "/w", "r1", &frontendv1.ShutdownCmd{StopShims: true}); err != nil {
		t.Fatalf("Shutdown: %v", err)
	}

	// Assert.
	select {
	case stopShims := <-fired:
		if !stopShims {
			t.Fatal("stop_shims=true did not reach the teardown")
		}
	case <-time.After(time.Second):
		t.Fatal("shutdown command did not invoke the graceful teardown func")
	}
}

func TestCommandHandlerShutdownUnconfiguredErrors(t *testing.T) {
	// Arrange — no shutdown func wired.
	h, err := newCommandHandler(&fakePrompts{}, &fakeMerges{}, &fakeLifecycle{}, nil, &fakeSessionCmds{}, nil, nil, nil)
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
	h, err := newCommandHandler(&fakePrompts{}, &fakeMerges{}, &fakeLifecycle{}, nil, sc, nil, nil, nil)
	if err != nil {
		t.Fatalf("newCommandHandler: %v", err)
	}
	// Act / Assert — the error is surfaced, never swallowed.
	if got := h.CreateSession(context.Background(), "/w", "r1", &frontendv1.CreateSessionCmd{Cwd: "/w"}); got == nil {
		t.Fatal("want the create error surfaced")
	}
}

func TestCommandHandlerRoutesDaemonOwnedWorkspaceWork(t *testing.T) {
	// Arrange — creation itself is not a wire command; what the host still
	// drives over the wire is the durable acknowledgement pair.
	bridge := newFakeWorkspaceCreation()
	h, err := newCommandHandler(&fakePrompts{}, &fakeMerges{}, &fakeLifecycle{}, nil, &fakeSessionCmds{}, nil, nil, nil,
		CommandHandlerConfig{WorkspaceCreation: bridge})
	if err != nil {
		t.Fatalf("newCommandHandler: %v", err)
	}

	// Act.
	if err := h.WorkspaceMaterialized(context.Background(), "", "request-2", &frontendv1.WorkspaceMaterializedCmd{JobId: "job-1"}); err != nil {
		t.Fatalf("WorkspaceMaterialized: %v", err)
	}
	if err := h.HostActionCompleted(context.Background(), "", "request-3", &frontendv1.HostActionCompletedCmd{ActionId: "action-1", Ok: false, Error: "Emacs rejected it"}); err != nil {
		t.Fatalf("HostActionCompleted: %v", err)
	}

	// Assert — all durable transitions are delegated exactly once, including a
	// failed completion (which the bridge preserves rather than dropping).
	if got := bridge.materialized; len(got) != 1 || got[0] != "job-1" {
		t.Fatalf("materialized = %v", got)
	}
	if got := bridge.completions; len(got) != 1 || got[0].actionID != "action-1" || got[0].ok || got[0].failure != "Emacs rejected it" {
		t.Fatalf("completions = %+v", got)
	}
}

func TestSnapshotProviderIncludesDurableHostWork(t *testing.T) {
	// Arrange.
	bridge := newFakeWorkspaceCreation()
	bridge.snapshot = WorkspaceHostWorkSnapshot{
		WorkspaceAvailable: []*frontendv1.WorkspaceAvailable{{JobId: "job-1", FinalName: "fresh", WorktreePath: "/repo-worktrees/fresh"}},
		HostActions: []*frontendv1.HostAction{{
			ActionId: "action-1",
			Action:   &frontendv1.HostAction_LegacyCommand{LegacyCommand: &frontendv1.HostLegacyCommand{Type: "prompt"}},
		}},
	}

	// Act.
	snap := (&ssmSnapshotProvider{workspaceCreation: bridge}).Snapshot()

	// Assert — reconnecting Emacs receives both retained collections before
	// relying on live subscriptions.
	if got := snap.GetWorkspaceAvailable(); len(got) != 1 || got[0].GetJobId() != "job-1" {
		t.Fatalf("workspace_available = %+v", got)
	}
	if got := snap.GetHostActions(); len(got) != 1 || got[0].GetLegacyCommand().GetType() != "prompt" {
		t.Fatalf("host_actions = %+v", got)
	}
}

func TestHostWorkForwardersPublishEachTypedStream(t *testing.T) {
	// Arrange — separate typed streams make it impossible for a host action to
	// masquerade as workspace availability or vice versa.
	publisher := &fakeHostWorkPublisher{}
	available := make(chan *frontendv1.WorkspaceAvailable, 1)
	actions := make(chan *frontendv1.HostAction, 1)
	available <- &frontendv1.WorkspaceAvailable{JobId: "job-1", FinalName: "fresh"}
	actions <- &frontendv1.HostAction{ActionId: "action-1"}
	close(available)
	close(actions)

	// Act.
	forwardWorkspaceAvailable(func(string, ...any) {}, publisher, available)
	forwardHostActions(func(string, ...any) {}, publisher, actions)

	// Assert.
	if got := publisher.available; len(got) != 1 || got[0].GetJobId() != "job-1" {
		t.Fatalf("workspace available pushes = %+v", got)
	}
	if got := publisher.actions; len(got) != 1 || got[0].GetActionId() != "action-1" {
		t.Fatalf("host action pushes = %+v", got)
	}
}

// --- snapshot provider ----------------------------------------------------

type fakeSessions struct{ views []*frontendv1.SessionView }

func (f fakeSessions) SessionViews() []*frontendv1.SessionView { return f.views }

type fakeInits struct{ inits []*frontendv1.SessionInitView }

func (f fakeInits) SessionInits() []*frontendv1.SessionInitView { return f.inits }

type fakeCatalogs struct{ catalogs []*frontendv1.TaskCatalog }

func (f fakeCatalogs) TaskCatalogs() []*frontendv1.TaskCatalog { return f.catalogs }

func TestSnapshotProviderIncludesSessionInits(t *testing.T) {
	// Arrange — a snapshot provider with a SessionInitSource (S9).
	provider := &ssmSnapshotProvider{
		workspaceCreation: newFakeWorkspaceCreation(),
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

func TestSnapshotProviderIncludesTaskCatalogsAndLogsTheirShape(t *testing.T) {
	// Arrange.
	var logs []string
	provider := &ssmSnapshotProvider{
		workspaceCreation: newFakeWorkspaceCreation(),
		catalogs: fakeCatalogs{catalogs: []*frontendv1.TaskCatalog{{
			Workspace: "/w", SessionId: "s1",
			Tasks: []*frontendv1.TaskEntry{{TaskId: "t1"}},
		}}},
		logf: func(format string, args ...any) {
			logs = append(logs, fmt.Sprintf(format, args...))
		},
	}

	// Act.
	snap := provider.Snapshot()

	// Assert.
	if len(snap.GetCatalogs()) != 1 || snap.GetCatalogs()[0].GetTasks()[0].GetTaskId() != "t1" {
		t.Fatalf("catalogs = %v, want t1", snap.GetCatalogs())
	}
	if len(logs) != 1 || !strings.Contains(logs[0], "catalogs=1 tasks=1") {
		t.Fatalf("snapshot logs = %v, want catalog/task counts", logs)
	}
}

func TestSnapshotProviderCombinesSSMAndSessions(t *testing.T) {
	// Arrange — an SSM with one workspace transition, plus a session view.
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{SessionID: "s1", CWD: "/w"}); err != nil {
		t.Fatalf("put: %v", err)
	}
	shim, err := WireAgentShim(AgentShimConfig{
		SSM:               openTestSSM(t, reg),
		Progress:          progress.New(progress.Options{Logf: func(string, ...any) {}}),
		Prompts:           &fakePrompts{},
		Turns:             &fakePrompts{},
		MergeDirs:         fakeMergeDirs{},
		Lifecycle:         &fakeLifecycle{},
		Sessions:          fakeSessions{views: []*frontendv1.SessionView{{Workspace: "/w", SessionId: "s1", Model: "haiku"}}},
		SessionCommands:   &SessionCommandBinding{},
		WorkspaceCreation: newFakeWorkspaceCreation(),
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
	provider := &ssmSnapshotProvider{ssm: shim.SSM, sessions: fakeSessions{views: []*frontendv1.SessionView{{Workspace: "/w", SessionId: "s1", Model: "haiku"}}}, workspaceCreation: newFakeWorkspaceCreation()}
	snap := provider.Snapshot()
	// Assert
	if len(snap.GetWorkspaces()) != 1 || snap.GetWorkspaces()[0].GetWorkspace() != "/w" {
		t.Fatalf("workspaces = %v", snap.GetWorkspaces())
	}
	if len(snap.GetSessions()) != 1 || snap.GetSessions()[0].GetModel() != "haiku" {
		t.Fatalf("sessions = %v", snap.GetSessions())
	}
}

func TestSnapshotProviderCarriesTheProgressViews(t *testing.T) {
	// Arrange — a resolver holding one workspace's progress.
	prog := progress.New(progress.Options{Logf: func(string, ...any) {}})
	defer prog.Close()
	prog.SetCounts("/w", 2, 1)
	// Act — a (re)connecting frontend's footer must start populated.
	provider := &ssmSnapshotProvider{progress: prog, workspaceCreation: newFakeWorkspaceCreation()}
	snap := provider.Snapshot()
	// Assert
	if len(snap.GetProgress()) != 1 || snap.GetProgress()[0].GetPendingPermissions() != 2 {
		t.Fatalf("progress = %v, want the resolved view for /w", snap.GetProgress())
	}
}

// The seam between the two resolvers still fires — an SSM transition reaches
// the progress resolver through the same loop that pushes the WorkspaceState
// frame — but it no longer carries a PHASE. The footer reads the phase off the
// WorkspaceState, so a copy here would be a second, staler answer to a question
// the SSM has already answered.
func TestWireAgentShimFeedsTheSsmTransitionIntoProgressWithoutAPhaseCopy(t *testing.T) {
	// Arrange.
	reg := openTestRegistry(t)
	prog := progress.New(progress.Options{Logf: func(string, ...any) {}})
	shim, err := WireAgentShim(AgentShimConfig{
		SSM:               openTestSSM(t, reg),
		Progress:          prog,
		Prompts:           &fakePrompts{},
		Turns:             &fakePrompts{},
		MergeDirs:         fakeMergeDirs{},
		Lifecycle:         &fakeLifecycle{},
		SessionCommands:   &SessionCommandBinding{},
		WorkspaceCreation: newFakeWorkspaceCreation(),
	})
	if err != nil {
		t.Fatalf("WireAgentShim: %v", err)
	}
	defer shim.Close()
	views, cancel := prog.Subscribe()
	defer cancel()
	// Act — an SSM transition feeds the progress resolver through the same loop
	// that pushes the WorkspaceState frame.
	if err := shim.SSM.ApplyMergeTransition("/w", "merging", "test"); err != nil {
		t.Fatalf("apply merge transition: %v", err)
	}
	// Assert
	select {
	case v := <-views:
		if v.GetWorkspace() != "/w" {
			t.Fatalf("progress view = %v, want the transition's workspace", v)
		}
		if v.GetState() != frontendv1.RenderState_RENDER_STATE_UNSPECIFIED {
			t.Fatalf("progress state = %v, want UNSPECIFIED (the phase is not mirrored)", v.GetState())
		}
	case <-time.After(2 * time.Second):
		t.Fatal("timed out waiting for the SSM transition to reach the progress resolver")
	}
}

func TestWireAgentShimRejectsNilProgress(t *testing.T) {
	// Arrange / Act / Assert — an unwired footer resolver is a construction
	// error, not a silently progress-free daemon.
	reg := openTestRegistry(t)
	_, err := WireAgentShim(AgentShimConfig{
		SSM:             openTestSSM(t, reg),
		MergeDirs:       fakeMergeDirs{},
		SessionCommands: &SessionCommandBinding{},
	})
	if err == nil {
		t.Fatal("want a construction error for a nil progress resolver")
	}
}

func TestWireAgentShimRejectsNilWorkspaceCreation(t *testing.T) {
	reg := openTestRegistry(t)
	_, err := WireAgentShim(AgentShimConfig{
		SSM:             openTestSSM(t, reg),
		Progress:        progress.New(progress.Options{Logf: func(string, ...any) {}}),
		Prompts:         &fakePrompts{},
		MergeDirs:       fakeMergeDirs{},
		Lifecycle:       &fakeLifecycle{},
		SessionCommands: &SessionCommandBinding{},
	})
	if err == nil || !strings.Contains(err.Error(), "WorkspaceCreation") {
		t.Fatalf("WireAgentShim error = %v, want missing WorkspaceCreation bridge", err)
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
		SSM:               openTestSSM(t, reg),
		Progress:          progress.New(progress.Options{Logf: func(string, ...any) {}}),
		Prompts:           &fakePrompts{},
		Turns:             &fakePrompts{},
		MergeDirs:         fakeMergeDirs{},
		Lifecycle:         &fakeLifecycle{},
		SessionCommands:   &SessionCommandBinding{},
		WorkspaceCreation: newFakeWorkspaceCreation(),
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

// --- ClientLog (E4) ---------------------------------------------------------

type fakeClientLogWriter struct {
	workspace string
	requestID string
	cmd       *frontendv1.ClientLogCmd
	err       error
}

type fakeClientLogIdentityResolver struct {
	identity ClientLogSessionIdentity
	known    bool
}

func (f fakeClientLogIdentityResolver) ResolveClientLogIdentity(string) (ClientLogSessionIdentity, bool) {
	return f.identity, f.known
}

func (f *fakeClientLogWriter) PersistClientLog(workspace, requestID string, cmd *frontendv1.ClientLogCmd) error {
	f.workspace, f.requestID, f.cmd = workspace, requestID, cmd
	return f.err
}

func newLoggingHandler(t *testing.T, writer ClientLogWriter) *commandHandler {
	t.Helper()
	h, err := newCommandHandler(&fakePrompts{}, &fakeMerges{}, &fakeLifecycle{}, nil, &fakeSessionCmds{}, nil, nil,
		t.Logf)
	if err != nil {
		t.Fatalf("newCommandHandler: %v", err)
	}
	h.clientLogs = writer
	return h
}

func TestCommandHandlerClientLogForwardsOnlyToDedicatedWriter(t *testing.T) {
	writer := &fakeClientLogWriter{}
	h := newLoggingHandler(t, writer)
	ctx, err := structpb.NewStruct(map[string]any{"runtime": "webapp"})
	if err != nil {
		t.Fatal(err)
	}

	if err := h.ClientLog(context.Background(), "/w", "r1", &frontendv1.ClientLogCmd{Level: frontendv1.ClientLogLevel_CLIENT_LOG_LEVEL_WARN, Message: "seq gap at 42", Context: ctx}); err != nil {
		t.Fatalf("ClientLog: %v", err)
	}
	if writer.workspace != "/w" || writer.requestID != "r1" || writer.cmd.GetMessage() != "seq gap at 42" {
		t.Fatalf("writer got workspace=%q request=%q cmd=%+v", writer.workspace, writer.requestID, writer.cmd)
	}
}

func TestCommandHandlerClientLogFailsWhenPersistenceIsUnwired(t *testing.T) {
	h := newLoggingHandler(t, nil)
	err := h.ClientLog(context.Background(), "/w", "r1", &frontendv1.ClientLogCmd{})
	if err == nil {
		t.Fatal("want a loud error when client-log persistence is unwired")
	}
}

func TestTargetClientLogWriterPersistsCanonicalWebappRecord(t *testing.T) {
	workspace := t.TempDir()
	targets := dlog.NewTargetManager()
	writer, err := NewTargetClientLogWriter(targets, fakeClientLogIdentityResolver{
		identity: ClientLogSessionIdentity{AgentReplSessionID: "session-1", ClaudeSessionID: "claude-1"},
		known:    true,
	}, io.Discard, false)
	if err != nil {
		t.Fatal(err)
	}
	context, err := structpb.NewStruct(map[string]any{
		"timestamp": "2026-07-28T12:00:00.123Z", "runtime": "webapp", "level": "warn", "verbosity": "normal",
		"operation": "webapp.render.failed", "message": "render failed", "context": map[string]any{"cause": "x"}, "connection_id": "connection-1",
		"agent_repl_session_id": "session-1", "claude_session_id": "claude-1",
	})
	if err != nil {
		t.Fatal(err)
	}
	if err := writer.PersistClientLog(workspace, "request-1", &frontendv1.ClientLogCmd{Level: frontendv1.ClientLogLevel_CLIENT_LOG_LEVEL_WARN, Message: "render failed", Context: context}); err != nil {
		t.Fatal(err)
	}
	ws, err := dlog.WorkspaceFromDirectory(workspace)
	if err != nil {
		t.Fatal(err)
	}
	raw, err := os.ReadFile(filepath.Join(workspace, ".claude", "emacs", "webapp.log"))
	if err != nil {
		t.Fatal(err)
	}
	var record dlog.Record
	if err := json.Unmarshal(raw, &record); err != nil {
		t.Fatal(err)
	}
	if record.WorkspaceDirectory != ws.Directory || record.WorkspaceID != ws.ID || record.RequestID != "request-1" || record.ConnectionID != "connection-1" ||
		record.AgentReplSessionID != "session-1" || record.ClaudeSessionID != "claude-1" {
		t.Fatalf("persisted record=%+v", record)
	}
	if err := targets.Close(); err != nil {
		t.Fatal(err)
	}
}

func TestTargetClientLogWriterRejectsMismatchedCommandMetadata(t *testing.T) {
	writer, err := NewTargetClientLogWriter(dlog.NewTargetManager(), fakeClientLogIdentityResolver{}, io.Discard, false)
	if err != nil {
		t.Fatal(err)
	}
	context, err := structpb.NewStruct(map[string]any{
		"timestamp": "2026-07-28T12:00:00Z", "runtime": "webapp", "level": "info", "verbosity": "normal",
		"operation": "webapp.x", "message": "source", "context": map[string]any{}, "connection_id": "connection-1",
	})
	if err != nil {
		t.Fatal(err)
	}
	if err := writer.PersistClientLog(t.TempDir(), "request-1", &frontendv1.ClientLogCmd{Level: frontendv1.ClientLogLevel_CLIENT_LOG_LEVEL_WARN, Message: "command", Context: context}); err == nil {
		t.Fatal("mismatched source record was accepted")
	}
}

func TestTargetClientLogWriterRejectsMismatchedSourceWorkspace(t *testing.T) {
	writer, err := NewTargetClientLogWriter(dlog.NewTargetManager(), fakeClientLogIdentityResolver{}, io.Discard, false)
	if err != nil {
		t.Fatal(err)
	}
	context, err := structpb.NewStruct(map[string]any{
		"timestamp": "2026-07-28T12:00:00Z", "runtime": "webapp", "level": "info", "verbosity": "normal",
		"operation": "webapp.x", "message": "source", "context": map[string]any{}, "connection_id": "connection-1",
		"workspace_dir": "/not-the-command-workspace", "workspace_id": "wrong-id",
	})
	if err != nil {
		t.Fatal(err)
	}
	if err := writer.PersistClientLog(t.TempDir(), "request-1", &frontendv1.ClientLogCmd{Level: frontendv1.ClientLogLevel_CLIENT_LOG_LEVEL_INFO, Message: "source", Context: context}); err == nil {
		t.Fatal("mismatched source workspace was accepted")
	}
}

func TestCommandHandlerClientLogChangesNoDaemonState(t *testing.T) {
	// Arrange — a client log is EVIDENCE, never a control signal.
	p := &fakePrompts{}
	writer := &fakeClientLogWriter{}
	h, err := newCommandHandler(p, &fakeMerges{}, &fakeLifecycle{}, nil, &fakeSessionCmds{}, nil, nil, t.Logf)
	if err != nil {
		t.Fatal(err)
	}
	h.clientLogs = writer

	// Act.
	_ = h.ClientLog(context.Background(), "/w", "r1", &frontendv1.ClientLogCmd{})

	// Assert.
	if len(p.prompted) != 0 || len(p.interrupts) != 0 {
		t.Fatal("a client log must not drive the prompt router")
	}
}

// --- workspace-key validation ----------------------------------------------
//
// Session-routed commands are keyed by the session CWD. A display name matches
// no record, so it must be refused AT THE BOUNDARY with a message that names
// the wire-contract violation — not forwarded, where it degrades into an
// indistinguishable "no live session to drive" (the 2026-07-25 regression).

func TestSubmitPromptRejectsANonAbsoluteWorkspaceKey(t *testing.T) {
	// Arrange
	h, p, _, _ := newTestHandler(t)
	// Act
	err := h.SubmitPrompt(context.Background(), "doom", "r1", &frontendv1.SubmitPromptCmd{Text: "hi"})
	// Assert
	if err == nil {
		t.Fatal("a display-name workspace key must be refused")
	}
	if len(p.prompted) != 0 {
		t.Fatalf("prompted = %v, want nothing forwarded", p.prompted)
	}
}

func TestSubmitPromptRejectionNamesTheContractViolation(t *testing.T) {
	// Arrange
	h, _, _, _ := newTestHandler(t)
	// Act
	err := h.SubmitPrompt(context.Background(), "doom", "r1", &frontendv1.SubmitPromptCmd{Text: "hi"})
	// Assert: the message must not read as a dead session.
	if err == nil || !strings.Contains(err.Error(), "not an absolute path") {
		t.Fatalf("err = %v, want it to name the non-absolute key", err)
	}
}

func TestInterruptRejectsANonAbsoluteWorkspaceKey(t *testing.T) {
	// Arrange
	h, p, _, _ := newTestHandler(t)
	// Act
	err := h.Interrupt(context.Background(), "doom", "r1", &frontendv1.InterruptCmd{})
	// Assert
	if err == nil {
		t.Fatal("a display-name workspace key must be refused")
	}
	if len(p.interrupts) != 0 {
		t.Fatalf("interrupts = %v, want nothing forwarded", p.interrupts)
	}
}

func TestAnswerPermissionRejectsANonAbsoluteWorkspaceKey(t *testing.T) {
	// Arrange
	h, p, _, _ := newTestHandler(t)
	// Act
	err := h.AnswerPermission(context.Background(), "doom", "r1", &frontendv1.PermissionAnswerCmd{PermissionRequestId: "perm-9"})
	// Assert
	if err == nil {
		t.Fatal("a display-name workspace key must be refused")
	}
	if len(p.perms) != 0 {
		t.Fatalf("perms = %v, want nothing forwarded", p.perms)
	}
}

func TestSubmitPromptAcceptsAnAbsoluteWorkspaceKey(t *testing.T) {
	// Arrange
	h, p, _, _ := newTestHandler(t)
	// Act
	if err := h.SubmitPrompt(context.Background(), "/Users/x/.config/doom", "r1", &frontendv1.SubmitPromptCmd{Text: "hi"}); err != nil {
		t.Fatalf("err: %v", err)
	}
	// Assert
	if len(p.prompted) != 1 {
		t.Fatalf("prompted = %v, want the cwd-keyed prompt forwarded", p.prompted)
	}
}

// --- restartSession -------------------------------------------------------
//
// The command is SYNCHRONOUS on purpose: the ack is the user's only report of
// whether their session came back, so a failed restart must nack.

type fakeRestarter struct {
	calls []string
	err   error
}

func (f *fakeRestarter) RestartSession(_ context.Context, workspace string) error {
	f.calls = append(f.calls, workspace)
	return f.err
}

func TestCommandHandlerRestartSessionRoutesToTheRestarter(t *testing.T) {
	// Arrange.
	r := &fakeRestarter{}
	h, err := newCommandHandler(&fakePrompts{}, &fakeMerges{}, &fakeLifecycle{}, nil, &fakeSessionCmds{}, nil, nil, nil,
		CommandHandlerConfig{Restarts: r})
	if err != nil {
		t.Fatalf("newCommandHandler: %v", err)
	}

	// Act.
	if err := h.RestartSession(context.Background(), "/w", "r1", &frontendv1.RestartSessionCmd{}); err != nil {
		t.Fatalf("RestartSession: %v", err)
	}

	// Assert.
	if len(r.calls) != 1 || r.calls[0] != "/w" {
		t.Fatalf("restart calls = %v, want exactly one for /w", r.calls)
	}
}

func TestCommandHandlerRestartSessionNacksAFailedRestart(t *testing.T) {
	// Arrange.
	r := &fakeRestarter{err: errors.New("the shim never came back")}
	h, err := newCommandHandler(&fakePrompts{}, &fakeMerges{}, &fakeLifecycle{}, nil, &fakeSessionCmds{}, nil, nil, nil,
		CommandHandlerConfig{Restarts: r})
	if err != nil {
		t.Fatalf("newCommandHandler: %v", err)
	}

	// Act.
	err = h.RestartSession(context.Background(), "/w", "r1", &frontendv1.RestartSessionCmd{})

	// Assert — silence here would tell the user a dead workspace came back.
	if err == nil {
		t.Fatal("a failed restart returned ok")
	}
}

func TestCommandHandlerRestartSessionUnconfiguredErrors(t *testing.T) {
	// Arrange — no restarter wired.
	h, err := newCommandHandler(&fakePrompts{}, &fakeMerges{}, &fakeLifecycle{}, nil, &fakeSessionCmds{}, nil, nil, nil)
	if err != nil {
		t.Fatalf("newCommandHandler: %v", err)
	}

	// Act / Assert — an unconfigured capability is a loud failing ack, never a
	// success-shaped no-op.
	if err := h.RestartSession(context.Background(), "/w", "r1", &frontendv1.RestartSessionCmd{}); err == nil {
		t.Fatal("an unwired restart capability reported success")
	}
}
