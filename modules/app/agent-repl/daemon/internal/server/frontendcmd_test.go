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
	"sync"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/errclass"
	"claude-repld/internal/progress"
	"claude-repld/internal/registry"
	"claude-repld/internal/sessioncontroller"
	"claude-repld/internal/ssm"
	"claude-repld/internal/workspace/geometry"
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
	promptOrigins    []corev1.PromptOrigin
	interrupts       []string
	perms            []string
	models           []string
	err              error
	// turnActive is what this fake reports as the workspace's observed turn
	// state, so one double serves as both the prompt router and the interrupt
	// gate's turn source (the production wiring binds one controller to both).
	turnActive bool
	turnErr    error
	// resolutions records every merge conflict this double was asked to drive
	// the workspace's own session through.
	resolutions []merge.ConflictResolution
	// testFailures records every merge test failure this double was asked to
	// drive the workspace's own session through.
	testFailures  []merge.TestFailureResolution
	beforeActions []merge.BeforeAction
	afterActions  []merge.AfterAction
}

// TurnActive makes the prompt double the gate's turn source.
func (f *fakePrompts) TurnActive(string) (bool, error) { return f.turnActive, f.turnErr }

// ResolveMergeConflict makes the prompt double merge.Coordinator's conflict
// resolver too, exactly as the production controller is both. It records the
// resolutions it was asked to drive and reports each as a completed turn.
func (f *fakePrompts) ResolveMergeConflict(_ context.Context, res merge.ConflictResolution) error {
	f.resolutions = append(f.resolutions, res)
	return f.err
}

// ResolveMergeTestFailure makes the prompt double merge.Coordinator's test-gate
// resolver too, on the same footing as ResolveMergeConflict.
func (f *fakePrompts) ResolveMergeTestFailure(_ context.Context, res merge.TestFailureResolution) error {
	f.testFailures = append(f.testFailures, res)
	return f.err
}

// RunMergeBeforeAction makes the prompt double merge.Coordinator's before-action
// runner too, on the same footing as the two resolvers.
func (f *fakePrompts) RunMergeBeforeAction(_ context.Context, act merge.BeforeAction) error {
	f.beforeActions = append(f.beforeActions, act)
	return f.err
}

// RunMergeAfterAction makes the prompt double merge.Coordinator's after-action
// runner too, on the same footing as the before-action's.
func (f *fakePrompts) RunMergeAfterAction(_ context.Context, act merge.AfterAction) error {
	f.afterActions = append(f.afterActions, act)
	return f.err
}

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
	h, p, _ := newGatedHandlerWithMerges(t, turnActive, tasks, &fakeMerges{})
	return h, p
}

// newGatedHandlerWithMerges is newGatedHandler for a test that also drives the
// merge runner behind the interrupt's queue half. The two share one constructor
// so a gate test and an eviction test cannot end up wiring different handlers.
func newGatedHandlerWithMerges(t *testing.T, turnActive bool, tasks LiveTaskSource, merges *fakeMerges) (*commandHandler, *fakePrompts, *fakeMerges) {
	t.Helper()
	p := &fakePrompts{turnActive: turnActive}
	h, err := newCommandHandler(p, merges, &fakeLifecycle{}, nil, &fakeSessionCmds{}, nil, nil, nil,
		CommandHandlerConfig{Interrupt: InterruptGateConfig{Turns: p, LiveTasks: tasks}})
	if err != nil {
		t.Fatalf("newCommandHandler: %v", err)
	}
	return h, p, merges
}

func (f *fakePrompts) SubmitPrompt(_ context.Context, ws, requestID, text, _ string, promptOrigin corev1.PromptOrigin) error {
	f.prompted = append(f.prompted, ws+":"+text)
	f.promptRequestIDs = append(f.promptRequestIDs, requestID)
	f.promptOrigins = append(f.promptOrigins, promptOrigin)
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
func (f *fakePrompts) SetModel(_ context.Context, _ string, model string) (string, error) {
	f.models = append(f.models, model)
	return "opus", f.err
}

// fakeMerges records WHOLE requests, not just workspace names: the geometry
// the command carried is the thing the handler must pass through intact, so a
// fake that kept only the name could not tell a working handler from the one
// that dropped all three dirs on the floor.
type fakeMerges struct {
	merged    []merge.Request
	resumed   []merge.Request
	abandoned []string
	// abandonHit makes Abandon report an abandoned merge; abandonErr makes it
	// fail. Zero values are the common "no merge in flight" answer.
	abandonHit bool
	abandonErr error
	// evicted records every workspace Evict was asked about, evictCount is what
	// it reports having dropped, and evictErr makes it fail. The zero values are
	// the common "nothing was queued" answer.
	evicted    []string
	evictCount int
	evictErr   error
	// onEvict runs INSIDE Evict, before it answers, exactly as onEnqueue does:
	// it is how a test reads what the handler had already done at the instant
	// the eviction was asked for, rather than assuming the ordering.
	onEvict func()
	// enqueueErr refuses the admission, which is the path that must still leave
	// a terminal merge phase behind.
	enqueueErr error
	// onEnqueue runs INSIDE Enqueue, before it answers. It is how a test reads
	// what was already recorded at the instant the coordinator was asked, which
	// is the only way to observe the mark's ordering rather than assume it.
	onEnqueue func()
}

func (f *fakeMerges) Enqueue(_ context.Context, req merge.Request) (merge.Position, error) {
	if f.onEnqueue != nil {
		f.onEnqueue()
	}
	if f.enqueueErr != nil {
		return merge.Position{}, f.enqueueErr
	}
	f.merged = append(f.merged, req)
	return merge.Position{Index: len(f.merged), Depth: len(f.merged), Repo: "/repo/.git"}, nil
}
func (f *fakeMerges) Resume(_ context.Context, req merge.Request) error {
	f.resumed = append(f.resumed, req)
	return nil
}
func (f *fakeMerges) Abandon(_ context.Context, workspace string) (bool, error) {
	f.abandoned = append(f.abandoned, workspace)
	if f.abandonErr != nil {
		return false, f.abandonErr
	}
	return f.abandonHit, nil
}

func (f *fakeMerges) Evict(_ context.Context, workspace string) (int, error) {
	if f.onEvict != nil {
		f.onEvict()
	}
	f.evicted = append(f.evicted, workspace)
	if f.evictErr != nil {
		return 0, f.evictErr
	}
	return f.evictCount, nil
}

// mergedWorkspaces returns just the workspace names of the recorded merges, for
// assertions that only care about routing.
func (f *fakeMerges) mergedWorkspaces() []string {
	out := make([]string, 0, len(f.merged))
	for _, req := range f.merged {
		out = append(out, req.Workspace)
	}
	return out
}

// resumedWorkspaces is mergedWorkspaces for the resume path.
func (f *fakeMerges) resumedWorkspaces() []string {
	out := make([]string, 0, len(f.resumed))
	for _, req := range f.resumed {
		out = append(out, req.Workspace)
	}
	return out
}

type fakeLifecycle struct {
	closed []string
	opened []string
	// openOpts records the run preferences each Open carried, positionally
	// alongside opened.
	openOpts        []WorkspaceOpenOpts
	openedDriveable []string
	openedForMerge  []string
	// closeErr, when set, refuses every close.
	closeErr error
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
	if f.closeErr != nil {
		return f.closeErr
	}
	f.closed = append(f.closed, ws)
	return nil
}
func (f *fakeLifecycle) Open(_ context.Context, ws string, opts WorkspaceOpenOpts) error {
	f.opened = append(f.opened, ws)
	f.openOpts = append(f.openOpts, opts)
	return nil
}

// OpenDriveable records separately from Open: these harnesses drive no merge,
// and folding the two together would hide which bring-up a future one used.
func (f *fakeLifecycle) OpenDriveable(_ context.Context, ws string) error {
	f.openedDriveable = append(f.openedDriveable, ws)
	return nil
}

func (f *fakeLifecycle) OpenForMerge(_ context.Context, ws string) error {
	f.openedForMerge = append(f.openedForMerge, ws)
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
	releases     chan SessionPublicationRelease
	decisions    map[string]SessionPublicationDecision
	materialized []string
	completions  []hostActionCompletion
}

type fakeHostWorkPublisher struct {
	available []*frontendv1.WorkspaceAvailable
	actions   []*frontendv1.HostAction
	// hostClients is how many host clients each push reports reaching. Zero is
	// the "no Emacs host is connected" case the forwarding loops must call out.
	hostClients int
}

func (f *fakeHostWorkPublisher) PushWorkspaceAvailable(available *frontendv1.WorkspaceAvailable) int {
	f.available = append(f.available, available)
	return f.hostClients
}

func (f *fakeHostWorkPublisher) PushHostAction(action *frontendv1.HostAction) int {
	f.actions = append(f.actions, action)
	return f.hostClients
}

func newFakeWorkspaceCreation() *fakeWorkspaceCreation {
	return &fakeWorkspaceCreation{
		available: make(chan *frontendv1.WorkspaceAvailable, 8),
		actions:   make(chan *frontendv1.HostAction, 8),
		releases:  make(chan SessionPublicationRelease, 8),
		decisions: map[string]SessionPublicationDecision{},
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

func (f *fakeWorkspaceCreation) SessionPublicationDecision(worktreePath, sessionID string) (SessionPublicationDecision, error) {
	if decision, ok := f.decisions[worktreePath+"\x00"+sessionID]; ok {
		return decision, nil
	}
	return SessionPublicationDecision{WorktreePath: worktreePath, SessionID: sessionID, Materialized: true}, nil
}

func (f *fakeWorkspaceCreation) SubscribeSessionPublicationReleases() (<-chan SessionPublicationRelease, func()) {
	return f.releases, func() {}
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

// mergeTransition is one recorded merge-state write, in order.
type mergeTransition struct {
	workspace string
	phase     merge.Phase
	cause     string
}

// fakeMergeStates is the handler's merge.StateSink: it records every phase the
// command handler itself emits, in order, which is what makes the
// merge_enqueuing-before-enqueue ordering observable.
type fakeMergeStates struct {
	mu  sync.Mutex
	got []mergeTransition
	// err fails EVERY record, for the "the mark itself could not be written"
	// path.
	err error
	// failPhase fails exactly one phase, for the "the terminal record failed"
	// path where the mark must still have landed.
	failPhase merge.Phase
}

func (f *fakeMergeStates) RecordMergeTransition(ws string, phase merge.Phase, cause string) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.err != nil {
		return f.err
	}
	if f.failPhase != "" && phase == f.failPhase {
		return fmt.Errorf("sink refused %s", phase)
	}
	f.got = append(f.got, mergeTransition{ws, phase, cause})
	return nil
}

// phases returns the recorded phases in order.
func (f *fakeMergeStates) phases() []merge.Phase {
	f.mu.Lock()
	defer f.mu.Unlock()
	out := make([]merge.Phase, len(f.got))
	for i, tr := range f.got {
		out[i] = tr.phase
	}
	return out
}

func newTestHandler(t *testing.T) (*commandHandler, *fakePrompts, *fakeMerges, *fakeLifecycle) {
	t.Helper()
	p, m, l := &fakePrompts{}, &fakeMerges{}, &fakeLifecycle{}
	h, err := newCommandHandler(p, m, l, nil, &fakeSessionCmds{}, nil, nil, nil,
		CommandHandlerConfig{MergeStates: &fakeMergeStates{}})
	if err != nil {
		t.Fatalf("newCommandHandler: %v", err)
	}
	return h, p, m, l
}

// fakeGeometry stands in for the daemon's durable workspace -> merge-geometry
// map (*geometry.Store).
type fakeGeometry struct {
	records map[string]geometry.Record
	err     error
	lookups []string
}

func (f *fakeGeometry) Lookup(_ context.Context, workspace string) (geometry.Record, bool, error) {
	f.lookups = append(f.lookups, workspace)
	if f.err != nil {
		return geometry.Record{}, false, f.err
	}
	rec, ok := f.records[workspace]
	return rec, ok, nil
}

// newMergeTestHandler wires a handler over a geometry record for "ws1", which
// is the post-cutover shape: Emacs sends a bare workspace name and the daemon
// answers the three coordinates from its own record.
func newMergeTestHandler(t *testing.T) (*commandHandler, *fakeMerges, *fakeGeometry, *fakeMergeStates) {
	t.Helper()
	g := &fakeGeometry{records: map[string]geometry.Record{
		"ws1": {Workspace: "ws1", SourceBranch: "DWC/recorded", SourceDir: "/worktrees/ws1", TargetDir: "/repo", Origin: geometry.OriginCreated},
	}}
	m := &fakeMerges{}
	states := &fakeMergeStates{}
	h, err := newCommandHandler(&fakePrompts{}, m, &fakeLifecycle{}, nil, &fakeSessionCmds{}, nil, nil, nil,
		CommandHandlerConfig{MergeGeometry: g, MergeStates: states})
	if err != nil {
		t.Fatalf("newCommandHandler: %v", err)
	}
	return h, m, g, states
}

// --- dispatch tests -------------------------------------------------------

func TestCommandHandlerSetModelRejectsSyntheticWithoutCallingPromptRouter(t *testing.T) {
	// Arrange: the CLI marker means "no override", never a selectable model.
	h, prompts, _, _ := newTestHandler(t)

	// Act
	selected, err := h.SetModel(context.Background(), "/workspace", "request-1", &frontendv1.SetModelCmd{Model: "<synthetic>"})

	// Assert: no daemon-to-shim request is made and no fictional selection is
	// returned to the correlated frontend receipt.
	if err == nil || !strings.Contains(err.Error(), "empty model") {
		t.Fatalf("SetModel(<synthetic>) error = %v, want empty-model refusal", err)
	}
	if selected != "" {
		t.Fatalf("SetModel(<synthetic>) selected = %q, want empty", selected)
	}
	if len(prompts.models) != 0 {
		t.Fatalf("SetModel(<synthetic>) called prompt router with %#v", prompts.models)
	}
}

func TestCommandHandlerSubmitPromptRoutesToPrompts(t *testing.T) {
	// Arrange
	h, p, _, _ := newTestHandler(t)
	// Act
	err := h.SubmitPrompt(context.Background(), "/ws1", "r1", &frontendv1.SubmitPromptCmd{Text: "hi", PromptOrigin: corev1.PromptOrigin_PROMPT_ORIGIN_USER_SENT})
	// Assert
	if err != nil {
		t.Fatalf("err: %v", err)
	}
	if len(p.prompted) != 1 || p.prompted[0] != "/ws1:hi" {
		t.Fatalf("prompted = %v", p.prompted)
	}
	if len(p.promptOrigins) != 1 || p.promptOrigins[0] != corev1.PromptOrigin_PROMPT_ORIGIN_USER_SENT {
		t.Fatalf("prompt origins = %v, want USER_SENT", p.promptOrigins)
	}
}

func TestCommandHandlerSubmitPromptRejectsInvalidOriginBeforeRouting(t *testing.T) {
	for _, origin := range []corev1.PromptOrigin{
		corev1.PromptOrigin_PROMPT_ORIGIN_UNSPECIFIED,
		corev1.PromptOrigin(999),
	} {
		t.Run(fmt.Sprint(origin), func(t *testing.T) {
			h, p, _, _ := newTestHandler(t)
			err := h.SubmitPrompt(context.Background(), "/ws1", "r1", &frontendv1.SubmitPromptCmd{Text: "hi", PromptOrigin: origin})
			if err == nil || !strings.Contains(err.Error(), "prompt_origin") {
				t.Fatalf("SubmitPrompt origin=%v error = %v, want prompt_origin refusal", origin, err)
			}
			if len(p.prompted) != 0 {
				t.Fatalf("SubmitPrompt origin=%v routed prompts = %v, want none", origin, p.prompted)
			}
		})
	}
}

// TestCommandHandlerSubmitPromptCarriesTheRequestID pins the correlation the
// prompt receipt is built on: the session controller cannot key a bubble on an id the
// handler kept to itself.
func TestCommandHandlerSubmitPromptCarriesTheRequestID(t *testing.T) {
	// Arrange
	h, p, _, _ := newTestHandler(t)
	// Act
	if err := h.SubmitPrompt(context.Background(), "/ws1", "r1", &frontendv1.SubmitPromptCmd{Text: "hi", PromptOrigin: corev1.PromptOrigin_PROMPT_ORIGIN_USER_SENT}); err != nil {
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
	h, m, _, _ := newMergeTestHandler(t)
	// Act — no conflict_resolved_continue -> Merge.
	_ = h.MergeWorkspace(context.Background(), "ws1", "r1", &frontendv1.MergeWorkspaceCmd{})
	// Assert
	if got := m.mergedWorkspaces(); len(got) != 1 || got[0] != "ws1" || len(m.resumed) != 0 {
		t.Fatalf("merged=%v resumed=%v", got, m.resumedWorkspaces())
	}
}

func TestCommandHandlerMergeResolvedContinueRoutesToResume(t *testing.T) {
	// Arrange
	h, m, _, _ := newMergeTestHandler(t)
	// Act — conflict_resolved_continue -> Resume.
	_ = h.MergeWorkspace(context.Background(), "ws1", "r1", &frontendv1.MergeWorkspaceCmd{ConflictResolvedContinue: true})
	// Assert
	if got := m.resumedWorkspaces(); len(got) != 1 || got[0] != "ws1" || len(m.merged) != 0 {
		t.Fatalf("merged=%v resumed=%v", m.mergedWorkspaces(), got)
	}
}

// --- merge geometry resolution ---------------------------------------------

func TestBareMergeResolvesTheRecordedGeometry(t *testing.T) {
	// Arrange — the post-cutover Emacs command: a workspace name and nothing
	// else.
	h, m, g, _ := newMergeTestHandler(t)

	// Act.
	err := h.MergeWorkspace(context.Background(), "ws1", "r1", &frontendv1.MergeWorkspaceCmd{WorkspaceName: "ws1"})

	// Assert.
	if err != nil {
		t.Fatalf("MergeWorkspace: %v", err)
	}
	want := merge.Request{Workspace: "ws1", Name: "ws1", SourceBranch: "DWC/recorded", SourceDir: "/worktrees/ws1", TargetDir: "/repo"}
	if len(m.merged) != 1 || m.merged[0] != want {
		t.Fatalf("merge request = %+v, want %+v", m.merged, want)
	}
	if len(g.lookups) != 1 || g.lookups[0] != "ws1" {
		t.Fatalf("geometry lookups = %v, want one for ws1", g.lookups)
	}
}

func TestBareResumeResolvesTheRecordedGeometry(t *testing.T) {
	// Arrange — a resume carries no geometry either.
	h, m, _, _ := newMergeTestHandler(t)

	// Act.
	err := h.MergeWorkspace(context.Background(), "ws1", "r1", &frontendv1.MergeWorkspaceCmd{WorkspaceName: "ws1", ConflictResolvedContinue: true})

	// Assert.
	if err != nil {
		t.Fatalf("MergeWorkspace: %v", err)
	}
	want := merge.Request{Workspace: "ws1", Name: "ws1", SourceBranch: "DWC/recorded", SourceDir: "/worktrees/ws1", TargetDir: "/repo"}
	if len(m.resumed) != 1 || m.resumed[0] != want {
		t.Fatalf("resume request = %+v, want %+v", m.resumed, want)
	}
}

func TestAMergeForAnUnrecordedWorkspaceIsRefusedWithAnExplanation(t *testing.T) {
	// Arrange.
	h, m, _, _ := newMergeTestHandler(t)

	// Act.
	err := h.MergeWorkspace(context.Background(), "ws-unknown", "r1", &frontendv1.MergeWorkspaceCmd{WorkspaceName: "ws-unknown"})

	// Assert — the ack explains, and nothing is enqueued against a guess.
	if err == nil || !strings.Contains(err.Error(), "no recorded merge geometry") {
		t.Fatalf("MergeWorkspace error = %v, want an unrecorded-geometry refusal", err)
	}
	if !strings.Contains(err.Error(), "ws-unknown") {
		t.Fatalf("the refusal does not name the workspace: %v", err)
	}
	if len(m.merged) != 0 || len(m.resumed) != 0 {
		t.Fatalf("a merge ran without geometry: merged=%v resumed=%v", m.merged, m.resumed)
	}
}

func TestAGeometryLookupFailureRefusesTheMerge(t *testing.T) {
	// Arrange — the state store is unreadable.
	h, m, g, _ := newMergeTestHandler(t)
	g.err = errors.New("state store is down")

	// Act.
	err := h.MergeWorkspace(context.Background(), "ws1", "r1", &frontendv1.MergeWorkspaceCmd{WorkspaceName: "ws1"})

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "state store is down") {
		t.Fatalf("MergeWorkspace error = %v, want the lookup failure surfaced", err)
	}
	if len(m.merged) != 0 {
		t.Fatalf("a merge ran despite the lookup failure: %v", m.merged)
	}
}

func TestABareMergeWithoutAWiredGeometrySourceIsRefused(t *testing.T) {
	// Arrange — an unwired capability is a loud nack, never a guessed target.
	h, _, m, _ := newTestHandler(t)

	// Act.
	err := h.MergeWorkspace(context.Background(), "ws1", "r1", &frontendv1.MergeWorkspaceCmd{WorkspaceName: "ws1"})

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "merge-geometry record is not wired") {
		t.Fatalf("MergeWorkspace error = %v, want an unwired-source refusal", err)
	}
	if len(m.merged) != 0 {
		t.Fatalf("a merge ran with no geometry source: %v", m.merged)
	}
}

func TestMergeStateIsKeyedOnTheEnvelopeWorkspaceNotTheName(t *testing.T) {
	// The defect this guards: keying merge state on the DISPLAY name filed a
	// merge's rows under a workspace the SSM knew nothing else about, so its
	// WorkspaceState carried no connectivity verdict and Emacs refused the
	// frame. The merge landed on disk and its workspace was never torn down.
	// Arrange — the workspace's geometry is the daemon's own record, keyed by
	// the cwd the envelope carries.
	h, m, g, _ := newMergeTestHandler(t)
	g.records["/Users/me/worktrees/feature-one"] = geometry.Record{
		Workspace:    "/Users/me/worktrees/feature-one",
		SourceBranch: "DWC/feature-one",
		SourceDir:    "/Users/me/worktrees/feature-one",
		TargetDir:    "/repo",
		Origin:       geometry.OriginCreated,
	}
	// Act
	_ = h.MergeWorkspace(context.Background(), "/Users/me/worktrees/feature-one", "r1", &frontendv1.MergeWorkspaceCmd{
		WorkspaceName: "feature-one",
	})
	// Assert — the state key is the cwd every other axis files under, and the
	// name is carried separately for the tag.
	if len(m.merged) != 1 {
		t.Fatalf("merges = %d, want one", len(m.merged))
	}
	if got := m.merged[0].Workspace; got != "/Users/me/worktrees/feature-one" {
		t.Errorf("state key = %q, want the envelope's cwd", got)
	}
	if got := m.merged[0].Name; got != "feature-one" {
		t.Errorf("display name = %q, want the bare workspace name", got)
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

func TestCommandHandlerCloseAbandonsTheWorkspacesMergeFirst(t *testing.T) {
	// Arrange
	h, _, m, l := newTestHandler(t)
	// Act
	if err := h.CloseWorkspace(context.Background(), "ws1", "r1", &frontendv1.CloseWorkspaceCmd{}); err != nil {
		t.Fatalf("CloseWorkspace: %v", err)
	}
	// Assert — the merge abandonment ran, and the close still went through.
	if len(m.abandoned) != 1 || m.abandoned[0] != "ws1" {
		t.Fatalf("abandoned = %v, want [ws1]", m.abandoned)
	}
	if len(l.closed) != 1 || l.closed[0] != "ws1" {
		t.Fatalf("closed = %v, want [ws1]", l.closed)
	}
}

func TestCommandHandlerCloseRefusesWhenAbandonFails(t *testing.T) {
	// Arrange — the abandonment cannot complete.
	p, l := &fakePrompts{}, &fakeLifecycle{}
	m := &fakeMerges{abandonErr: errors.New("coordinator is shutting down")}
	h, err := newCommandHandler(p, m, l, nil, &fakeSessionCmds{}, nil, nil, nil)
	if err != nil {
		t.Fatalf("newCommandHandler: %v", err)
	}
	// Act
	err = h.CloseWorkspace(context.Background(), "ws1", "r1", &frontendv1.CloseWorkspaceCmd{})
	// Assert — the close is refused rather than leaving the lease standing.
	if err == nil {
		t.Fatalf("CloseWorkspace() error = nil, want error")
	}
	if len(l.closed) != 0 {
		t.Fatalf("closed = %v, want none", l.closed)
	}
}

// fakeLogTargets records the workspaces whose log targets were released.
type fakeLogTargets struct {
	evicted []dlog.Workspace
	err     error
}

func (f *fakeLogTargets) EvictWorkspace(workspace dlog.Workspace) (int, error) {
	if f.err != nil {
		return 0, f.err
	}
	f.evicted = append(f.evicted, workspace)
	return 1, nil
}

// newCloseHandler builds a handler over a lifecycle and a log-target evictor,
// which is the pairing the close path binds together.
func newCloseHandler(t *testing.T, lifecycle *fakeLifecycle, targets *fakeLogTargets) *commandHandler {
	t.Helper()
	h, err := newCommandHandler(&fakePrompts{}, &fakeMerges{}, lifecycle, nil, &fakeSessionCmds{}, nil, nil, func(string, ...any) {},
		CommandHandlerConfig{LogTargets: targets})
	if err != nil {
		t.Fatalf("newCommandHandler: %v", err)
	}
	return h
}

// THE LOG TARGETS GO WITH THE WORKSPACE. A closed workspace's descriptors are
// released rather than held for the daemon's lifetime.
func TestCommandHandlerCloseReleasesTheWorkspacesLogTargets(t *testing.T) {
	// Arrange.
	workspace := t.TempDir()
	targets := &fakeLogTargets{}
	h := newCloseHandler(t, &fakeLifecycle{}, targets)
	resolved, err := dlog.WorkspaceFromDirectory(workspace)
	if err != nil {
		t.Fatal(err)
	}

	// Act.
	if err := h.CloseWorkspace(context.Background(), workspace, "r1", &frontendv1.CloseWorkspaceCmd{}); err != nil {
		t.Fatalf("CloseWorkspace: %v", err)
	}

	// Assert.
	if len(targets.evicted) != 1 || targets.evicted[0].Directory != resolved.Directory {
		t.Fatalf("evicted = %v, want the closed workspace %q", targets.evicted, resolved.Directory)
	}
}

// A REFUSED CLOSE KEEPS ITS TARGETS. The workspace is still live, and taking
// its descriptors away would break the writers still using them.
func TestCommandHandlerARefusedCloseRetainsTheLogTargets(t *testing.T) {
	// Arrange.
	workspace := t.TempDir()
	targets := &fakeLogTargets{}
	h := newCloseHandler(t, &fakeLifecycle{closeErr: errors.New("the workspace is still merging")}, targets)

	// Act.
	err := h.CloseWorkspace(context.Background(), workspace, "r1", &frontendv1.CloseWorkspaceCmd{})

	// Assert.
	if err == nil {
		t.Fatal("a refused close reported success")
	}
	if len(targets.evicted) != 0 {
		t.Fatalf("evicted = %v on a refused close; the workspace is still live", targets.evicted)
	}
}

// AN EVICTION FAILURE IS REPORTED AND DOES NOT UNDO THE CLOSE, which has
// already happened.
func TestCommandHandlerCloseSurvivesALogTargetEvictionFailure(t *testing.T) {
	// Arrange.
	workspace := t.TempDir()
	lifecycle := &fakeLifecycle{}
	h := newCloseHandler(t, lifecycle, &fakeLogTargets{err: errors.New("descriptor already closed")})

	// Act.
	err := h.CloseWorkspace(context.Background(), workspace, "r1", &frontendv1.CloseWorkspaceCmd{})

	// Assert.
	if err != nil {
		t.Fatalf("CloseWorkspace: %v — an eviction failure must not undo a close that already happened", err)
	}
	if len(lifecycle.closed) != 1 {
		t.Fatalf("closed = %v, want the one workspace", lifecycle.closed)
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
	_, err := h.CreateSession(context.Background(), "/w", "r1", &frontendv1.CreateSessionCmd{Cwd: "/w"})
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
	_, err := h.CreateSession(context.Background(), "/w", "r1", &frontendv1.CreateSessionCmd{
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
	_, err := h.CreateSession(context.Background(), "/w", "r1", &frontendv1.CreateSessionCmd{Cwd: "/w"})
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
		func(stopShims bool, _ sessioncontroller.StopCause) { fired <- stopShims }, nil, nil)
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
	if _, got := h.CreateSession(context.Background(), "/w", "r1", &frontendv1.CreateSessionCmd{Cwd: "/w"}); got == nil {
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
	publisher := &fakeHostWorkPublisher{hostClients: 1}
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

func TestMaterializationRequestReachingNoHostIsLoggedAsUndelivered(t *testing.T) {
	// Arrange — a host-only frame pushed while no Emacs host is connected is
	// dropped by the fan-out, and this line is the only trace it ever leaves.
	publisher := &fakeHostWorkPublisher{hostClients: 0}
	available := make(chan *frontendv1.WorkspaceAvailable, 1)
	available <- &frontendv1.WorkspaceAvailable{JobId: "job-1", FinalName: "fresh"}
	close(available)
	var lines []string
	logf := func(format string, _ ...any) { lines = append(lines, format) }

	// Act.
	forwardWorkspaceAvailable(logf, publisher, available)

	// Assert.
	if !containsFormat(lines, "MATERIALIZATION REQUEST UNDELIVERED") {
		t.Fatalf("logged = %q, want an undelivered materialization request", lines)
	}
	if containsFormat(lines, "materialization request delivered") {
		t.Fatalf("logged = %q, want no delivery claim for a request nobody received", lines)
	}
}

func TestMaterializationRequestReachingAHostIsLoggedAsDelivered(t *testing.T) {
	// Arrange.
	publisher := &fakeHostWorkPublisher{hostClients: 2}
	available := make(chan *frontendv1.WorkspaceAvailable, 1)
	available <- &frontendv1.WorkspaceAvailable{JobId: "job-1", FinalName: "fresh"}
	close(available)
	var lines []string
	logf := func(format string, _ ...any) { lines = append(lines, format) }

	// Act.
	forwardWorkspaceAvailable(logf, publisher, available)

	// Assert.
	if !containsFormat(lines, "materialization request delivered") {
		t.Fatalf("logged = %q, want a delivery line", lines)
	}
	if containsFormat(lines, "MATERIALIZATION REQUEST UNDELIVERED") {
		t.Fatalf("logged = %q, want no undelivered claim for a request two hosts received", lines)
	}
}

func containsFormat(lines []string, want string) bool {
	for _, line := range lines {
		if strings.Contains(line, want) {
			return true
		}
	}
	return false
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
			{Workspace: "/w", Fence: "s1", Init: &datav1.SystemInit{Model: "haiku"}},
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
			Workspace: "/w", Fence: "s1",
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
		Resumes:           &fakeResumes{},
		SSM:               openTestSSM(t, reg),
		Progress:          progress.New(progress.Options{Logf: func(string, ...any) {}}),
		Prompts:           &fakePrompts{},
		Turns:             &fakePrompts{},
		Lifecycle:         &fakeLifecycle{},
		SessionDeaths:     stubSessionDeaths{},
		Sessions:          fakeSessions{views: []*frontendv1.SessionView{{Workspace: "/w", SessionId: "s1", Model: "haiku"}}},
		SessionCommands:   &SessionCommandBinding{},
		WorkspaceCreation: newFakeWorkspaceCreation(),
		MergeLease:        stubMergeLease{},
		MergeQueue:        newTestMergeQueue(t),
		LogVerbosef:       t.Logf,
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
		Resumes:           &fakeResumes{},
		SSM:               openTestSSM(t, reg),
		Progress:          prog,
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
		Resumes:         &fakeResumes{},
		LogVerbosef:     t.Logf,
		SSM:             openTestSSM(t, reg),
		SessionCommands: &SessionCommandBinding{},
	})
	if err == nil {
		t.Fatal("want a construction error for a nil progress resolver")
	}
}

func TestWireAgentShimRejectsNilWorkspaceCreation(t *testing.T) {
	reg := openTestRegistry(t)
	_, err := WireAgentShim(AgentShimConfig{
		Resumes:         &fakeResumes{},
		LogVerbosef:     t.Logf,
		SSM:             openTestSSM(t, reg),
		Progress:        progress.New(progress.Options{Logf: func(string, ...any) {}}),
		Prompts:         &fakePrompts{},
		Lifecycle:       &fakeLifecycle{},
		SessionDeaths:   stubSessionDeaths{},
		SessionCommands: &SessionCommandBinding{},
	})
	if err == nil || !strings.Contains(err.Error(), "WorkspaceCreation") {
		t.Fatalf("WireAgentShim error = %v, want missing WorkspaceCreation bridge", err)
	}
}

func TestWireAgentShimRejectsAScheduleStoreWithNoQueueBackend(t *testing.T) {
	// A daemon that can PARK a prompt but has neither force nor cancel to
	// release it would strand the user with a chip and no verb. That is a
	// construction error, not something to discover under a live drain.
	// Arrange.
	reg := openTestRegistry(t)

	// Act.
	_, err := WireAgentShim(AgentShimConfig{
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
		ShutdownSchedules: &fakeScheduleStore{},
		DrainHolds:        &fakeHoldSource{},
		DrainEvidence:     newFakeEvidence(),
		RequestShutdown:   func(bool, sessioncontroller.StopCause) {},
		// Queues is deliberately nil.
	})

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "Queues") {
		t.Fatalf("WireAgentShim = %v, want a refusal naming the missing Queues backend", err)
	}
}

// --- merge runner ---------------------------------------------------------

// The MergeRunner IS merge.Coordinator now, so the runner resolves nothing and
// the failure it used to have — a workspace it could not map to dirs — is gone.
// What remains is a request that arrived INCOMPLETE, and it must still surface
// rather than queue a merge with a blank source or target.

func TestMergeRunnerSurfacesAnIncompleteRequest(t *testing.T) {
	// Arrange — a command that named no source branch.
	runner := newTestMergeCoordinator(t)
	// Act
	_, got := runner.Enqueue(context.Background(), merge.Request{Workspace: "/ws/ws1", Name: "ws1", SourceDir: "/s", TargetDir: "/t"})
	// Assert — merge.Request's own validation refuses it, never a silent no-op merge.
	if got == nil || !strings.Contains(got.Error(), "SourceBranch is required") {
		t.Fatalf("merge error = %v, want the incomplete request refused", got)
	}
}

func TestMergeResumeSurfacesAnIncompleteRequest(t *testing.T) {
	// Arrange — the resume path re-receives the geometry, so it can arrive
	// incomplete in exactly the same way.
	runner := newTestMergeCoordinator(t)
	// Act
	got := runner.Resume(context.Background(), merge.Request{Workspace: "/ws/ws1", Name: "ws1", SourceBranch: "b", SourceDir: "/s"})
	// Assert
	if got == nil || !strings.Contains(got.Error(), "TargetDir is required") {
		t.Fatalf("resume error = %v, want the incomplete request refused", got)
	}
}

// newTestMergeCoordinator builds the production merge.Coordinator over a
// scratch durable queue, so a handler-level test exercises the real admission
// path rather than a fake's idea of it.
func newTestMergeCoordinator(t *testing.T) *merge.QueueCoordinator {
	t.Helper()
	logf := func(string, ...any) {}
	suite, err := merge.NewRepoSuiteRunner(logf)
	if err != nil {
		t.Fatalf("suite runner: %v", err)
	}
	driver, err := merge.NewDriver(merge.Config{Logf: logf, Sink: noopSink{}, Suite: suite})
	if err != nil {
		t.Fatalf("driver: %v", err)
	}
	keyer, err := merge.NewGitRepoKeyer(logf)
	if err != nil {
		t.Fatalf("keyer: %v", err)
	}
	queue, err := merge.NewFileQueue(t.TempDir(), logf)
	if err != nil {
		t.Fatalf("queue: %v", err)
	}
	coord, err := merge.NewCoordinator(merge.CoordinatorConfig{
		Logf: logf, Sink: noopSink{}, Queue: queue, Phases: noopPhases{}, Keyer: keyer, Picker: driver,
		Lease: stubMergeLease{}, Resolver: stubMergeResolver{}, TestResolver: stubMergeTestResolver{},
		PostMerge: stubPostMergeHook{},
		Status:    noopSink{},
		Sessions:  stubSessionBringUp{},
		Deaths:    stubSessionDeaths{},
		// No workspace in these harnesses was created with a before-merge action,
		// which is the common case; the run goes straight to the plan.
		BeforeActions:      stubBeforeActions{},
		AfterActions:       stubAfterActions{},
		BeforeActionRunner: stubBeforeActionRunner{},
		AfterActionRunner:  stubAfterActionRunner{},
	})
	if err != nil {
		t.Fatalf("coordinator: %v", err)
	}
	t.Cleanup(func() { coord.Close() })
	return coord
}

type noopSink struct{}

func (noopSink) RecordMergeTransition(string, merge.Phase, string) error { return nil }

// RecordMergeStatus makes the double the StatusSink too, as the production sink
// is: one call carries the axis row and the phase status.
func (noopSink) RecordMergeStatus(string, merge.Phase, string, *frontendv1.MergeStatus) error {
	return nil
}

// stubSessionBringUp is the merge.SessionBringUp a unit harness binds: nothing
// here runs a shim, so every workspace is reported already live.
type stubSessionBringUp struct{}

func (stubSessionBringUp) EnsureLive(context.Context, string) error { return nil }

// stubSessionDeaths is the SessionDeaths a unit harness binds: no workspace of
// a wiring test has ever had a session deleted.
type stubSessionDeaths struct{}

func (stubSessionDeaths) DeletedSession(string) (string, bool, error) { return "", false, nil }

// stubBeforeActions is the merge.BeforeActionSource a unit harness binds: no
// workspace here was created with an action.
type stubBeforeActions struct{}

func (stubBeforeActions) BeforeAction(string) (string, error) { return "", nil }

// stubBeforeActionRunner is the merge.BeforeActionRunner a unit harness binds.
type stubBeforeActionRunner struct{}

func (stubBeforeActionRunner) Run(context.Context, merge.BeforeAction) error { return nil }

// stubAfterActionRunner is the merge.AfterActionRunner a unit harness binds.
type stubAfterActionRunner struct{}

func (stubAfterActionRunner) Run(context.Context, merge.AfterAction) error { return nil }

type stubAfterActions struct{}

func (stubAfterActions) AfterAction(merge.Request) (string, error) { return "", nil }

// noopPhases is the merge.PhaseSource a unit harness binds: no workspace is
// pinned on any phase, so the boot sweep has nothing to sweep.
type noopPhases struct{}

func (noopPhases) WorkspacesAtPhase(merge.Phase) ([]string, error) { return nil, nil }

// stubMergeLease is the merge.Lease a unit harness binds. The real one lives in
// internal/ssm and interrupts the workspace's turn; nothing in these tests runs
// a shim, so taking the lease here is a no-op that still returns a release func
// (a nil release is a hard panic in the coordinator, by design).
type stubMergeLease struct{}

func (stubMergeLease) Acquire(context.Context, string) (func(), error) { return func() {}, nil }
func (stubMergeLease) Held(string) bool                                { return false }

// stubMergeResolver is the merge.ConflictResolver a unit harness binds where it
// builds a coordinator directly. In WireAgentShim the resolver is DERIVED from
// the PromptRouter (see mergeConflictResolver), so those harnesses get theirs
// from fakePrompts instead.
type stubMergeResolver struct{}

func (stubMergeResolver) Resolve(context.Context, merge.ConflictResolution) error { return nil }

// stubMergeTestResolver is the merge.TestFailureResolver sibling of
// stubMergeResolver, bound by the same harnesses for the same reason.
type stubMergeTestResolver struct{}

func (stubMergeTestResolver) Resolve(context.Context, merge.TestFailureResolution) error { return nil }

// newTestMergeQueue roots the durable merge queue in a scratch directory, so a
// harness never publishes into the operator's real state root.
func newTestMergeQueue(t *testing.T) merge.DurableQueue {
	t.Helper()
	q, err := merge.NewFileQueue(filepath.Join(t.TempDir(), "merge-queue"), func(string, ...any) {})
	if err != nil {
		t.Fatalf("merge queue: %v", err)
	}
	return q
}

// --- wire assembly --------------------------------------------------------

func TestWireAgentShimRejectsNilSSM(t *testing.T) {
	// Arrange / Act / Assert — the SSM is now injected; a nil one is a
	// construction error rather than a nil-deref later.
	if _, err := WireAgentShim(AgentShimConfig{}); err == nil {
		t.Fatal("want error for nil SSM")
	}
}

func TestWireAgentShimMergeTransitionReachesSSM(t *testing.T) {
	// Arrange
	reg := openTestRegistry(t)
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
	if err != nil {
		t.Fatalf("WireAgentShim: %v", err)
	}
	defer shim.Close()
	// Act — the merge.Driver's sink is the SSM, so a transition it emits lands
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

func TestTargetClientLogWriterMismatchNamesReceivedAndAuthoritativeClaudeIDs(t *testing.T) {
	workspace := t.TempDir()
	writer, err := NewTargetClientLogWriter(dlog.NewTargetManager(), fakeClientLogIdentityResolver{
		identity: ClientLogSessionIdentity{AgentReplSessionID: "session-current", ClaudeSessionID: "claude-current"},
		known:    true,
	}, io.Discard, false)
	if err != nil {
		t.Fatal(err)
	}
	context, err := structpb.NewStruct(map[string]any{
		"timestamp": "2026-07-28T12:00:00Z", "runtime": "webapp", "level": "info", "verbosity": "normal",
		"operation": "webapp.x", "message": "source", "context": map[string]any{}, "connection_id": "connection-1",
		"agent_repl_session_id": "session-current", "claude_session_id": "claude-retired",
	})
	if err != nil {
		t.Fatal(err)
	}

	err = writer.PersistClientLog(workspace, "request-1", &frontendv1.ClientLogCmd{
		Level: frontendv1.ClientLogLevel_CLIENT_LOG_LEVEL_INFO, Message: "source", Context: context,
	})

	if err == nil || !strings.Contains(err.Error(), `got="claude-retired"`) || !strings.Contains(err.Error(), `want="claude-current"`) {
		t.Fatalf("mismatch error = %v, want got and authoritative Claude ids", err)
	}
}

func TestTargetClientLogWriterMismatchNamesReceivedAndAuthoritativeAgentReplIDs(t *testing.T) {
	workspace := t.TempDir()
	writer, err := NewTargetClientLogWriter(dlog.NewTargetManager(), fakeClientLogIdentityResolver{
		identity: ClientLogSessionIdentity{AgentReplSessionID: "session-current", ClaudeSessionID: "claude-current"},
		known:    true,
	}, io.Discard, false)
	if err != nil {
		t.Fatal(err)
	}
	context, err := structpb.NewStruct(map[string]any{
		"timestamp": "2026-07-28T12:00:00Z", "runtime": "webapp", "level": "info", "verbosity": "normal",
		"operation": "webapp.x", "message": "source", "context": map[string]any{}, "connection_id": "connection-1",
		"agent_repl_session_id": "session-retired", "claude_session_id": "claude-current",
	})
	if err != nil {
		t.Fatal(err)
	}

	err = writer.PersistClientLog(workspace, "request-1", &frontendv1.ClientLogCmd{
		Level: frontendv1.ClientLogLevel_CLIENT_LOG_LEVEL_INFO, Message: "source", Context: context,
	})

	if err == nil || !strings.Contains(err.Error(), `got="session-retired"`) || !strings.Contains(err.Error(), `want="session-current"`) {
		t.Fatalf("mismatch error = %v, want got and authoritative agent-repl ids", err)
	}
}

func TestTargetClientLogWriterMismatchWrapsTheStaleIdentitySentinel(t *testing.T) {
	// Arrange — a page that outlived its session forwards one record per log
	// call, and each rejection used to reach the classifier as an unmatched
	// error. The sentinel is what keeps it out of internal.unclassified.
	workspace := t.TempDir()
	writer, err := NewTargetClientLogWriter(dlog.NewTargetManager(), fakeClientLogIdentityResolver{
		identity: ClientLogSessionIdentity{AgentReplSessionID: "session-current", ClaudeSessionID: "claude-current"},
		known:    true,
	}, io.Discard, false)
	if err != nil {
		t.Fatal(err)
	}
	context, err := structpb.NewStruct(map[string]any{
		"timestamp": "2026-07-28T12:00:00Z", "runtime": "webapp", "level": "info", "verbosity": "normal",
		"operation": "webapp.x", "message": "source", "context": map[string]any{}, "connection_id": "connection-1",
		"agent_repl_session_id": "session-retired", "claude_session_id": "claude-current",
	})
	if err != nil {
		t.Fatal(err)
	}

	// Act.
	err = writer.PersistClientLog(workspace, "request-1", &frontendv1.ClientLogCmd{
		Level: frontendv1.ClientLogLevel_CLIENT_LOG_LEVEL_INFO, Message: "source", Context: context,
	})

	// Assert.
	if !errors.Is(err, errclass.ErrClientLogIdentityStale) {
		t.Fatalf("mismatch error = %v, want it to wrap the stale-identity sentinel", err)
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
	if err := h.SubmitPrompt(context.Background(), "/Users/x/.config/doom", "r1", &frontendv1.SubmitPromptCmd{Text: "hi", PromptOrigin: corev1.PromptOrigin_PROMPT_ORIGIN_USER_SENT}); err != nil {
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
