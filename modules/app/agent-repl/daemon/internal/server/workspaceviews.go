package server

// workspaceviews.go is the PUBLICATION SEAM for the three resolved per-workspace
// views: the topbar, the token-breakdown menu, and the revival gate.
//
// The resolution itself lives in internal/frontend, which is IO-free. This file
// is what gathers the facts those resolvers need from the authorities that own
// them — the SSM's WorkspaceState, the session registry, the model catalogs,
// the durable token ledger, the progress resolver's settled accounting cell —
// and decides WHEN a view goes out.
//
// TWO INVARIANTS ARE STRUCTURAL HERE.
//
// FULLY RESOLVED OR NOT AT ALL. Every resolver returns (view, error) and this
// file publishes only on the nil error. A resolver that refuses — no fence, an
// unknown connectivity, a hibernation with no account — leaves the client
// holding its previous view, which is a view that was once completely true,
// rather than a new one it would have to finish. The refusal is recorded
// loudly; it is never absorbed.
//
// THE FENCE IS NEVER MINTED HERE. Every fence on every view published by this
// file is read off the WorkspaceState the SSM pushed, which is where the
// daemon's one fence composer (ssm.Fence) put it. There is no second
// composition site, and nothing in this package may add one: a fence minted
// beside the authoritative one would be equal to it only by coincidence.
//
// PUBLISHED ON CHANGE, and the change is judged on the RENDERED VIEW rather
// than on the inputs behind it. Two resolutions that differ in no rendered
// field are the same push, and the second is dropped; a workspace whose branch,
// model menu, connectivity, accounting sentence or gate arm moved is a
// different view and goes out. Comparing the output is what makes "push on
// change of any rendered fact" true by construction instead of by keeping a
// list of the facts up to date.

import (
	"context"
	"sync"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/frontend"
	"claude-repld/internal/registry"

	"google.golang.org/protobuf/proto"
)

// WorkspaceViewPush is the frontend fan-out slice this publisher needs.
// Satisfied by *frontend.Server.
type WorkspaceViewPush interface {
	PushTopbarView(*frontendv1.TopbarView)
	PushTokenBreakdownView(*frontendv1.TokenBreakdownView)
	PushWorkspaceGateView(*frontendv1.WorkspaceGateView)
}

// SessionRecordSource reads one session's durable record. Satisfied by
// *registry.Registry.
type SessionRecordSource interface {
	Get(id string) (registry.Record, bool)
}

// ProgressCellSource reads a workspace's current resolved ProgressView, which
// is where the settled turn's accounting cell lives. The topbar shows that
// cell's SENTENCE and never its verdict, so this is the one place the two
// surfaces' shared prose comes from — see FooterAccountingCell's proto comment.
type ProgressCellSource interface {
	Current(workspace string) (*frontendv1.ProgressView, bool)
}

// WorkspaceBranchSource answers which branch a workspace's worktree is on. It
// is MergeGeometrySource under this reader's name: the geometry record is the
// daemon's existing durable answer to that question, and reading it here is
// what keeps the topbar from acquiring a second one. A nil source leaves the
// title unbranded, which is a complete title rather than a missing fact.
type WorkspaceBranchSource = MergeGeometrySource

// WorkspaceStateSource reads a workspace's current resolved WorkspaceState.
// Satisfied by *ssm.Manager. It is what lets a fact that moved on ANOTHER
// clock — the settled turn's accounting cell — be re-rendered against the
// authoritative state rather than against a remembered copy of it.
type WorkspaceStateSource interface {
	Current(workspace string) (*frontendv1.WorkspaceState, bool, error)
}

// WorkspaceViews resolves and publishes the three views, and retains the last
// published one per workspace so the connect snapshot carries them.
//
// RETENTION AND PUBLICATION ARE ONE STEP under this mutex, exactly as the
// roster's are in internal/frontend: a connect racing a publication must get
// the newer view once rather than the older one afterwards.
type WorkspaceViews struct {
	logf     dlog.Logf
	push     WorkspaceViewPush
	records  SessionRecordSource
	catalogs *SessionModelCatalogs
	progress ProgressCellSource
	branches WorkspaceBranchSource
	// states re-reads a workspace's authoritative state when a fact the topbar
	// renders moved without the state moving. A nil source disables the
	// accounting republication and says so once, at the seam.
	states WorkspaceStateSource

	// emitMu admits ONE drainer of pending at a time. It is taken with mu
	// released and is the only lock held while the frontend is called, which is
	// what keeps this publisher out of the frontend's lock order.
	emitMu sync.Mutex

	mu sync.Mutex
	// pending is the ordered outbox of fan-outs whose retention has already
	// been advanced under mu. Staging under mu is what fixes an emission's
	// POSITION relative to every other resolution's; the drain then emits in
	// that order with no subsystem lock held.
	//
	// THIS IS NOT A RELAXATION OF THE ORDERING the retain-and-push hold bought.
	// Pushing directly under mu was a cross-subsystem call under a subsystem
	// mutex: the frontend takes its own lock to broadcast, and its release path
	// called back into this publisher's Topbars while holding it. Staging keeps
	// "the wire is never older than the retention" — the order is fixed under
	// mu, and one drainer replays it — without putting mu inside the frontend's
	// lock order.
	pending []func()
	// retained is the last published view per workspace, per family. It is
	// both the change-detection baseline and the snapshot source, deliberately:
	// a separate snapshot store would be a second answer to "what has this
	// workspace been told", and the two would drift on exactly the frames a
	// publication was dropped as unchanged.
	retained map[string]*retainedWorkspaceViews
	// branchOf memoizes the branch lookup per workspace. A workspace's worktree
	// does not change branches under the daemon — the geometry record is
	// written when the workspace is created — and doing a store lookup on every
	// state transition would put a durable read on the SSM's push path.
	branchOf map[string]string
}

type retainedWorkspaceViews struct {
	topbar    *frontendv1.TopbarView
	breakdown *frontendv1.TokenBreakdownView
	gate      *frontendv1.WorkspaceGateView
}

// NewWorkspaceViews builds the publisher. push and logf are required — a
// publisher that cannot deliver and cannot report is not a degraded publisher,
// it is a silent one. Every other source is optional and its absence is a
// resolved fact rather than a gap: no catalogs is an empty model menu, no
// progress is an empty accounting line, no branches is an unbranded title.
func NewWorkspaceViews(logf dlog.Logf, push WorkspaceViewPush, records SessionRecordSource, catalogs *SessionModelCatalogs, progress ProgressCellSource, branches WorkspaceBranchSource, states WorkspaceStateSource) *WorkspaceViews {
	if logf == nil {
		panic("server: WorkspaceViews requires a log channel")
	}
	if push == nil {
		panic("server: WorkspaceViews requires a frontend push")
	}
	return &WorkspaceViews{
		logf:     logf,
		push:     push,
		records:  records,
		catalogs: catalogs,
		progress: progress,
		branches: branches,
		states:   states,
		retained: map[string]*retainedWorkspaceViews{},
		branchOf: map[string]string{},
	}
}

// PublishState resolves and publishes the TOPBAR and the GATE for the workspace
// the state describes.
//
// It is fed from the SSM's state subscription, which is the authority for every
// fact both views turn on: the fence, the connectivity verdict, and which
// session owns the workspace. A gate transition is a session-record fact rather
// than a state fact, so the gate is republished from PublishSession too — both
// paths resolve it through resolveGate and retain-and-push it through
// publishGateLocked, so the two cannot resolve the gate differently.
func (v *WorkspaceViews) PublishState(state *frontendv1.WorkspaceState) {
	workspace := state.GetWorkspace()
	if workspace == "" {
		v.logf("server: workspace views PUBLICATION DECLINED — a workspace state arrived with no workspace, so there is no view to key")
		return
	}
	rec, haveRecord := v.recordFor(state.GetSessionId())
	topbar, topbarErr := frontend.TopbarView(frontend.TopbarInputs{
		Workspace:       workspace,
		Fence:           state.GetFence(),
		Branch:          v.branch(workspace),
		SessionID:       state.GetSessionId(),
		ClaudeSessionID: rec.ClaudeSessionID,
		ModelDisplay:    rec.Model,
		ModelOptions:    v.modelOptions(state.GetSessionId()),
		Connectivity:    state.GetConnectivity(),
		AccountingLine:  v.accountingLine(workspace),
	})
	if topbarErr != nil {
		v.logf("server: topbar view WITHHELD ws=%q session=%q — the client keeps its last complete topbar rather than one it would have to finish: %v",
			workspace, state.GetSessionId(), topbarErr)
	}
	gate, gateErr := v.resolveGate(workspace, state.GetFence(), state.GetSessionId(), rec, haveRecord)

	// RETAINED AND STAGED UNDER ONE HOLD of the mutex. Staging after the
	// release would let two concurrent resolutions retain the newer view and
	// broadcast the older one — the client would then hold a view the snapshot
	// disagrees with until something else moved. The emission itself happens in
	// drain, with the mutex released; see pending.
	v.mu.Lock()
	if topbarErr == nil {
		retained := v.forLocked(workspace)
		if !proto.Equal(retained.topbar, topbar) {
			retained.topbar = topbar
			v.stageLocked(func() { v.push.PushTopbarView(topbar) })
		}
	}
	v.publishGateLocked(workspace, state.GetSessionId(), gate, gateErr)
	v.mu.Unlock()
	v.drain()
}

// stageLocked appends one fan-out to the ordered outbox. Caller holds v.mu,
// which is what fixes this emission's position against every other one's.
func (v *WorkspaceViews) stageLocked(emit func()) {
	v.pending = append(v.pending, emit)
}

// drain emits every staged fan-out, in staging order, with v.mu RELEASED.
//
// emitMu admits one drainer, so the order staging fixed is the order the
// frontend sees: a goroutine that staged an older view and has not reached
// here yet has still put it ahead in the queue, and whichever goroutine drains
// first emits it first. A drainer may therefore emit another goroutine's work,
// which is the point — nothing is left staged behind a caller that is slow to
// get here.
func (v *WorkspaceViews) drain() {
	v.emitMu.Lock()
	defer v.emitMu.Unlock()
	for {
		v.mu.Lock()
		if len(v.pending) == 0 {
			v.mu.Unlock()
			return
		}
		emit := v.pending[0]
		v.pending = v.pending[1:]
		v.mu.Unlock()
		emit()
	}
}

// RepublishAccounting re-renders the workspace's TOPBAR after its settled
// turn's accounting cell has resolved.
//
// WHY IT EXISTS. The accounting line is a TOPBAR fact, but it is not a
// WorkspaceState fact: the progress resolver composes it on its own clock,
// downstream of the very state transition that triggers PublishState. So the
// topbar published on a transition renders the accounting the resolver had
// BEFORE that transition, and a client connecting after the last transition was
// served a topbar whose accounting line would never settle — the retention the
// snapshot serves from was itself stale.
//
// It is fed from the progress resolver's own subscription, which is where the
// resolved cell arrives, so the republication happens exactly when the fact it
// is about has moved.
//
// THE STATE IS RE-READ, NEVER REMEMBERED. Everything else on the topbar — the
// fence above all — belongs to the state machine, and re-rendering them from a
// copy this file kept would be a second answer to what the workspace's current
// state is. Re-reading and then running the ONE publication path is what keeps
// this a re-render rather than a second resolver.
//
// IT IS CHEAP BY CONSTRUCTION. PublishState compares the RENDERED view, so a
// republication whose accounting line did not actually change is dropped
// without reaching a client.
func (v *WorkspaceViews) RepublishAccounting(workspace string) {
	if workspace == "" {
		v.logf("server: topbar accounting republication DECLINED — a progress view arrived with no workspace, so there is no topbar to key")
		return
	}
	if v.states == nil {
		v.logf("server: topbar accounting republication UNAVAILABLE ws=%q — no workspace-state source is wired, so the settled accounting line will not reach a client that connects after the last state transition", workspace)
		return
	}
	state, found, err := v.states.Current(workspace)
	if err != nil {
		v.logf("server: topbar accounting republication FAILED ws=%q — the workspace's current state could not be read, so the topbar keeps the accounting line it was last published with: %v", workspace, err)
		return
	}
	if !found {
		v.logf("server: topbar accounting republication SKIPPED ws=%q — the workspace has no resolved state yet, so there is no fence to stamp a topbar with", workspace)
		return
	}
	v.PublishState(state)
}

// PublishSession republishes the views whose facts are read off the SESSION
// RECORD rather than off the workspace state — today that is the gate alone.
//
// It is fed from the SessionView push, which is the funnel EVERY session-record
// mutation already runs through: the registry's hibernation write re-pushes the
// session view as its last step. Without this trigger a hibernation flip that
// records no SSM transition would leave the gate stale indefinitely, because
// PublishState is the only other trigger and nothing would fire it.
//
// fence comes from the workspace's current state and is carried, never
// composed — same as PublishTokenBreakdown's.
func (v *WorkspaceViews) PublishSession(workspace, fence, sessionID string) {
	if workspace == "" {
		v.logf("server: workspace gate PUBLICATION DECLINED — a session publication arrived with no workspace, so there is no view to key")
		return
	}
	rec, haveRecord := v.recordFor(sessionID)
	gate, gateErr := v.resolveGate(workspace, fence, sessionID, rec, haveRecord)
	v.mu.Lock()
	v.publishGateLocked(workspace, sessionID, gate, gateErr)
	v.mu.Unlock()
	v.drain()
}

// resolveGate is the ONE gate resolution both triggers run through, so a
// state-driven publication and a record-driven one cannot disagree about the
// arm. A refusal is recorded here and returned; it is never absorbed.
func (v *WorkspaceViews) resolveGate(workspace, fence, sessionID string, rec registry.Record, haveRecord bool) (*frontendv1.WorkspaceGateView, error) {
	gate, err := frontend.WorkspaceGateView(workspace, fence, haveRecord && rec.Hibernated,
		hibernationDetail(v.logf, rec.SessionID, rec.Hibernation))
	if err != nil {
		v.logf("server: workspace gate view WITHHELD ws=%q session=%q — the composer keeps its last complete gate: %v",
			workspace, sessionID, err)
	}
	return gate, err
}

// publishGateLocked retains a resolved gate and STAGES its push in ONE step.
// The caller holds v.mu, which is what keeps the retention and the broadcast
// from being reordered against another resolution's.
func (v *WorkspaceViews) publishGateLocked(workspace, sessionID string, gate *frontendv1.WorkspaceGateView, resolveErr error) {
	if resolveErr != nil {
		return
	}
	retained := v.forLocked(workspace)
	if proto.Equal(retained.gate, gate) {
		return
	}
	retained.gate = gate
	v.logf("server: workspace gate TRANSITION ws=%q session=%q gate=%s", workspace, sessionID, gateArmName(gate))
	v.stageLocked(func() { v.push.PushWorkspaceGateView(gate) })
}

// PublishTokenBreakdown resolves and publishes the breakdown menu from the
// session's durable completed-response aggregate.
//
// It is fed from the SessionView push, which is where that aggregate is already
// read — the same aggregate SessionViewFromRecordWithModelsAndUsage has always
// been handed and had nowhere to put. fence comes from the workspace's current
// state and is carried, never composed.
func (v *WorkspaceViews) PublishTokenBreakdown(workspace, fence string, usage *frontendv1.SessionTokenUtilization) {
	if workspace == "" {
		v.logf("server: token breakdown PUBLICATION DECLINED — no workspace to key the view on")
		return
	}
	view, err := frontend.TokenBreakdownView(workspace, fence, usage)
	if err != nil {
		v.logf("server: token breakdown view WITHHELD ws=%q — the menu keeps its last complete resolution: %v", workspace, err)
		return
	}
	// One hold for the retention AND the STAGING, for the reason PublishState
	// gives: an emission ordered outside the mutex can be overtaken by a newer
	// resolution's, leaving the wire older than the retention.
	v.mu.Lock()
	retained := v.forLocked(workspace)
	if proto.Equal(retained.breakdown, view) {
		v.mu.Unlock()
		return
	}
	retained.breakdown = view
	v.stageLocked(func() { v.push.PushTokenBreakdownView(view) })
	v.mu.Unlock()
	v.drain()
}

// Forget drops a closed workspace's retained views AND its memoized branch, so
// the connect snapshot stops carrying a topbar for a workspace nothing runs.
//
// IT IS CALLED FROM THE CLOSE, beside the log-target eviction, which is what
// binds these retentions' lifetime to the workspace that owns them. The branch
// memo has to go with them: a workspace re-created at the same path is a
// different worktree on a possibly different branch, and inheriting its dead
// predecessor's memo would render the OLD branch under a genuinely current
// fence — a lie no client gate can reject.
func (v *WorkspaceViews) Forget(workspace string) {
	v.mu.Lock()
	defer v.mu.Unlock()
	delete(v.retained, workspace)
	delete(v.branchOf, workspace)
}

// Topbars, TokenBreakdowns and WorkspaceGates are the connect snapshot's three
// resolved-view fields, served from the same retention the pushes advance.
func (v *WorkspaceViews) Topbars() []*frontendv1.TopbarView {
	v.mu.Lock()
	defer v.mu.Unlock()
	out := make([]*frontendv1.TopbarView, 0, len(v.retained))
	for _, r := range v.retained {
		if r.topbar != nil {
			out = append(out, r.topbar)
		}
	}
	return out
}

func (v *WorkspaceViews) TokenBreakdowns() []*frontendv1.TokenBreakdownView {
	v.mu.Lock()
	defer v.mu.Unlock()
	out := make([]*frontendv1.TokenBreakdownView, 0, len(v.retained))
	for _, r := range v.retained {
		if r.breakdown != nil {
			out = append(out, r.breakdown)
		}
	}
	return out
}

func (v *WorkspaceViews) WorkspaceGates() []*frontendv1.WorkspaceGateView {
	v.mu.Lock()
	defer v.mu.Unlock()
	out := make([]*frontendv1.WorkspaceGateView, 0, len(v.retained))
	for _, r := range v.retained {
		if r.gate != nil {
			out = append(out, r.gate)
		}
	}
	return out
}

func (v *WorkspaceViews) forLocked(workspace string) *retainedWorkspaceViews {
	if r, ok := v.retained[workspace]; ok {
		return r
	}
	r := &retainedWorkspaceViews{}
	v.retained[workspace] = r
	return r
}

// recordFor reads the owning session's durable record. A workspace between
// sessions has none, and that absence is reported to the caller rather than
// papered over with a zero record: the gate must not read "awake" from a record
// that does not exist.
func (v *WorkspaceViews) recordFor(sessionID string) (registry.Record, bool) {
	if v.records == nil || sessionID == "" {
		return registry.Record{}, false
	}
	return v.records.Get(sessionID)
}

func (v *WorkspaceViews) modelOptions(sessionID string) []*frontendv1.ModelOption {
	if v.catalogs == nil || sessionID == "" {
		return nil
	}
	return v.catalogs.Get(sessionID)
}

// accountingLine is the settled turn's composed summary, taken from the
// progress resolver's own cell. The topbar renders the SENTENCE and never the
// verdict, which is why it takes a string where the footer takes arms — and why
// there is one composition rather than two.
func (v *WorkspaceViews) accountingLine(workspace string) string {
	if v.progress == nil {
		return ""
	}
	view, ok := v.progress.Current(workspace)
	if !ok {
		return ""
	}
	return view.GetAccounting().GetSummary()
}

// branch resolves the workspace's branch once and remembers it. A lookup that
// FAILS is recorded and remembered as "no branch", not retried on every state
// transition: the title without a branch is complete and correct, and a durable
// read on the SSM's push path is a cost the topbar cannot justify.
func (v *WorkspaceViews) branch(workspace string) string {
	v.mu.Lock()
	branch, known := v.branchOf[workspace]
	v.mu.Unlock()
	if known {
		return branch
	}
	branch = ""
	if v.branches != nil {
		rec, found, err := v.branches.Lookup(context.Background(), workspace)
		switch {
		case err != nil:
			v.logf("server: topbar branch lookup FAILED ws=%q — the title renders the workspace name alone: %v", workspace, err)
		case found:
			branch = rec.SourceBranch
		}
	}
	v.mu.Lock()
	v.branchOf[workspace] = branch
	v.mu.Unlock()
	return branch
}

// gateArmName names a resolved gate's arm for the transition record. The
// resolver cannot produce an armless gate, so the fallback names the defect it
// would be rather than printing a blank.
func gateArmName(gate *frontendv1.WorkspaceGateView) string {
	switch {
	case gate.GetOpen() != nil:
		return "open"
	case gate.GetHibernated() != nil:
		return "hibernated"
	default:
		return "UNSET"
	}
}
