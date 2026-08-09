package server

import (
	"context"
	"errors"
	"fmt"
	"sync"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/registry"
	"claude-repld/internal/workspace/geometry"
)

// recordingViewPush captures every published view, so a case asserts what
// actually reached the wire rather than what the resolver produced.
type recordingViewPush struct {
	mu         sync.Mutex
	topbars    []*frontendv1.TopbarView
	breakdowns []*frontendv1.TokenBreakdownView
	gates      []*frontendv1.WorkspaceGateView
}

func (p *recordingViewPush) PushTopbarView(v *frontendv1.TopbarView) {
	p.mu.Lock()
	defer p.mu.Unlock()
	p.topbars = append(p.topbars, v)
}

func (p *recordingViewPush) PushTokenBreakdownView(v *frontendv1.TokenBreakdownView) {
	p.mu.Lock()
	defer p.mu.Unlock()
	p.breakdowns = append(p.breakdowns, v)
}

func (p *recordingViewPush) PushWorkspaceGateView(v *frontendv1.WorkspaceGateView) {
	p.mu.Lock()
	defer p.mu.Unlock()
	p.gates = append(p.gates, v)
}

// fixedRecords answers every lookup with one record.
type fixedRecords struct {
	rec   registry.Record
	found bool
}

func (f fixedRecords) Get(string) (registry.Record, bool) { return f.rec, f.found }

// fixedProgress answers Current with one view.
type fixedProgress struct {
	view *frontendv1.ProgressView
	ok   bool
}

func (f fixedProgress) Current(string) (*frontendv1.ProgressView, bool) { return f.view, f.ok }

// fixedGeometry answers the branch lookup.
type fixedGeometry struct {
	rec   geometry.Record
	found bool
	err   error
}

func (f fixedGeometry) Lookup(context.Context, string) (geometry.Record, bool, error) {
	return f.rec, f.found, f.err
}

// countingGeometry records how many lookups the publisher performed.
type countingGeometry struct {
	mu    sync.Mutex
	calls int
}

func (c *countingGeometry) Lookup(context.Context, string) (geometry.Record, bool, error) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.calls++
	return geometry.Record{SourceBranch: "b"}, true, nil
}

func (c *countingGeometry) count() int {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.calls
}

func newViewPublisher(t *testing.T, records SessionRecordSource, progress ProgressCellSource, branches WorkspaceBranchSource) (*WorkspaceViews, *recordingViewPush) {
	t.Helper()
	push := &recordingViewPush{}
	return NewWorkspaceViews(func(string, ...any) {}, push, records, nil, progress, branches, nil), push
}

// fixedStates is a WorkspaceStateSource answering with one state, so a test can
// drive the accounting republication without an SSM.
type fixedStates struct {
	state *frontendv1.WorkspaceState
	found bool
	err   error
}

func (f fixedStates) Current(string) (*frontendv1.WorkspaceState, bool, error) {
	return f.state, f.found, f.err
}

// newRepublishingPublisher is newViewPublisher with a state source wired, plus
// the log capture the republication's refusals are asserted through.
func newRepublishingPublisher(t *testing.T, progress ProgressCellSource, states WorkspaceStateSource) (*WorkspaceViews, *recordingViewPush, *[]string) {
	t.Helper()
	push := &recordingViewPush{}
	var lines []string
	logf := func(format string, args ...any) { lines = append(lines, fmt.Sprintf(format, args...)) }
	return NewWorkspaceViews(logf, push, nil, nil, progress, nil, states), push, &lines
}

func liveState() *frontendv1.WorkspaceState {
	return &frontendv1.WorkspaceState{
		Workspace:    "/home/u/ws",
		SessionId:    "s1",
		Fence:        "s1|g1",
		Connectivity: frontendv1.SessionConnectivity_SESSION_CONNECTIVITY_OPERATIONAL,
	}
}

func TestAStateWithNoWorkspacePublishesNothing(t *testing.T) {
	// Arrange — there is no key to route the view on.
	v, push := newViewPublisher(t, nil, nil, nil)
	// Act.
	v.PublishState(&frontendv1.WorkspaceState{Fence: "s|g"})
	// Assert.
	if len(push.topbars) != 0 || len(push.gates) != 0 {
		t.Fatalf("published %d topbars and %d gates for a keyless state", len(push.topbars), len(push.gates))
	}
}

func TestAnUnfencedStateWithholdsTheTopbar(t *testing.T) {
	// Arrange — an unfenced push cannot be told from a stale one.
	v, push := newViewPublisher(t, nil, nil, nil)
	state := liveState()
	state.Fence = ""
	// Act.
	v.PublishState(state)
	// Assert.
	if len(push.topbars) != 0 {
		t.Fatalf("published an unfenced topbar: %v", push.topbars)
	}
}

func TestAResolvedStatePublishesTheTopbar(t *testing.T) {
	// Arrange.
	v, push := newViewPublisher(t, nil, nil, nil)
	// Act.
	v.PublishState(liveState())
	// Assert.
	if len(push.topbars) != 1 {
		t.Fatalf("published %d topbars, want 1", len(push.topbars))
	}
}

func TestAnUnchangedStatePublishesTheTopbarOnlyOnce(t *testing.T) {
	// Arrange — change is judged on the rendered view, not on the trigger.
	v, push := newViewPublisher(t, nil, nil, nil)
	v.PublishState(liveState())
	// Act.
	v.PublishState(liveState())
	// Assert.
	if len(push.topbars) != 1 {
		t.Fatalf("published %d topbars for two identical states, want 1", len(push.topbars))
	}
}

func TestAChangedConnectivityRepublishesTheTopbar(t *testing.T) {
	// Arrange.
	v, push := newViewPublisher(t, nil, nil, nil)
	v.PublishState(liveState())
	degraded := liveState()
	degraded.Connectivity = frontendv1.SessionConnectivity_SESSION_CONNECTIVITY_DEGRADED
	// Act.
	v.PublishState(degraded)
	// Assert.
	if len(push.topbars) != 2 {
		t.Fatalf("published %d topbars, want the changed connectivity republished", len(push.topbars))
	}
}

func TestARotatedFenceRepublishesTheTopbar(t *testing.T) {
	// Arrange — the fence is a rendered field, so its rotation is a change.
	v, push := newViewPublisher(t, nil, nil, nil)
	v.PublishState(liveState())
	rebound := liveState()
	rebound.Fence = "s2|g2"
	// Act.
	v.PublishState(rebound)
	// Assert.
	if len(push.topbars) != 2 {
		t.Fatalf("published %d topbars, want the rotated fence republished", len(push.topbars))
	}
}

func TestTheTopbarCarriesTheStatesFenceRatherThanOneOfItsOwn(t *testing.T) {
	// Arrange — the daemon has exactly one fence composer, and it is not here.
	v, push := newViewPublisher(t, nil, nil, nil)
	// Act.
	v.PublishState(liveState())
	// Assert.
	if push.topbars[0].GetFence() != "s1|g1" {
		t.Fatalf("topbar fence = %q, want the state's own fence", push.topbars[0].GetFence())
	}
}

func TestTheTopbarShowsTheSettledTurnsAccountingSentence(t *testing.T) {
	// Arrange — the topbar shows the sentence and never the verdict.
	progress := fixedProgress{ok: true, view: &frontendv1.ProgressView{
		Accounting: &frontendv1.FooterAccountingCell{Summary: "5h 1.0%→2.0% (1.0pp)"},
	}}
	v, push := newViewPublisher(t, nil, progress, nil)
	// Act.
	v.PublishState(liveState())
	// Assert.
	if push.topbars[0].GetAccountingLine() != "5h 1.0%→2.0% (1.0pp)" {
		t.Fatalf("accounting line = %q", push.topbars[0].GetAccountingLine())
	}
}

func TestTheTopbarTitleCarriesTheWorkspacesBranch(t *testing.T) {
	// Arrange.
	v, push := newViewPublisher(t, nil, nil, fixedGeometry{rec: geometry.Record{SourceBranch: "feature-x"}, found: true})
	// Act.
	v.PublishState(liveState())
	// Assert.
	if push.topbars[0].GetTitle() != "ws (feature-x)" {
		t.Fatalf("title = %q", push.topbars[0].GetTitle())
	}
}

func TestAFailedBranchLookupStillPublishesATitle(t *testing.T) {
	// Arrange — a title without a branch is complete, not degraded.
	v, push := newViewPublisher(t, nil, nil, fixedGeometry{err: errors.New("store closed")})
	// Act.
	v.PublishState(liveState())
	// Assert.
	if push.topbars[0].GetTitle() != "ws" {
		t.Fatalf("title = %q, want the workspace name alone", push.topbars[0].GetTitle())
	}
}

func TestTheBranchIsLookedUpOnceRatherThanPerStateTransition(t *testing.T) {
	// Arrange — a durable read on the SSM's push path is a cost the topbar
	// cannot justify.
	branches := &countingGeometry{}
	v, _ := newViewPublisher(t, nil, nil, branches)
	// Act.
	v.PublishState(liveState())
	v.PublishState(liveState())
	// Assert.
	if branches.count() != 1 {
		t.Fatalf("branch lookups = %d, want 1", branches.count())
	}
}

func TestAWorkspaceWithNoSessionRecordPublishesAnOpenGate(t *testing.T) {
	// Arrange — a workspace between sessions is not asleep.
	v, push := newViewPublisher(t, fixedRecords{}, nil, nil)
	// Act.
	v.PublishState(liveState())
	// Assert.
	if push.gates[0].GetOpen() == nil {
		t.Fatalf("gate = %v, want open", push.gates[0].GetGate())
	}
}

func TestAHibernatedRecordPublishesAClosedGateWithItsAccount(t *testing.T) {
	// Arrange.
	v, push := newViewPublisher(t, fixedRecords{found: true, rec: registry.Record{
		SessionID:  "s1",
		Hibernated: true,
		Hibernation: registry.HibernationDetail{
			Cause: registry.HibernationCauseForced, SinceMs: 99,
		},
	}}, nil, nil)
	// Act.
	v.PublishState(liveState())
	// Assert.
	if push.gates[0].GetHibernated().GetDetail().GetForced() == nil {
		t.Fatalf("gate = %v, want hibernated with its cause", push.gates[0].GetGate())
	}
}

func TestAGateThatDoesNotTransitionIsPublishedOnlyOnce(t *testing.T) {
	// Arrange.
	v, push := newViewPublisher(t, fixedRecords{}, nil, nil)
	v.PublishState(liveState())
	// Act.
	v.PublishState(liveState())
	// Assert.
	if len(push.gates) != 1 {
		t.Fatalf("published %d gates for an unchanged gate, want 1", len(push.gates))
	}
}

func TestEveryGateTransitionIsPublished(t *testing.T) {
	// Arrange — awake, then asleep.
	records := &mutableRecords{}
	v, push := newViewPublisher(t, records, nil, nil)
	v.PublishState(liveState())
	records.set(registry.Record{
		SessionID: "s1", Hibernated: true,
		Hibernation: registry.HibernationDetail{Cause: registry.HibernationCauseForced, SinceMs: 1},
	}, true)
	// Act.
	v.PublishState(liveState())
	// Assert.
	if len(push.gates) != 2 {
		t.Fatalf("published %d gates across a transition, want 2", len(push.gates))
	}
}

// mutableRecords is a record source a case can move between publications.
type mutableRecords struct {
	mu    sync.Mutex
	rec   registry.Record
	found bool
}

func (m *mutableRecords) set(rec registry.Record, found bool) {
	m.mu.Lock()
	defer m.mu.Unlock()
	m.rec, m.found = rec, found
}

func (m *mutableRecords) Get(string) (registry.Record, bool) {
	m.mu.Lock()
	defer m.mu.Unlock()
	return m.rec, m.found
}

func TestAnUnfencedTokenBreakdownIsWithheld(t *testing.T) {
	// Arrange.
	v, push := newViewPublisher(t, nil, nil, nil)
	// Act.
	v.PublishTokenBreakdown("/home/u/ws", "", nil)
	// Assert.
	if len(push.breakdowns) != 0 {
		t.Fatalf("published an unfenced breakdown: %v", push.breakdowns)
	}
}

func TestAResolvedTokenBreakdownIsPublished(t *testing.T) {
	// Arrange.
	v, push := newViewPublisher(t, nil, nil, nil)
	// Act.
	v.PublishTokenBreakdown("/home/u/ws", "s1|g1", nil)
	// Assert.
	if len(push.breakdowns) != 1 {
		t.Fatalf("published %d breakdowns, want 1", len(push.breakdowns))
	}
}

func TestAnUnchangedTokenBreakdownIsPublishedOnlyOnce(t *testing.T) {
	// Arrange.
	v, push := newViewPublisher(t, nil, nil, nil)
	v.PublishTokenBreakdown("/home/u/ws", "s1|g1", nil)
	// Act.
	v.PublishTokenBreakdown("/home/u/ws", "s1|g1", nil)
	// Assert.
	if len(push.breakdowns) != 1 {
		t.Fatalf("published %d identical breakdowns, want 1", len(push.breakdowns))
	}
}

func TestTheSnapshotServesTheLastPublishedTopbar(t *testing.T) {
	// Arrange — the retention the pushes advance IS the snapshot source, so a
	// client connecting between two pushes adopts what the last push delivered.
	v, _ := newViewPublisher(t, nil, nil, nil)
	v.PublishState(liveState())
	// Act.
	got := v.Topbars()
	// Assert.
	if len(got) != 1 || got[0].GetFence() != "s1|g1" {
		t.Fatalf("snapshot topbars = %v", got)
	}
}

func TestTheSnapshotServesTheLastPublishedGate(t *testing.T) {
	// Arrange.
	v, _ := newViewPublisher(t, fixedRecords{}, nil, nil)
	v.PublishState(liveState())
	// Act.
	got := v.WorkspaceGates()
	// Assert.
	if len(got) != 1 || got[0].GetOpen() == nil {
		t.Fatalf("snapshot gates = %v", got)
	}
}

func TestTheSnapshotServesTheLastPublishedTokenBreakdown(t *testing.T) {
	// Arrange.
	v, _ := newViewPublisher(t, nil, nil, nil)
	v.PublishTokenBreakdown("/home/u/ws", "s1|g1", nil)
	// Act.
	got := v.TokenBreakdowns()
	// Assert.
	if len(got) != 1 || got[0].GetWorkspace() != "/home/u/ws" {
		t.Fatalf("snapshot breakdowns = %v", got)
	}
}

// movingGeometry answers each lookup with the branch it currently holds, so a
// case can re-create a workspace on a different branch between publications.
type movingGeometry struct {
	mu     sync.Mutex
	branch string
}

func (m *movingGeometry) set(branch string) {
	m.mu.Lock()
	defer m.mu.Unlock()
	m.branch = branch
}

func (m *movingGeometry) Lookup(context.Context, string) (geometry.Record, bool, error) {
	m.mu.Lock()
	defer m.mu.Unlock()
	return geometry.Record{SourceBranch: m.branch}, true, nil
}

func TestARecreatedWorkspaceResolvesAFreshBranch(t *testing.T) {
	// Arrange — the workspace is re-created at the same path on another branch,
	// and the memo of its dead predecessor's branch died with the close.
	branches := &movingGeometry{branch: "old-branch"}
	v, push := newViewPublisher(t, nil, nil, branches)
	v.PublishState(liveState())
	v.Forget("/home/u/ws")
	branches.set("new-branch")
	// Act.
	v.PublishState(liveState())
	// Assert.
	last := push.topbars[len(push.topbars)-1]
	if last.GetTitle() != "ws (new-branch)" {
		t.Fatalf("title = %q, want the re-created workspace's own branch", last.GetTitle())
	}
}

func TestARecordOnlyHibernationFlipRepublishesTheGate(t *testing.T) {
	// Arrange — the record went to sleep with no SSM state transition behind
	// it, so PublishState will never fire again on its own.
	records := &mutableRecords{}
	v, push := newViewPublisher(t, records, nil, nil)
	v.PublishState(liveState())
	records.set(registry.Record{
		SessionID: "s1", Hibernated: true,
		Hibernation: registry.HibernationDetail{Cause: registry.HibernationCauseForced, SinceMs: 1},
	}, true)
	// Act.
	v.PublishSession("/home/u/ws", "s1|g1", "s1")
	// Assert.
	if len(push.gates) != 2 || push.gates[1].GetHibernated() == nil {
		t.Fatalf("gates = %v, want the record-only flip republished as hibernated", push.gates)
	}
}

func TestASessionPublicationWithNoWorkspacePublishesNothing(t *testing.T) {
	// Arrange — there is no key to route the gate on.
	v, push := newViewPublisher(t, fixedRecords{}, nil, nil)
	// Act.
	v.PublishSession("", "s1|g1", "s1")
	// Assert.
	if len(push.gates) != 0 {
		t.Fatalf("published %d gates for a keyless session publication", len(push.gates))
	}
}

func TestAnUnchangedGateFromASessionPublicationIsPublishedOnlyOnce(t *testing.T) {
	// Arrange — change is judged on the rendered gate, not on the trigger.
	v, push := newViewPublisher(t, fixedRecords{}, nil, nil)
	v.PublishState(liveState())
	// Act.
	v.PublishSession("/home/u/ws", "s1|g1", "s1")
	// Assert.
	if len(push.gates) != 1 {
		t.Fatalf("published %d gates for an unchanged gate, want 1", len(push.gates))
	}
}

func TestAnUnfencedSessionPublicationWithholdsTheGate(t *testing.T) {
	// Arrange — an unfenced gate cannot be told from a stale one.
	v, push := newViewPublisher(t, fixedRecords{}, nil, nil)
	// Act.
	v.PublishSession("/home/u/ws", "", "s1")
	// Assert.
	if len(push.gates) != 0 {
		t.Fatalf("published an unfenced gate: %v", push.gates)
	}
}

// lockProbePush answers, at push time, whether the publisher's own mutex was
// held. That is the whole of "retention and publication are one step": a push
// that reaches the wire with the lock free is a push a concurrent resolution
// can overtake, retaining the newer view and broadcasting the older one.
//
// TryLock is the probe rather than a second goroutine because it needs no
// timing at all — the answer is a fact about this instant, not a race whose
// outcome a test would have to wait on.
type lockProbePush struct {
	v                                   *WorkspaceViews
	topbarHeld, gateHeld, breakdownHeld []bool
}

// probe reports whether v.mu was held at this instant.
func (p *lockProbePush) probe() bool {
	if p.v.mu.TryLock() {
		p.v.mu.Unlock()
		return false
	}
	return true
}

func (p *lockProbePush) PushTopbarView(*frontendv1.TopbarView) {
	p.topbarHeld = append(p.topbarHeld, p.probe())
}

func (p *lockProbePush) PushTokenBreakdownView(*frontendv1.TokenBreakdownView) {
	p.breakdownHeld = append(p.breakdownHeld, p.probe())
}

func (p *lockProbePush) PushWorkspaceGateView(*frontendv1.WorkspaceGateView) {
	p.gateHeld = append(p.gateHeld, p.probe())
}

// newLockProbePublisher builds a publisher whose push probes the mutex.
func newLockProbePublisher(records SessionRecordSource) (*WorkspaceViews, *lockProbePush) {
	probe := &lockProbePush{}
	v := NewWorkspaceViews(func(string, ...any) {}, probe, records, nil, nil, nil, nil)
	probe.v = v
	return v, probe
}

func TestTheTopbarIsBroadcastUnderTheSameHoldThatRetainedIt(t *testing.T) {
	// Arrange.
	v, probe := newLockProbePublisher(nil)
	// Act.
	v.PublishState(liveState())
	// Assert.
	if len(probe.topbarHeld) != 1 || !probe.topbarHeld[0] {
		t.Fatalf("topbar push observed the lock held = %v, want one push with the lock held", probe.topbarHeld)
	}
}

func TestTheGateIsBroadcastUnderTheSameHoldThatRetainedIt(t *testing.T) {
	// Arrange — the gate alone, published off the session record.
	v, probe := newLockProbePublisher(fixedRecords{})
	// Act.
	v.PublishSession("/home/u/ws", "s1|g1", "s1")
	// Assert.
	if len(probe.gateHeld) != 1 || !probe.gateHeld[0] {
		t.Fatalf("gate push observed the lock held = %v, want one push with the lock held", probe.gateHeld)
	}
}

func TestTheTokenBreakdownIsBroadcastUnderTheSameHoldThatRetainedIt(t *testing.T) {
	// Arrange.
	v, probe := newLockProbePublisher(nil)
	// Act.
	v.PublishTokenBreakdown("/home/u/ws", "s1|g1", nil)
	// Assert.
	if len(probe.breakdownHeld) != 1 || !probe.breakdownHeld[0] {
		t.Fatalf("breakdown push observed the lock held = %v, want one push with the lock held", probe.breakdownHeld)
	}
}

func TestForgettingAWorkspaceDropsItFromTheSnapshot(t *testing.T) {
	// Arrange — the snapshot must stop carrying a topbar for a workspace
	// nothing runs.
	v, _ := newViewPublisher(t, nil, nil, nil)
	v.PublishState(liveState())
	// Act.
	v.Forget("/home/u/ws")
	// Assert.
	if got := v.Topbars(); len(got) != 0 {
		t.Fatalf("snapshot topbars after Forget = %v, want none", got)
	}
}

// --- the accounting republication -----------------------------------------
//
// The accounting line is a TOPBAR fact that moves on the PROGRESS resolver's
// clock, downstream of the state transition that publishes the topbar. Without
// a republication from that clock, the settled sentence never reaches a client
// that connects after the last transition — and never reaches the retention the
// snapshot serves from either.

// settledProgress is a progress source that starts empty and then reports a
// settled accounting sentence, so a test can move the accounting WITHOUT moving
// the workspace state — which is the situation the republication exists for.
type settledProgress struct{ summary string }

func (s *settledProgress) Current(string) (*frontendv1.ProgressView, bool) {
	return &frontendv1.ProgressView{
		Accounting: &frontendv1.FooterAccountingCell{Summary: s.summary},
	}, true
}

func TestASettledAccountingLineRepublishesTheTopbar(t *testing.T) {
	// Arrange — a topbar published while the accounting was still empty.
	progress := &settledProgress{}
	v, push, _ := newRepublishingPublisher(t, progress, fixedStates{state: liveState(), found: true})
	v.PublishState(liveState())

	// Act — the resolver settles the cell and the progress subscription reports it.
	progress.summary = "5h 1.0%→2.0% (1.0pp)"
	v.RepublishAccounting("/home/u/ws")

	// Assert
	if len(push.topbars) != 2 {
		t.Fatalf("published %d topbars, want the settled accounting line republished: a client connecting after the last state transition otherwise holds a topbar whose accounting never settles", len(push.topbars))
	}
}

func TestTheRepublishedTopbarCarriesTheSettledSentence(t *testing.T) {
	// Arrange
	progress := &settledProgress{}
	v, push, _ := newRepublishingPublisher(t, progress, fixedStates{state: liveState(), found: true})
	v.PublishState(liveState())

	// Act
	progress.summary = "5h 1.0%→2.0% (1.0pp)"
	v.RepublishAccounting("/home/u/ws")

	// Assert
	if got := push.topbars[1].GetAccountingLine(); got != "5h 1.0%→2.0% (1.0pp)" {
		t.Fatalf("republished accounting line = %q, want the settled sentence", got)
	}
}

func TestTheRepublishedTopbarReachesTheConnectSnapshot(t *testing.T) {
	// Arrange
	progress := &settledProgress{}
	v, _, _ := newRepublishingPublisher(t, progress, fixedStates{state: liveState(), found: true})
	v.PublishState(liveState())

	// Act
	progress.summary = "5h 1.0%→2.0% (1.0pp)"
	v.RepublishAccounting("/home/u/ws")

	// Assert
	topbars := v.Topbars()
	if len(topbars) != 1 || topbars[0].GetAccountingLine() != "5h 1.0%→2.0% (1.0pp)" {
		t.Fatalf("snapshot topbars = %v, want the retention advanced to the settled sentence", topbars)
	}
}

func TestAnUnchangedAccountingLineRepublishesNothing(t *testing.T) {
	// Arrange — change is judged on the rendered view, so a republication that
	// moved nothing must stay off the wire.
	progress := &settledProgress{summary: "5h 1.0%→2.0% (1.0pp)"}
	v, push, _ := newRepublishingPublisher(t, progress, fixedStates{state: liveState(), found: true})
	v.PublishState(liveState())

	// Act
	v.RepublishAccounting("/home/u/ws")

	// Assert
	if len(push.topbars) != 1 {
		t.Fatalf("published %d topbars, want 1: an identical re-render is not a new view", len(push.topbars))
	}
}

func TestARepublicationForAWorkspaceWithNoStateIsRecorded(t *testing.T) {
	// Arrange
	v, push, lines := newRepublishingPublisher(t, &settledProgress{}, fixedStates{})

	// Act
	v.RepublishAccounting("/home/u/ws")

	// Assert
	if len(push.topbars) != 0 || len(*lines) != 1 {
		t.Fatalf("published %d topbars and recorded %v, want an unfenced topbar withheld and the withholding recorded", len(push.topbars), *lines)
	}
}

func TestARepublicationOverAFailedStateReadIsRecorded(t *testing.T) {
	// Arrange
	v, push, lines := newRepublishingPublisher(t, &settledProgress{}, fixedStates{err: errors.New("store closed")})

	// Act
	v.RepublishAccounting("/home/u/ws")

	// Assert
	if len(push.topbars) != 0 || len(*lines) != 1 {
		t.Fatalf("published %d topbars and recorded %v, want the read failure surfaced rather than absorbed", len(push.topbars), *lines)
	}
}

func TestARepublicationWithNoStateSourceIsRecorded(t *testing.T) {
	// Arrange — an unwired state source must say so rather than return silently.
	v, push, lines := newRepublishingPublisher(t, &settledProgress{}, nil)

	// Act
	v.RepublishAccounting("/home/u/ws")

	// Assert
	if len(push.topbars) != 0 || len(*lines) != 1 {
		t.Fatalf("published %d topbars and recorded %v, want the unwired source named", len(push.topbars), *lines)
	}
}

func TestAKeylessRepublicationIsRecorded(t *testing.T) {
	// Arrange
	v, push, lines := newRepublishingPublisher(t, &settledProgress{}, fixedStates{state: liveState(), found: true})

	// Act
	v.RepublishAccounting("")

	// Assert
	if len(push.topbars) != 0 || len(*lines) != 1 {
		t.Fatalf("published %d topbars and recorded %v, want the keyless republication declined loudly", len(push.topbars), *lines)
	}
}
