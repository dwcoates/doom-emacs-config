package cachelapse

import (
	"context"
	"errors"
	"testing"
	"time"

	"google.golang.org/protobuf/proto"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/sourcescan"
	"agent-repld/internal/wsm"
)

func (f *fakeDB) SetProjectedColdGate(_ context.Context, id wsm.WorkspaceID, dueAt time.Time, cold *conversationv1.SessionCold) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.projectedErr != nil {
		return f.projectedErr
	}
	if f.projected == nil {
		f.projected = map[ids.WorkspaceID]wsm.ProjectedColdGate{}
	}
	f.projected[id] = wsm.ProjectedColdGate{Workspace: id, DueAt: dueAt, Cold: cold}
	return nil
}

func (f *fakeDB) ClearProjectedColdGate(_ context.Context, id wsm.WorkspaceID) (bool, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.projectedErr != nil {
		return false, f.projectedErr
	}
	_, had := f.projected[id]
	delete(f.projected, id)
	return had, nil
}

func (f *fakeDB) ProjectedColdGate(_ context.Context, id wsm.WorkspaceID) (wsm.ProjectedColdGate, bool, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.projectedErr != nil {
		return wsm.ProjectedColdGate{}, false, f.projectedErr
	}
	p, ok := f.projected[id]
	return p, ok, nil
}

func (f *fakeDB) ProjectedColdGates(context.Context) ([]wsm.ProjectedColdGate, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.projectedErr != nil {
		return nil, f.projectedErr
	}
	var all []wsm.ProjectedColdGate
	for _, p := range f.projected {
		all = append(all, p)
	}
	return all, nil
}

// statedGate is a projection whose gated verdict states every fact the gate
// will carry once the cache lapses at AT.
func statedGate(at time.Time, tokens uint64) *conversationv1.SessionCacheLapse {
	return &conversationv1.SessionCacheLapse{
		LapsesAtMs: at.UnixMilli(),
		AfterLapse: &conversationv1.SessionCacheLapse_Gated{Gated: &conversationv1.SessionCacheLapseGated{
			ContextTokens: tokens, LastRequestAtMs: at.Add(-time.Hour).UnixMilli(), CacheTtlMs: time.Hour.Milliseconds(),
		}},
	}
}

func projectedHarness(t *testing.T, rec wsm.Workspace) *harness {
	t.Helper()
	return newHarness(t, rec)
}

func visibleWorkspace() wsm.Workspace { return wsm.Workspace{ID: theWorkspace} }

func TestAGatedProjectionIsKeptAndArmedForAnyWorkspace(t *testing.T) {
	// Arrange
	h := projectedHarness(t, visibleWorkspace())
	due := h.clock.Now().Add(time.Hour)

	// Act
	h.keeper.OnCacheLapse(theWorkspace, statedGate(due, 120_000))
	h.applyQueued()

	// Assert
	got, ok := h.db.projected[theWorkspace]
	want := &conversationv1.SessionCold{
		ContextTokens: 120_000, LastRequestAtMs: due.Add(-time.Hour).UnixMilli(),
		Reason: &conversationv1.SessionCold_Lapsed{Lapsed: &conversationv1.SessionColdLapsed{CacheTtlMs: time.Hour.Milliseconds()}},
	}
	if !ok || !got.DueAt.Equal(due.UTC()) || !proto.Equal(got.Cold, want) || !h.keeper.gateDue[theWorkspace].Equal(due.UTC()) {
		t.Fatalf("projected = %+v (%v), armed %v; want %v due %v kept and armed", got, ok, h.keeper.gateDue[theWorkspace], want, due)
	}
}

func TestAProjectionThatRaisesNoGateRetiresTheOneKept(t *testing.T) {
	tests := []struct {
		name  string
		lapse func(at time.Time) *conversationv1.SessionCacheLapse
	}{
		{name: "an ungated verdict", lapse: func(at time.Time) *conversationv1.SessionCacheLapse { return stated(at, nil) }},
		{name: "a gated verdict from a shim that states no last request", lapse: func(at time.Time) *conversationv1.SessionCacheLapse {
			tokens := uint64(120_000)
			return stated(at, &tokens)
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := projectedHarness(t, visibleWorkspace())
			due := h.clock.Now().Add(time.Hour)
			h.keeper.OnCacheLapse(theWorkspace, statedGate(due, 120_000))
			h.applyQueued()

			// Act
			h.keeper.OnCacheLapse(theWorkspace, tt.lapse(due))
			h.applyQueued()

			// Assert
			if _, kept := h.db.projected[theWorkspace]; kept {
				t.Fatalf("projection kept, want it retired")
			}
			if _, armed := h.keeper.gateDue[theWorkspace]; armed {
				t.Fatalf("gate instant still armed, want it disarmed")
			}
		})
	}
}

func TestAProjectionThatCannotBeRecordedIsLoggedAndNotArmed(t *testing.T) {
	// Arrange
	h := projectedHarness(t, visibleWorkspace())
	h.db.projectedErr = errors.New("disk full")

	// Act
	h.keeper.OnCacheLapse(theWorkspace, statedGate(h.clock.Now().Add(time.Hour), 120_000))
	h.applyQueued()

	// Assert
	if !h.found(dlog.RecordQuery{Level: "error", Operation: opGate, Message: "could not record the workspace's projected cold gate; its gate is raised at the next prompt as before"}) {
		t.Fatalf("no ERROR record for the projection that could not be recorded: %v", h.log.Records())
	}
	if _, armed := h.keeper.gateDue[theWorkspace]; armed {
		t.Fatalf("gate instant armed after a failed write")
	}
}

func TestGatesDueRaisesTheGateTheMomentItsInstantComes(t *testing.T) {
	// Arrange
	h := projectedHarness(t, visibleWorkspace())
	h.raiseOutcome = true
	due := h.clock.Now().Add(time.Hour)
	h.keeper.OnCacheLapse(theWorkspace, statedGate(due, 120_000))
	h.applyQueued()
	h.clock.Set(h.clock.Now().Add(time.Hour))

	// Act
	h.keeper.gatesDue(context.Background())

	// Assert
	if len(h.raised) != 1 || h.raised[0] != theWorkspace {
		t.Fatalf("raised = %v, want the gate raised once for the workspace", h.raised)
	}
	if _, armed := h.keeper.gateDue[theWorkspace]; armed {
		t.Fatalf("gate instant still armed after the raise")
	}
}

func TestGatesDueKeepsAnInstantNotYetDue(t *testing.T) {
	// Arrange
	h := projectedHarness(t, visibleWorkspace())
	h.keeper.OnCacheLapse(theWorkspace, statedGate(h.clock.Now().Add(time.Hour), 120_000))
	h.applyQueued()
	h.clock.Set(h.clock.Now().Add(time.Minute))

	// Act
	h.keeper.gatesDue(context.Background())

	// Assert
	if len(h.raised) != 0 {
		t.Fatalf("raised = %v, want none before the instant", h.raised)
	}
}

func TestRaiseGateLeavesAWorkspaceItShouldNotRaiseFor(t *testing.T) {
	hidden := visibleWorkspace()
	hidden.Hidden = true
	closed := visibleWorkspace()
	closed.Closed = true
	tests := []struct {
		name string
		rec  wsm.Workspace
		gate *conversationv1.SessionCold
	}{
		{name: "a hidden workspace, whose cache lapse owns it", rec: hidden},
		{name: "a closed workspace, whose cache lapse owns it", rec: closed},
		{name: "a workspace a cold gate already stands on", rec: visibleWorkspace(), gate: &conversationv1.SessionCold{ContextTokens: 1}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := projectedHarness(t, tt.rec)
			h.gate = tt.gate
			h.db.projected = map[ids.WorkspaceID]wsm.ProjectedColdGate{theWorkspace: {
				Workspace: theWorkspace, DueAt: h.clock.Now(), Cold: &conversationv1.SessionCold{ContextTokens: 120_000},
			}}

			// Act
			h.keeper.raiseGate(context.Background(), theWorkspace, h.clock.Now())

			// Assert
			if len(h.raised) != 0 {
				t.Fatalf("raised = %v, want no raise", h.raised)
			}
		})
	}
}

func TestRaiseGateRetiresTheProjectionOfAGateThatAlreadyStands(t *testing.T) {
	// Arrange
	h := projectedHarness(t, visibleWorkspace())
	h.gate = &conversationv1.SessionCold{ContextTokens: 1}
	h.db.projected = map[ids.WorkspaceID]wsm.ProjectedColdGate{theWorkspace: {Workspace: theWorkspace, DueAt: h.clock.Now(), Cold: &conversationv1.SessionCold{}}}

	// Act
	h.keeper.raiseGate(context.Background(), theWorkspace, h.clock.Now())

	// Assert
	if _, kept := h.db.projected[theWorkspace]; kept {
		t.Fatalf("projection kept beside a standing gate, want it retired")
	}
}

func TestRaiseGateRearmsAProjectionRestatedLater(t *testing.T) {
	// Arrange
	h := projectedHarness(t, visibleWorkspace())
	later := h.clock.Now().Add(time.Hour)
	h.db.projected = map[ids.WorkspaceID]wsm.ProjectedColdGate{theWorkspace: {Workspace: theWorkspace, DueAt: later, Cold: &conversationv1.SessionCold{}}}

	// Act
	h.keeper.raiseGate(context.Background(), theWorkspace, h.clock.Now())

	// Assert
	if len(h.raised) != 0 || !h.keeper.gateDue[theWorkspace].Equal(later) {
		t.Fatalf("raised %v, armed %v; want no raise and the later instant armed", h.raised, h.keeper.gateDue[theWorkspace])
	}
}

func TestRaiseGateThatTheFleetDeclinesIsRecordedAndLeftForTheNextProjection(t *testing.T) {
	// Arrange
	h := projectedHarness(t, visibleWorkspace())
	h.raiseOutcome = false
	h.db.projected = map[ids.WorkspaceID]wsm.ProjectedColdGate{theWorkspace: {Workspace: theWorkspace, DueAt: h.clock.Now(), Cold: &conversationv1.SessionCold{ContextTokens: 120_000}}}

	// Act
	h.keeper.raiseGate(context.Background(), theWorkspace, h.clock.Now())

	// Assert
	if !h.found(dlog.RecordQuery{Level: "info", Operation: opGate, Message: "the workspace's projected cold gate is not raised: a turn runs, no session is held, or its read is already paid for; the shim states the next projection when the cache moves"}) {
		t.Fatalf("no INFO record of the declined raise: %v", h.log.Records())
	}
	if _, armed := h.keeper.gateDue[theWorkspace]; armed {
		t.Fatalf("gate instant armed after a declined raise, want it left to the next projection")
	}
}

func TestRaiseGateThatFailsIsLoggedAndAskedAgainShortly(t *testing.T) {
	// Arrange
	h := projectedHarness(t, visibleWorkspace())
	h.raiseErr = errors.New("session read failed")
	h.db.projected = map[ids.WorkspaceID]wsm.ProjectedColdGate{theWorkspace: {Workspace: theWorkspace, DueAt: h.clock.Now(), Cold: &conversationv1.SessionCold{ContextTokens: 120_000}}}
	now := h.clock.Now()

	// Act
	h.keeper.raiseGate(context.Background(), theWorkspace, now)

	// Assert
	if !h.found(dlog.RecordQuery{Level: "error", Operation: opGate, Message: "could not raise the workspace's projected cold gate; it is asked again shortly"}) {
		t.Fatalf("no ERROR record of the failed raise: %v", h.log.Records())
	}
	if !h.keeper.gateDue[theWorkspace].Equal(now.Add(BeatRetry)) {
		t.Fatalf("armed %v, want a retry at %v", h.keeper.gateDue[theWorkspace], now.Add(BeatRetry))
	}
}

func TestRestoreArmsEveryRecordedProjection(t *testing.T) {
	// Arrange
	h := projectedHarness(t, visibleWorkspace())
	due := h.clock.Now().Add(time.Hour)
	h.db.projected = map[ids.WorkspaceID]wsm.ProjectedColdGate{theWorkspace: {Workspace: theWorkspace, DueAt: due, Cold: &conversationv1.SessionCold{}}}
	h.db.listErr = nil

	// Act
	err := h.keeper.restore(context.Background())

	// Assert
	if err != nil || !h.keeper.gateDue[theWorkspace].Equal(due) {
		t.Fatalf("restore = %v, armed %v; want the recorded instant armed", err, h.keeper.gateDue[theWorkspace])
	}
}

func TestRestoreThatCannotListProjectionsStopsTheKeeperLoudly(t *testing.T) {
	// Arrange
	h := projectedHarness(t, visibleWorkspace())
	h.db.projectedErr = errors.New("read failed")

	// Act
	err := h.keeper.restore(context.Background())

	// Assert
	if err == nil || !h.found(dlog.RecordQuery{Level: "error", Operation: opGate, Message: "could not list the projected cold gates to re-arm them; the cache keeper stops, and no keep-alive goes out for any workspace until the next daemon runs its own"}) {
		t.Fatalf("restore = %v; want an error and an ERROR record", err)
	}
}

func TestAGateRaisedRetiresItsProjection(t *testing.T) {
	// Arrange
	h := projectedHarness(t, visibleWorkspace())
	h.keeper.OnCacheLapse(theWorkspace, statedGate(h.clock.Now().Add(time.Hour), 120_000))
	h.applyQueued()
	h.gate = &conversationv1.SessionCold{ContextTokens: 120_000, LastRequestAtMs: 1}

	// Act
	h.keeper.ColdGateRaised(theWorkspace, true)
	h.applyQueued()

	// Assert
	if _, kept := h.db.projected[theWorkspace]; kept {
		t.Fatalf("projection kept after its gate was raised, want it retired")
	}
}

func TestAGateRaisedParksItsSessionForTheWorkspaceInView(t *testing.T) {
	// Arrange
	h := projectedHarness(t, visibleWorkspace())

	// Act
	h.keeper.ColdGateRaised(theWorkspace, false)

	// Assert
	if len(h.coldDowns) != 1 || !h.coldDowns[0].atGate {
		t.Fatalf("cold parks = %+v, want one park taken at the gate", h.coldDowns)
	}
}

func TestProjectedGateWritesHaveOneHome(t *testing.T) {
	// Act
	sites := sourcescan.Sites(t, ".SetProjectedColdGate(ctx")

	// Assert
	if len(sites) != 1 || sites[0] != "projectedgate.go" {
		t.Fatalf("a projected gate is written at %v, want only Keeper.projectGate in projectedgate.go", sites)
	}
}
