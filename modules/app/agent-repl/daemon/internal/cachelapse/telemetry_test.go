package cachelapse

import (
	"context"
	"errors"
	"fmt"
	"sync"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/ids"
	"agent-repld/internal/shimclient"
	"agent-repld/internal/wsm"
)

type beatObservation struct {
	outcome string
	late    time.Duration
}

type gateObservation struct {
	outcome string
	idle    time.Duration
}

// fakeTelemetry records what the Keeper reports.
type fakeTelemetry struct {
	mu     sync.Mutex
	beats  []beatObservation
	lapses []string
	gates  []gateObservation
}

func (f *fakeTelemetry) ObserveBeat(_ ids.WorkspaceID, outcome string, late time.Duration) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.beats = append(f.beats, beatObservation{outcome, late})
}

func (f *fakeTelemetry) CountLapse(_ ids.WorkspaceID, outcome string) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.lapses = append(f.lapses, outcome)
}

func (f *fakeTelemetry) ObserveColdGate(_ ids.WorkspaceID, outcome string, idle time.Duration) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.gates = append(f.gates, gateObservation{outcome, idle})
}

func TestADueBeatIsReportedByHowItEnded(t *testing.T) {
	busy := skipped(&shimv1.SendKeepaliveSkipped{Reason: &shimv1.SendKeepaliveSkipped_MainAgentBusy{MainAgentBusy: &shimv1.SendKeepaliveSkippedMainAgentBusy{}}})
	detached := skipped(&shimv1.SendKeepaliveSkipped{Reason: &shimv1.SendKeepaliveSkipped_DetachedWorkLive{DetachedWorkLive: &shimv1.SendKeepaliveSkippedDetachedWorkLive{}}})
	lapsed := skipped(&shimv1.SendKeepaliveSkipped{Reason: &shimv1.SendKeepaliveSkipped_CacheLapsed{CacheLapsed: &shimv1.SendKeepaliveSkippedCacheLapsed{LapsedAtMs: 1}}})
	tests := []struct {
		name    string
		answer  *shimv1.SendKeepaliveSuccess
		err     error
		outcome string
	}{
		{name: "sent", answer: sent(), outcome: BeatOutcomeSent},
		{name: "main agent busy", answer: busy, outcome: BeatOutcomeMainAgentBusy},
		{name: "detached work live", answer: detached, outcome: BeatOutcomeDetachedWorkLive},
		{name: "cache lapsed", answer: lapsed, outcome: BeatOutcomeCacheLapsed},
		{name: "a judgment this daemon does not name", answer: &shimv1.SendKeepaliveSuccess{}, outcome: BeatOutcomeUnnamedJudgment},
		{name: "a failed submission", err: &BeatSubmitFailed{Detail: "boom"}, outcome: BeatOutcomeSubmitFailed},
		{name: "an unreachable shim", err: errors.New("connection refused"), outcome: BeatOutcomeShimUnreachable},
		{name: "a shim that died under the call", err: fmt.Errorf("%w: eof", shimclient.ErrShimDied), outcome: BeatOutcomeShimUnreachable},
		{name: "a shim with no session", err: ErrNoSession, outcome: BeatOutcomeNoSession},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := dueHarness(t, tt.answer)
			h.shim.beatErr = tt.err
			h.keeper.beats[theWorkspace] = now

			// Act
			h.keeper.beatsDue(context.Background())

			// Assert: the fixture's beat came due a minute before the clock.
			want := []beatObservation{{tt.outcome, time.Minute}}
			if len(h.telemetry.beats) != 1 || h.telemetry.beats[0] != want[0] {
				t.Fatalf("beats = %+v, want %+v", h.telemetry.beats, want)
			}
		})
	}
}

func TestADueBeatWithNoSessionToAskIsReported(t *testing.T) {
	tests := []struct {
		name    string
		session func() wsm.Session
		outcome string
	}{
		{name: "a parked session", session: func() wsm.Session { return parkedSession(theWorkspace) }, outcome: BeatOutcomeSessionParked},
		{name: "no session served", session: func() wsm.Session { return liveSession(theWorkspace) }, outcome: BeatOutcomeNotServed},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := dueHarness(t, sent())
			h.db.sessions[theWorkspace] = tt.session()
			h.serving = false
			h.keeper.beats[theWorkspace] = now

			// Act
			h.keeper.beatsDue(context.Background())

			// Assert
			if len(h.telemetry.beats) != 1 || h.telemetry.beats[0].outcome != tt.outcome {
				t.Fatalf("beats = %+v, want one %s", h.telemetry.beats, tt.outcome)
			}
		})
	}
}

func TestABeatATurnMovedIsNotReported(t *testing.T) {
	// Arrange
	h := newHarness(t, beatable(visible(), time.Minute))
	h.shim.answer = sent()
	h.keeper.beats[theWorkspace] = now

	// Act
	h.keeper.beatsDue(context.Background())

	// Assert
	if len(h.telemetry.beats) != 0 {
		t.Fatalf("beats = %+v, want none for a re-armed beat", h.telemetry.beats)
	}
}

func TestACameDueLapseIsReportedByWhatBecameOfIt(t *testing.T) {
	tests := []struct {
		name    string
		rec     func() wsm.Workspace
		queued  bool
		outcome string
	}{
		{name: "stood down", rec: func() wsm.Workspace {
			rec := hidden()
			rec.CacheLapse = &wsm.CacheLapse{LapsesAt: lapseAt}
			return rec
		}, outcome: LapseStoodDown},
		{name: "shown first", rec: visible, outcome: LapseShownFirst},
		{name: "kept for a queued merge", rec: func() wsm.Workspace {
			rec := hidden()
			rec.CacheLapse = &wsm.CacheLapse{LapsesAt: lapseAt}
			return rec
		}, queued: true, outcome: LapseMergeQueued},
		{name: "moved later", rec: func() wsm.Workspace {
			rec := hidden()
			rec.CacheLapse = &wsm.CacheLapse{LapsesAt: lapseAt.Add(time.Hour)}
			return rec
		}, outcome: LapseMovedLater},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t, tt.rec())
			if tt.queued {
				h.db.queued = append(h.db.queued, theWorkspace)
			}
			h.keeper.deadlines[theWorkspace] = lapseAt
			h.clock.Set(lapseAt)

			// Act
			h.keeper.lapseDue(context.Background())

			// Assert
			if len(h.telemetry.lapses) != 1 || h.telemetry.lapses[0] != tt.outcome {
				t.Fatalf("lapses = %v, want [%s]", h.telemetry.lapses, tt.outcome)
			}
		})
	}
}

func TestAColdGateIsReportedByItsKindAndIdleTime(t *testing.T) {
	lapsedAfter := func(ttl time.Duration) *conversationv1.SessionCold_Lapsed {
		return &conversationv1.SessionCold_Lapsed{Lapsed: &conversationv1.SessionColdLapsed{CacheTtlMs: ttl.Milliseconds()}}
	}
	tests := []struct {
		name    string
		cold    *conversationv1.SessionCold
		outcome string
	}{
		{name: "a five-minute lifetime", cold: &conversationv1.SessionCold{Reason: lapsedAfter(5 * time.Minute)}, outcome: ColdGateLapsedFiveMin},
		{name: "a one-hour lifetime", cold: &conversationv1.SessionCold{Reason: lapsedAfter(time.Hour)}, outcome: ColdGateLapsedOneHour},
		{name: "another lifetime", cold: &conversationv1.SessionCold{Reason: lapsedAfter(90 * time.Minute)}, outcome: ColdGateLapsedOtherTTL},
		{name: "a model switch", cold: &conversationv1.SessionCold{Reason: &conversationv1.SessionCold_ModelSwitch{ModelSwitch: &conversationv1.SessionColdModelSwitch{}}}, outcome: ColdGateModelSwitch},
		{name: "a reason this daemon does not name", cold: &conversationv1.SessionCold{}, outcome: ColdGateUnnamedReason},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t, visible())
			tt.cold.LastRequestAtMs = now.Add(-2 * time.Hour).UnixMilli()
			h.gate = tt.cold

			// Act
			h.keeper.ColdGateRaised(theWorkspace, true)

			// Assert
			want := gateObservation{tt.outcome, 2 * time.Hour}
			if len(h.telemetry.gates) != 1 || h.telemetry.gates[0] != want {
				t.Fatalf("gates = %+v, want [%+v]", h.telemetry.gates, want)
			}
		})
	}
}

func TestAColdGateThatNoLongerStandsIsNotReported(t *testing.T) {
	// Arrange
	h := newHarness(t, visible())

	// Act
	h.keeper.ColdGateRaised(theWorkspace, true)

	// Assert
	if len(h.telemetry.gates) != 0 {
		t.Fatalf("gates = %+v, want none", h.telemetry.gates)
	}
}

// TestAGateRaisedAgainIsNotCountedAgain pins that a gate is counted once, when
// a shim judged it: a raise from its record, a re-raise after a failed answer
// and a carried gate are the same gate.
func TestAGateRaisedAgainIsNotCountedAgain(t *testing.T) {
	// Arrange
	h := newHarness(t, visible())
	h.gate = &conversationv1.SessionCold{LastRequestAtMs: now.Add(-time.Hour).UnixMilli()}

	// Act
	h.keeper.ColdGateRaised(theWorkspace, false)

	// Assert
	if len(h.telemetry.gates) != 0 {
		t.Fatalf("gates = %+v, want none for a gate raised again", h.telemetry.gates)
	}
}
