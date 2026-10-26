package cachelapse

import (
	"context"
	"errors"
	"fmt"
	"testing"
	"time"

	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/shimclient"
	"agent-repld/internal/wsm"
)

// parkedSession is a session the keep-alive cycle stood down.
func parkedSession(ws ids.WorkspaceID) wsm.Session {
	s := liveSession(ws)
	s.Terminal = &wsm.SessionTerminal{Kind: wsm.TerminalKeepaliveCycle}
	return s
}

// dueHarness is a visible workspace whose beat is already due, its shim
// answering ANSWER.
func dueHarness(t *testing.T, answer *shimv1.SendKeepaliveSuccess) *harness {
	t.Helper()
	h := newHarness(t, beatable(visible(), KeepaliveInterval+time.Minute))
	h.shim.answer = answer
	return h
}

func skipped(s *shimv1.SendKeepaliveSkipped) *shimv1.SendKeepaliveSuccess {
	return &shimv1.SendKeepaliveSuccess{Outcome: &shimv1.SendKeepaliveSuccess_Skipped{Skipped: s}}
}

func TestScheduleArmsTheBeatAnIntervalAfterTheLastTurn(t *testing.T) {
	// Arrange
	h := newHarness(t, beatable(visible(), 10*time.Minute))

	// Act
	h.keeper.schedule(context.Background(), theWorkspace, "test")

	// Assert
	if got, want := h.keeper.beats[theWorkspace], now.Add(-10*time.Minute).Add(KeepaliveInterval); !got.Equal(want) {
		t.Fatalf("beat = %v, want %v", got, want)
	}
}

func TestScheduleArmsAParkedSessionsRestoreALeadBeforeItsBeat(t *testing.T) {
	// Arrange
	h := newHarness(t, beatable(visible(), 10*time.Minute))
	h.db.sessions[theWorkspace] = parkedSession(theWorkspace)

	// Act
	h.keeper.schedule(context.Background(), theWorkspace, "test")

	// Assert
	if got, want := h.keeper.restores[theWorkspace], h.keeper.beats[theWorkspace].Add(-RestoreLead); !got.Equal(want) {
		t.Fatalf("restore = %v, want %v", got, want)
	}
}

func TestScheduleArmsNoRestoreForALiveSession(t *testing.T) {
	// Arrange
	h := newHarness(t, beatable(visible(), 10*time.Minute))

	// Act
	h.keeper.schedule(context.Background(), theWorkspace, "test")

	// Assert
	if _, armed := h.keeper.restores[theWorkspace]; armed {
		t.Fatal("a live session's restore was armed")
	}
}

func TestScheduleArmsNoBeat(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness)
	}{
		{name: "when no turn has ended", arrange: func(h *harness) { h.db.records[theWorkspace] = visible() }},
		{name: "when the cache lapsed and no turn moved its clock", arrange: func(h *harness) {
			h.keeper.lapsedAt[theWorkspace] = *h.db.records[theWorkspace].LastTurnEndedAt
		}},
		{name: "when the session stands at its cold gate", arrange: func(h *harness) { h.gate = coldLapsed() }},
		{name: "when there is no session", arrange: func(h *harness) { delete(h.db.sessions, theWorkspace) }},
		{name: "when the session is dead", arrange: func(h *harness) {
			s := liveSession(theWorkspace)
			s.Terminal = &wsm.SessionTerminal{Kind: "killed"}
			h.db.sessions[theWorkspace] = s
		}},
		{name: "when the session is hibernated", arrange: func(h *harness) {
			s := liveSession(theWorkspace)
			s.Terminal = &wsm.SessionTerminal{Kind: wsm.TerminalHibernated}
			h.db.sessions[theWorkspace] = s
		}},
		{name: "when it will be idle past the cutoff by the beat", arrange: func(h *harness) {
			s := liveSession(theWorkspace)
			s.LastEngagementAt = now.Add(-6 * time.Hour)
			h.db.sessions[theWorkspace] = s
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t, beatable(visible(), 10*time.Minute))
			tt.arrange(h)

			// Act
			h.keeper.schedule(context.Background(), theWorkspace, "test")

			// Assert
			if _, armed := h.keeper.beats[theWorkspace]; armed {
				t.Fatal("a beat was armed")
			}
		})
	}
}

func TestScheduleBeatsAgainOnceATurnMovesALapsedClock(t *testing.T) {
	// Arrange
	h := newHarness(t, beatable(visible(), 10*time.Minute))
	h.keeper.lapsedAt[theWorkspace] = now.Add(-time.Hour)

	// Act
	h.keeper.schedule(context.Background(), theWorkspace, "test")

	// Assert
	if _, armed := h.keeper.beats[theWorkspace]; !armed {
		t.Fatal("no beat was armed after a turn moved the clock past the lapse")
	}
}

func TestADueBeatIsSent(t *testing.T) {
	// Arrange
	h := dueHarness(t, sent())
	h.keeper.beats[theWorkspace] = now

	// Act
	h.keeper.beatsDue(context.Background())

	// Assert
	if h.shim.beats != 1 {
		t.Fatalf("beats = %d, want 1", h.shim.beats)
	}
}

func TestASentBeatAsksForTheCycleStandDown(t *testing.T) {
	// Arrange
	h := dueHarness(t, sent())
	h.keeper.beats[theWorkspace] = now

	// Act
	h.keeper.beatsDue(context.Background())

	// Assert
	if len(h.cycled) != 1 || h.cycled[0].ws != theWorkspace || !h.cycled[0].beat.Equal(now) {
		t.Fatalf("cycle stand-downs = %+v, want one at the beat", h.cycled)
	}
}

func TestASentBeatArmsAFallbackBeat(t *testing.T) {
	// Arrange
	h := dueHarness(t, sent())
	h.keeper.beats[theWorkspace] = now

	// Act
	h.keeper.beatsDue(context.Background())

	// Assert
	if got := h.keeper.beats[theWorkspace]; !got.Equal(now.Add(KeepaliveInterval)) {
		t.Fatalf("fallback beat = %v, want an interval on", got)
	}
}

func TestASentBeatIsRecorded(t *testing.T) {
	// Arrange
	h := dueHarness(t, sent())
	h.keeper.beats[theWorkspace] = now

	// Act
	h.keeper.beatsDue(context.Background())

	// Assert
	rec, _ := h.db.Workspace(context.Background(), theWorkspace)
	if rec.LastKeepaliveAt == nil || !rec.LastKeepaliveAt.Equal(now) {
		t.Fatalf("LastKeepaliveAt = %v, want the beat %v", rec.LastKeepaliveAt, now)
	}
}

// TestTheStandDownAfterASentBeatTimesTheNextBeatAnIntervalOn pins the
// 2026-10-26 loop: the stand-down's edge re-timed the next beat from the last
// turn alone, already overdue, so the session was restored and beat again
// within seconds.
func TestTheStandDownAfterASentBeatTimesTheNextBeatAnIntervalOn(t *testing.T) {
	// Arrange
	h := dueHarness(t, sent())
	h.keeper.beats[theWorkspace] = now
	h.keeper.beatsDue(context.Background())

	// Act
	h.keeper.schedule(context.Background(), theWorkspace, "test")

	// Assert
	if got := h.keeper.beats[theWorkspace]; !got.Equal(now.Add(KeepaliveInterval)) {
		t.Fatalf("next beat = %v, want an interval after the sent one", got)
	}
}

func TestABeatWhoseRecordFailedIsStillTimedFromIt(t *testing.T) {
	// Arrange
	h := dueHarness(t, sent())
	h.db.keepaliveErr = errors.New("disk full")
	h.keeper.beats[theWorkspace] = now
	h.keeper.beatsDue(context.Background())

	// Act
	h.keeper.schedule(context.Background(), theWorkspace, "test")

	// Assert
	if got := h.keeper.beats[theWorkspace]; !got.Equal(now.Add(KeepaliveInterval)) {
		t.Fatalf("next beat = %v, want an interval after the sent one", got)
	}
}

func TestScheduleTimesTheBeatFromARecordedKeepalive(t *testing.T) {
	// Arrange
	h := newHarness(t, beatable(visible(), 40*time.Minute))
	rec, _ := h.db.Workspace(context.Background(), theWorkspace)
	beaten := now.Add(-5 * time.Minute)
	rec.LastKeepaliveAt = &beaten
	h.db.records[theWorkspace] = rec

	// Act
	h.keeper.schedule(context.Background(), theWorkspace, "test")

	// Assert
	if got := h.keeper.beats[theWorkspace]; !got.Equal(beaten.Add(KeepaliveInterval)) {
		t.Fatalf("beat = %v, want an interval after the recorded keep-alive", got)
	}
}

func TestABeatThatATurnMovedIsRearmedNotSent(t *testing.T) {
	// Arrange: the row's clock moved since the beat was armed.
	h := newHarness(t, beatable(visible(), 10*time.Minute))
	h.keeper.beats[theWorkspace] = now

	// Act
	h.keeper.beatsDue(context.Background())

	// Assert
	if h.shim.beats != 0 || !h.keeper.beats[theWorkspace].After(now) {
		t.Fatalf("beats = %d with the next at %v, want none sent and the beat re-armed", h.shim.beats, h.keeper.beats[theWorkspace])
	}
}

func TestABeatTheShimSkippedIsNotCycledDown(t *testing.T) {
	tests := []struct {
		name   string
		answer *shimv1.SendKeepaliveSuccess
	}{
		{name: "the main agent is busy", answer: skipped(&shimv1.SendKeepaliveSkipped{Reason: &shimv1.SendKeepaliveSkipped_MainAgentBusy{MainAgentBusy: &shimv1.SendKeepaliveSkippedMainAgentBusy{Detail: "a turn is in flight"}}})},
		{name: "detached work is live", answer: skipped(&shimv1.SendKeepaliveSkipped{Reason: &shimv1.SendKeepaliveSkipped_DetachedWorkLive{DetachedWorkLive: &shimv1.SendKeepaliveSkippedDetachedWorkLive{LiveItems: 1}}})},
		{name: "the cache lapsed", answer: skipped(&shimv1.SendKeepaliveSkipped{Reason: &shimv1.SendKeepaliveSkipped_CacheLapsed{CacheLapsed: &shimv1.SendKeepaliveSkippedCacheLapsed{LapsedAtMs: 1}}})},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := dueHarness(t, tt.answer)
			h.keeper.beats[theWorkspace] = now

			// Act
			h.keeper.beatsDue(context.Background())

			// Assert
			if len(h.cycled) != 0 {
				t.Fatalf("cycle stand-downs = %+v, want none", h.cycled)
			}
		})
	}
}

func TestABeatSkippedForDetachedWorkIsSentWhenTheSessionFallsFree(t *testing.T) {
	// Arrange
	h := dueHarness(t, skipped(&shimv1.SendKeepaliveSkipped{Reason: &shimv1.SendKeepaliveSkipped_DetachedWorkLive{DetachedWorkLive: &shimv1.SendKeepaliveSkippedDetachedWorkLive{LiveItems: 1}}}))
	h.keeper.beats[theWorkspace] = now
	h.keeper.beatsDue(context.Background())
	h.shim.answer = sent()

	// Act
	h.keeper.apply(context.Background(), edge{ws: theWorkspace, kind: edgeFree, why: "test"})
	h.keeper.beatsDue(context.Background())

	// Assert
	if h.shim.beats != 2 || len(h.cycled) != 1 {
		t.Fatalf("beats = %d with stand-downs %+v, want the beat sent once free", h.shim.beats, h.cycled)
	}
}

func TestALapsedCacheIsBeatenNoMore(t *testing.T) {
	// Arrange
	h := dueHarness(t, skipped(&shimv1.SendKeepaliveSkipped{Reason: &shimv1.SendKeepaliveSkipped_CacheLapsed{CacheLapsed: &shimv1.SendKeepaliveSkippedCacheLapsed{LapsedAtMs: 1}}}))
	h.keeper.beats[theWorkspace] = now
	h.keeper.beatsDue(context.Background())

	// Act
	h.keeper.schedule(context.Background(), theWorkspace, "test")

	// Assert
	if _, armed := h.keeper.beats[theWorkspace]; armed {
		t.Fatal("a lapsed cache's beat was armed again")
	}
}

func TestABeatThatCouldNotBeAskedIsRetried(t *testing.T) {
	tests := []struct {
		name  string
		err   error
		level string
	}{
		{name: "a failed submission", err: &BeatSubmitFailed{Detail: "boom"}, level: "error"},
		{name: "a shim that died under the call", err: fmt.Errorf("%w: unavailable: unexpected EOF", shimclient.ErrShimDied), level: "info"},
		{name: "a shim this daemon was standing down", err: fmt.Errorf("%w: unavailable: use of closed network connection", shimclient.ErrStandDownOrdered), level: "info"},
		{name: "a shim that cannot be reached", err: errors.New("connection refused"), level: "error"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := dueHarness(t, nil)
			h.shim.beatErr = tt.err
			h.keeper.beats[theWorkspace] = now

			// Act
			h.keeper.beatsDue(context.Background())

			// Assert
			if got := h.keeper.beats[theWorkspace]; !got.Equal(now.Add(BeatRetry)) {
				t.Fatalf("retry = %v, want %v", got, now.Add(BeatRetry))
			}
			if !h.found(dlog.RecordQuery{Level: tt.level, Operation: opDrive}) {
				t.Fatalf("records = %+v, want one at %s", h.log.Records(), tt.level)
			}
		})
	}
}

func TestABeatToAShimWithNoSessionWaitsForTheSession(t *testing.T) {
	// Arrange
	h := dueHarness(t, nil)
	h.shim.beatErr = ErrNoSession
	h.keeper.beats[theWorkspace] = now

	// Act
	h.keeper.beatsDue(context.Background())

	// Assert
	if _, armed := h.keeper.beats[theWorkspace]; armed || h.found(dlog.RecordQuery{Level: "error"}) {
		t.Fatalf("beat armed %v with records %+v, want neither a retry nor an error", armed, h.log.Records())
	}
}

func TestADueBeatOfAParkedSessionRestoresItFirst(t *testing.T) {
	// Arrange
	h := dueHarness(t, sent())
	h.db.sessions[theWorkspace] = parkedSession(theWorkspace)
	h.serving = false
	h.keeper.beats[theWorkspace] = now

	// Act
	h.keeper.beatsDue(context.Background())

	// Assert
	if len(h.restoring) != 1 || h.restoring[0].ws != theWorkspace || h.shim.beats != 0 {
		t.Fatalf("restores %+v with beats %d, want one restore and no beat yet", h.restoring, h.shim.beats)
	}
}

// parkedFleet registers N parked visible workspaces whose restores are due.
func parkedFleet(t *testing.T, n int) (*harness, []ids.WorkspaceID) {
	t.Helper()
	h := newHarness(t, beatable(visible(), 10*time.Minute))
	h.serving = false
	var fleet []ids.WorkspaceID
	for i := range n {
		ws := ids.WorkspaceID(fmt.Sprintf("ws-parked-%d", i))
		rec := beatable(wsm.Workspace{ID: ws}, KeepaliveInterval-time.Duration(i)*time.Second)
		h.db.records[ws] = rec
		h.db.sessions[ws] = parkedSession(ws)
		h.keeper.schedule(context.Background(), ws, "test")
		fleet = append(fleet, ws)
	}
	h.clock.Set(now.Add(time.Hour))
	return h, fleet
}

func TestNoMoreThanMaxRestoresRunAtOnce(t *testing.T) {
	// Arrange
	h, _ := parkedFleet(t, MaxRestores+2)

	// Act
	h.keeper.restoresDue(context.Background())

	// Assert
	if len(h.restoring) != MaxRestores || len(h.keeper.queued) != 2 {
		t.Fatalf("restoring %d with %d waiting, want %d and 2", len(h.restoring), len(h.keeper.queued), MaxRestores)
	}
}

func TestTheEarliestBeatIsRestoredFirst(t *testing.T) {
	// Arrange: the fleet's beats fall later as their index rises.
	h, fleet := parkedFleet(t, MaxRestores+1)

	// Act
	h.keeper.restoresDue(context.Background())

	// Assert
	if latest := fleet[len(fleet)-1]; h.keeper.queued[0].ws != latest {
		t.Fatalf("waiting = %+v, want the latest beat %s left waiting", h.keeper.queued, latest)
	}
}

func TestAFinishedRestoreFreesItsSlotForTheNext(t *testing.T) {
	// Arrange
	h, _ := parkedFleet(t, MaxRestores+1)
	h.keeper.restoresDue(context.Background())

	// Act
	h.keeper.apply(context.Background(), edge{ws: h.restoring[0].ws, kind: edgeRestored, why: "test"})

	// Assert
	if len(h.restoring) != MaxRestores+1 || len(h.keeper.queued) != 0 {
		t.Fatalf("restores asked %d with %d waiting, want every one started", len(h.restoring), len(h.keeper.queued))
	}
}

func TestAFailedRestoreIsAskedAgainShortly(t *testing.T) {
	// Arrange
	h, fleet := parkedFleet(t, 1)
	h.keeper.restoresDue(context.Background())

	// Act
	h.keeper.apply(context.Background(), edge{ws: fleet[0], kind: edgeRestored, err: errors.New("the shim would not start"), why: "test"})

	// Assert
	if got := h.keeper.restores[fleet[0]]; !got.Equal(h.clock.Now().Add(BeatRetry)) {
		t.Fatalf("restore = %v, want a retry in %s", got, BeatRetry)
	}
	if !h.found(dlog.RecordQuery{Level: "error", Operation: opRestore}) {
		t.Fatalf("records = %+v, want the failed restore at ERROR", h.log.Records())
	}
}

func TestARestoreOfASessionServedAgainIsDropped(t *testing.T) {
	// Arrange: a look revived it before its restore's turn.
	h, _ := parkedFleet(t, 1)
	h.serving = true

	// Act
	h.keeper.restoresDue(context.Background())

	// Assert
	if len(h.restoring) != 0 {
		t.Fatalf("restores = %+v, want none for a session already served", h.restoring)
	}
}

func TestRestoreParked(t *testing.T) {
	tests := []struct {
		name    string
		session wsm.Session
		want    int
	}{
		{name: "a parked session is restored", session: parkedSession(theWorkspace), want: 1},
		{name: "a live session is left alone", session: liveSession(theWorkspace), want: 0},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t, beatable(visible(), 10*time.Minute))
			h.db.sessions[theWorkspace] = tt.session
			h.serving = false

			// Act
			h.keeper.apply(context.Background(), edge{ws: theWorkspace, kind: edgeRestoreAsked, why: "test"})

			// Assert
			if len(h.restoring) != tt.want {
				t.Fatalf("restores = %+v, want %d", h.restoring, tt.want)
			}
		})
	}
}

func TestRunArmsEveryWorkspacesBeatAtTheStart(t *testing.T) {
	// Arrange
	h := newHarness(t, beatable(visible(), KeepaliveInterval+time.Minute))
	h.shim.answer = sent()
	h.shim.beaten = make(chan struct{}, 1)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Act
	go func() { _ = h.keeper.Run(ctx) }()

	// Assert: the due beat goes out with no edge at all.
	<-h.shim.beaten
}
