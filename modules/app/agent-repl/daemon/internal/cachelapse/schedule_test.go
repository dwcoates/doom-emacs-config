package cachelapse

import (
	"agent-repld/internal/sourcescan"
	"context"
	"errors"
	"fmt"
	"io/fs"
	"slices"
	"strings"
	"testing"
	"time"

	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/shimclient"
	"agent-repld/internal/wsm"
)

// parkedSession is a session stood down while idle: the one park.
func parkedSession(ws ids.WorkspaceID) wsm.Session {
	s := liveSession(ws)
	s.Terminal = &wsm.SessionTerminal{Kind: wsm.TerminalParked}
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
		{name: "when its cache is known cold and no turn has closed since", arrange: func(h *harness) {
			rec := h.db.records[theWorkspace]
			rec.CacheColdTurnAt = rec.LastTurnEndedAt
			h.db.records[theWorkspace] = rec
		}},
		{name: "when the session stands at its cold gate", arrange: func(h *harness) { h.gate = coldLapsed() }},
		{name: "when there is no session", arrange: func(h *harness) { delete(h.db.sessions, theWorkspace) }},
		{name: "when the session is dead", arrange: func(h *harness) {
			s := liveSession(theWorkspace)
			s.Terminal = &wsm.SessionTerminal{Kind: "killed"}
			h.db.sessions[theWorkspace] = s
		}},
		{name: "when the session is parked past the cutoff by the beat", arrange: func(h *harness) {
			s := parkedSession(theWorkspace)
			h.db.sessions[theWorkspace] = s
			h.workedAgo(6 * time.Hour)
		}},
		{name: "when it will be idle past the cutoff by the beat", arrange: func(h *harness) {
			s := liveSession(theWorkspace)
			h.db.sessions[theWorkspace] = s
			h.workedAgo(6 * time.Hour)
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

func TestScheduleBeatsAgainOnceATurnClosesAfterTheColdMark(t *testing.T) {
	// Arrange
	rec := beatable(visible(), 10*time.Minute)
	marked := now.Add(-time.Hour)
	rec.CacheColdTurnAt = &marked
	h := newHarness(t, rec)

	// Act
	h.keeper.schedule(context.Background(), theWorkspace, "test")

	// Assert
	if _, armed := h.keeper.beats[theWorkspace]; !armed {
		t.Fatal("no beat was armed after a turn closed past the cold mark")
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

// A SENT BEAT AWAITS ITS ANSWER before the cycle stand-down (owner request,
// 2026-10-29, superseding the stand-down at the send): answer_test.go.
func TestASentBeatAwaitsItsAnswerBeforeTheCycleStandDown(t *testing.T) {
	// Arrange
	h := dueHarness(t, sent())
	h.keeper.beats[theWorkspace] = now

	// Act
	h.keeper.beatsDue(context.Background())

	// Assert
	if sentAt, waiting := h.keeper.answering[theWorkspace]; len(h.cycled) != 0 || !waiting || !sentAt.Equal(now) {
		t.Fatalf("cycle stand-downs = %+v, awaiting %v (%t), want none yet and the answer to the beat awaited", h.cycled, sentAt, waiting)
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

func TestASentBeatSoonAfterTheLastWorkLeavesTheShimUp(t *testing.T) {
	// Arrange: detached work ended ten minutes ago, long after the turn.
	h := dueHarness(t, sent())
	h.workedAgo(10 * time.Minute)
	h.keeper.beats[theWorkspace] = now

	// Act
	h.keeper.beatsDue(context.Background())

	// Assert
	if len(h.cycled) != 0 {
		t.Fatalf("cycle stand-downs = %+v, want none within an interval of falling free", h.cycled)
	}
}

func TestASentBeatTheGraceAfterTheLastWorkAwaitsItsAnswerToStandItDown(t *testing.T) {
	// Arrange
	h := dueHarness(t, sent())
	h.workedAgo(StandDownGrace)
	h.keeper.beats[theWorkspace] = now

	// Act
	h.keeper.beatsDue(context.Background())

	// Assert
	if _, waiting := h.keeper.answering[theWorkspace]; !waiting {
		t.Fatalf("answers awaited = %v, want the beat's, the grace after falling free", h.keeper.answering)
	}
}

// A test run's compressed cadence (IntervalFromEnv) times the beats and
// nothing else: the stand-down keeps its production grace, so a run of
// compressed beats is a run on one live shim.
func TestACompressedIntervalTimesTheBeats(t *testing.T) {
	tests := []struct {
		name   string
		act    func(h *harness)
		assert func(t *testing.T, h *harness)
	}{
		{
			name: "the beat is armed the interval after the last turn",
			act:  func(h *harness) { h.keeper.schedule(context.Background(), theWorkspace, "test") },
			assert: func(t *testing.T, h *harness) {
				if got, want := h.keeper.beats[theWorkspace], now.Add(-10*time.Minute).Add(100*time.Millisecond); !got.Equal(want) {
					t.Fatalf("beat = %v, want %v", got, want)
				}
			},
		},
		{
			name: "a sent beat arms the next the interval on",
			act:  func(h *harness) { h.keeper.beats[theWorkspace] = now; h.keeper.beatsDue(context.Background()) },
			assert: func(t *testing.T, h *harness) {
				if got, want := h.keeper.beats[theWorkspace], now.Add(100*time.Millisecond); !got.Equal(want) {
					t.Fatalf("next beat = %v, want %v", got, want)
				}
			},
		},
		{
			name: "a sent beat many intervals after the session fell free leaves the shim up",
			act:  func(h *harness) { h.keeper.beats[theWorkspace] = now; h.keeper.beatsDue(context.Background()) },
			assert: func(t *testing.T, h *harness) {
				if len(h.cycled) != 0 {
					t.Fatalf("cycle stand-downs = %+v, want none within the grace", h.cycled)
				}
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t, beatable(visible(), 10*time.Minute))
			h.keeper.deps.Interval = 100 * time.Millisecond
			h.shim.answer = sent()
			h.workedAgo(time.Second)

			// Act
			tt.act(h)

			// Assert
			tt.assert(t, h)
		})
	}
}

// The shim's own keep-alive turn, and a vendor turn it never serves, hold the
// main agent where the daemon sees no turn, so no free edge follows them.
func TestABusyBeatIsArmedAnIntervalOn(t *testing.T) {
	// Arrange
	h := dueHarness(t, skipped(&shimv1.SendKeepaliveSkipped{Reason: &shimv1.SendKeepaliveSkipped_MainAgentBusy{MainAgentBusy: &shimv1.SendKeepaliveSkippedMainAgentBusy{Detail: "a turn is open"}}}))
	h.keeper.beats[theWorkspace] = now

	// Act
	h.keeper.beatsDue(context.Background())

	// Assert
	if got, want := h.keeper.beats[theWorkspace], now.Add(KeepaliveInterval); !got.Equal(want) {
		t.Fatalf("next beat = %v, want %v", got, want)
	}
}

func TestTheFreeEdgeAfterWorkRecordsTheWorkOnTheRow(t *testing.T) {
	// Arrange
	h := newHarness(t, beatable(visible(), 10*time.Minute))

	// Act
	h.keeper.apply(context.Background(), edge{ws: theWorkspace, kind: edgeFree, workEnded: true, why: "test"})

	// Assert
	if got := h.db.records[theWorkspace].LastWorkAt; got == nil || !got.Equal(now) {
		t.Fatalf("last work = %v, want %v", got, now)
	}
	if len(h.db.works) != 1 || h.db.works[0] != wsm.WorkEnded {
		t.Fatalf("work writes = %v, want one of work ended", h.db.works)
	}
}

func TestAWorkEndThatCannotBeRecordedIsAnError(t *testing.T) {
	// Arrange
	h := newHarness(t, beatable(visible(), 10*time.Minute))
	h.db.workErr = errors.New("store unreachable")
	before := h.db.records[theWorkspace].LastWorkAt

	// Act
	h.keeper.apply(context.Background(), edge{ws: theWorkspace, kind: edgeFree, workEnded: true, why: "test"})

	// Assert
	if !h.found(dlog.RecordQuery{Level: "error", Operation: opOwner, Message: "could not record that the workspace's work ended; its park is measured from its earlier work"}) {
		t.Fatal("the lost work end was not recorded at error")
	}
	if got := h.db.records[theWorkspace].LastWorkAt; got == nil || !got.Equal(*before) {
		t.Fatalf("last work = %v, want the earlier %v", got, before)
	}
}

// TestAnAttachComingUpFreeStampsNothing pins the 2026-10-26 handover: the
// adoption's free edge restarted every workspace's hour of grace, so no beat
// in the next hour parked a shim.
func TestAnAttachComingUpFreeStampsNothing(t *testing.T) {
	// Arrange
	h := newHarness(t, beatable(visible(), 10*time.Minute))

	// Act
	h.keeper.apply(context.Background(), edge{ws: theWorkspace, kind: edgeFree, why: "test"})

	// Assert
	if len(h.db.works) != 0 {
		t.Fatalf("work writes = %v, want nothing recorded for an attach", h.db.works)
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

// The work just ended, so the beat goes out and leaves the shim up: a session
// is stood down only an interval after it fell free (owner ruling,
// 2026-10-26).
func TestABeatSkippedForDetachedWorkIsSentWhenTheSessionFallsFree(t *testing.T) {
	// Arrange
	h := dueHarness(t, skipped(&shimv1.SendKeepaliveSkipped{Reason: &shimv1.SendKeepaliveSkipped_DetachedWorkLive{DetachedWorkLive: &shimv1.SendKeepaliveSkippedDetachedWorkLive{LiveItems: 1}}}))
	h.keeper.beats[theWorkspace] = now
	h.keeper.beatsDue(context.Background())
	h.shim.answer = sent()

	// Act
	h.keeper.apply(context.Background(), edge{ws: theWorkspace, kind: edgeFree, workEnded: true, why: "test"})
	h.keeper.beatsDue(context.Background())

	// Assert
	if h.shim.beats != 2 || len(h.cycled) != 0 {
		t.Fatalf("beats = %d with stand-downs %+v, want the beat sent once free and the shim left up", h.shim.beats, h.cycled)
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

// errHandedOver is the error the harness's HandedOver classifier names a
// handover by.
var errHandedOver = errors.New("workspace is served by the successor daemon")

func TestARestoreRefusedByAHandoverIsAskedAgainAtInfo(t *testing.T) {
	cases := []struct {
		name   string
		assert func(t *testing.T, h *harness, ws ids.WorkspaceID)
	}{
		{name: "it is asked again shortly", assert: func(t *testing.T, h *harness, ws ids.WorkspaceID) {
			if got := h.keeper.restores[ws]; !got.Equal(h.clock.Now().Add(BeatRetry)) {
				t.Fatalf("restore = %v, want a retry in %s", got, BeatRetry)
			}
		}},
		{name: "its keep-alive stays armed", assert: func(t *testing.T, h *harness, ws ids.WorkspaceID) {
			if _, armed := h.keeper.beats[ws]; !armed {
				t.Fatal("beat retired, want it kept for a withdrawn handover")
			}
		}},
		{name: "it is recorded at INFO with its cause", assert: func(t *testing.T, h *harness, _ ids.WorkspaceID) {
			r, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: "info", Operation: opRestore,
				Message: "the parked session is served by another daemon now, so this one does not restore it; it is asked again shortly"})
			if !ok {
				t.Fatalf("records = %+v, want the handover at INFO", h.log.Records())
			}
			if r.Context["cause"] != "start: "+errHandedOver.Error() || r.Context["retry_in"] != BeatRetry.String() {
				t.Fatalf("context = %+v, want the cause and the retry", r.Context)
			}
		}},
		{name: "it writes no error", assert: func(t *testing.T, h *harness, _ ids.WorkspaceID) {
			if h.found(dlog.RecordQuery{Level: "error", Operation: opRestore}) {
				t.Fatalf("records = %+v, want no ERROR", h.log.Records())
			}
		}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h, fleet := parkedFleet(t, 1)
			h.keeper.restoresDue(context.Background())

			// Act
			h.keeper.apply(context.Background(), edge{ws: fleet[0], kind: edgeRestored, err: fmt.Errorf("start: %w", errHandedOver), why: "test"})

			// Assert
			tc.assert(t, h, fleet[0])
		})
	}
}

func TestARestoreWhoseWorktreeIsGoneRetiresTheKeepalive(t *testing.T) {
	cases := []struct {
		name   string
		assert func(t *testing.T, h *harness, ws ids.WorkspaceID)
	}{
		{name: "it is not asked again", assert: func(t *testing.T, h *harness, ws ids.WorkspaceID) {
			if _, again := h.keeper.restores[ws]; again {
				t.Fatalf("restore re-armed at %v, want none", h.keeper.restores[ws])
			}
		}},
		{name: "its keep-alive is retired", assert: func(t *testing.T, h *harness, ws ids.WorkspaceID) {
			if _, armed := h.keeper.beats[ws]; armed {
				t.Fatalf("beat still armed at %v, want it retired", h.keeper.beats[ws])
			}
		}},
		{name: "it is recorded at INFO, never as an error", assert: func(t *testing.T, h *harness, _ ids.WorkspaceID) {
			if !h.found(dlog.RecordQuery{Level: "info", Operation: opRestore,
				Message: "the workspace's directory is gone, so its parked session can never be restored; its keep-alive is retired"}) {
				t.Fatalf("records = %+v, want the retirement at INFO", h.log.Records())
			}
			if h.found(dlog.RecordQuery{Level: "error", Operation: opRestore}) {
				t.Fatalf("records = %+v, want no ERROR", h.log.Records())
			}
		}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h, fleet := parkedFleet(t, 1)
			h.keeper.restoresDue(context.Background())
			gone := fmt.Errorf("start session for %q: resolve log sink: %w", fleet[0], dlog.ErrWorkspaceDirGone)

			// Act
			h.keeper.apply(context.Background(), edge{ws: fleet[0], kind: edgeRestored, err: gone, why: "test"})

			// Assert
			tc.assert(t, h, fleet[0])
		})
	}
}

func TestAParkedSessionWhoseDirectoryIsGoneIsNeverArmed(t *testing.T) {
	const dir = "/worktrees/landed"
	cases := []struct {
		name    string
		statErr error
		assert  func(t *testing.T, h *harness, ws ids.WorkspaceID)
	}{
		{name: "no restore is armed for a gone directory", statErr: fs.ErrNotExist, assert: func(t *testing.T, h *harness, ws ids.WorkspaceID) {
			if at, armed := h.keeper.restores[ws]; armed {
				t.Fatalf("restore armed at %v, want none", at)
			}
		}},
		{name: "no beat is armed for a gone directory", statErr: fs.ErrNotExist, assert: func(t *testing.T, h *harness, ws ids.WorkspaceID) {
			if at, armed := h.keeper.beats[ws]; armed {
				t.Fatalf("beat armed at %v, want none", at)
			}
		}},
		{name: "a gone directory writes no error", statErr: fs.ErrNotExist, assert: func(t *testing.T, h *harness, _ ids.WorkspaceID) {
			if h.found(dlog.RecordQuery{Level: "error"}) {
				t.Fatalf("records = %+v, want no ERROR", h.log.Records())
			}
		}},
		{name: "a stat that cannot tell arms nothing", statErr: fs.ErrPermission, assert: func(t *testing.T, h *harness, ws ids.WorkspaceID) {
			if _, armed := h.keeper.beats[ws]; armed {
				t.Fatalf("beat armed at %v, want none", h.keeper.beats[ws])
			}
		}},
		{name: "a stat that cannot tell is said at ERROR", statErr: fs.ErrPermission, assert: func(t *testing.T, h *harness, _ ids.WorkspaceID) {
			if !h.found(dlog.RecordQuery{Level: "error", Operation: opDrive,
				Message: "could not tell whether the workspace's directory exists; its keep-alive is not armed"}) {
				t.Fatalf("records = %+v, want the ERROR", h.log.Records())
			}
		}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: a parked session owing its beat, whose directory stats as the case says.
			h := newHarness(t, beatable(visible(), 10*time.Minute))
			h.serving = false
			ws := ids.WorkspaceID("ws-landed")
			rec := beatable(wsm.Workspace{ID: ws, Dir: dir}, KeepaliveInterval)
			h.db.records[ws] = rec
			h.db.sessions[ws] = parkedSession(ws)
			h.statErr = map[string]error{dir: tc.statErr}

			// Act
			h.keeper.schedule(context.Background(), ws, "the daemon started")

			// Assert
			tc.assert(t, h, ws)
		})
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

func TestScheduleBeatsAWorkspaceWithAMergeQueued(t *testing.T) {
	spentClose := closed()
	spentClose.Closure = &wsm.Closure{At: now.Add(-time.Hour)}
	tests := []struct {
		name string
		rec  wsm.Workspace
	}{
		{name: "hidden, nobody looking at it", rec: hidden()},
		{name: "closed, its last keep-alive spent", rec: spentClose},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: its merge waits in a queue.
			h := newHarness(t, beatable(tt.rec, 10*time.Minute))
			h.db.queued = []ids.WorkspaceID{theWorkspace}

			// Act
			h.keeper.schedule(context.Background(), theWorkspace, "test")

			// Assert
			if got, want := h.keeper.beats[theWorkspace], now.Add(-10*time.Minute).Add(KeepaliveInterval); !got.Equal(want) {
				t.Fatalf("beat = %v, want %v: a queued workspace's cache is kept warm", got, want)
			}
		})
	}
}

// A SYNC MERGE'S PARK KEEPS ITS BEATS: the drain parks a finished workspace
// the moment its merge is in line (drain.Controller.MergeQueuedDown), and the
// Keeper restores it before each beat its queued entry is still owed.
func TestScheduleArmsTheRestoreOfAParkedWorkspaceWithAMergeQueued(t *testing.T) {
	spentClose := closed()
	spentClose.Closure = &wsm.Closure{At: now.Add(-time.Hour)}
	tests := []struct {
		name string
		rec  wsm.Workspace
	}{
		{name: "the workspace in view", rec: visible()},
		{name: "hidden, nobody looking at it", rec: hidden()},
		{name: "closed, its last keep-alive spent", rec: spentClose},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: parked while its merge waits in a queue.
			h := newHarness(t, beatable(tt.rec, 10*time.Minute))
			h.db.queued = []ids.WorkspaceID{theWorkspace}
			h.db.sessions[theWorkspace] = parkedSession(theWorkspace)

			// Act
			h.keeper.schedule(context.Background(), theWorkspace, "test")

			// Assert
			beat, armed := h.keeper.beats[theWorkspace]
			if want := now.Add(-10 * time.Minute).Add(KeepaliveInterval); !armed || !beat.Equal(want) {
				t.Fatalf("beat = %v (armed %v), want %v", beat, armed, want)
			}
			if got, want := h.keeper.restores[theWorkspace], beat.Add(-RestoreLead); !got.Equal(want) {
				t.Fatalf("restore = %v, want %v", got, want)
			}
		})
	}
}

func TestScheduleHoldsAHiddenWorkspaceOnceItsMergeLeftTheQueue(t *testing.T) {
	// Arrange: no entry of it stands in any queue.
	h := newHarness(t, beatable(hidden(), 10*time.Minute))

	// Act
	h.keeper.schedule(context.Background(), theWorkspace, "test")

	// Assert
	if _, armed := h.keeper.beats[theWorkspace]; armed {
		t.Fatal("a beat was armed for a hidden workspace with no merge queued")
	}
}

func TestScheduleLogsQueuesThatCannotBeRead(t *testing.T) {
	// Arrange
	h := newHarness(t, beatable(hidden(), 10*time.Minute))
	h.db.queuesErr = errors.New("database is locked")

	// Act
	h.keeper.schedule(context.Background(), theWorkspace, "test")

	// Assert
	rec, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: "error", Operation: opDrive})
	if cause, _ := rec.Context["cause"].(string); !ok || !strings.Contains(cause, "database is locked") {
		t.Fatalf("error record = %+v, %v; want the queues' read failure", rec, ok)
	}
	if _, armed := h.keeper.beats[theWorkspace]; armed {
		t.Fatal("a beat was armed though the queues could not be read")
	}
}

func TestScheduleBeatsAQueuedSessionIdlePastTheCutoff(t *testing.T) {
	// Arrange: idle six hours, past the cutoff, with its merge in line; the
	// idle sweep keeps a queued session warm, so its beats go on.
	h := newHarness(t, beatable(visible(), 10*time.Minute))
	s := liveSession(theWorkspace)
	h.db.sessions[theWorkspace] = s
	h.workedAgo(6 * time.Hour)
	h.db.queued = []ids.WorkspaceID{theWorkspace}

	// Act
	h.keeper.schedule(context.Background(), theWorkspace, "test")

	// Assert
	if _, armed := h.keeper.beats[theWorkspace]; !armed {
		t.Fatal("no beat was armed for a queued session idle past the cutoff")
	}
}

func TestScheduleOwesNoBeatToAQueuedSessionIdlePastTwiceTheCutoff(t *testing.T) {
	// Arrange: idle twelve hours, the two six-hour cutoffs a queued merge's
	// session is kept warm for, so the sweep parks it before the beat.
	h := newHarness(t, beatable(visible(), 10*time.Minute))
	s := liveSession(theWorkspace)
	h.db.sessions[theWorkspace] = s
	h.workedAgo(12 * time.Hour)
	h.db.queued = []ids.WorkspaceID{theWorkspace}

	// Act
	h.keeper.schedule(context.Background(), theWorkspace, "test")

	// Assert
	if _, armed := h.keeper.beats[theWorkspace]; armed {
		t.Fatal("a beat was armed for a queued session idle past twice the cutoff")
	}
}

func TestScheduleBeatsASessionIdleTwiceTheCutoffWhoseMergeJoinedItsQueueSince(t *testing.T) {
	// Arrange: idle twelve hours, its merge in line for one: the two cutoffs
	// count from joining the queue.
	h := newHarness(t, beatable(visible(), 10*time.Minute))
	s := liveSession(theWorkspace)
	h.db.sessions[theWorkspace] = s
	h.workedAgo(12 * time.Hour)
	h.db.queued = []ids.WorkspaceID{theWorkspace}
	h.db.queuedAt = now.Add(-time.Hour)

	// Act
	h.keeper.schedule(context.Background(), theWorkspace, "test")

	// Assert
	if _, armed := h.keeper.beats[theWorkspace]; !armed {
		t.Fatal("no beat was armed for a session whose merge joined its queue inside two cutoffs")
	}
}

func TestScheduleTimesTheBeatFromTheConfiguredInterval(t *testing.T) {
	// Arrange
	h := newHarness(t, beatable(visible(), 0))
	h.keeper.deps.Interval = 100 * time.Millisecond

	// Act
	h.keeper.schedule(context.Background(), theWorkspace, "test")

	// Assert
	if got, want := h.keeper.beats[theWorkspace], now.Add(100*time.Millisecond); !got.Equal(want) {
		t.Fatalf("beat = %v, want %v", got, want)
	}
}

// owesNoneRecords are the keeper's records of a workspace owing no beat, or
// owing beats again, at LEVEL.
func (h *harness) owesNoneRecords(level, message string) []dlog.Record {
	return dlog.FilterRecords(h.log.Records(), dlog.RecordQuery{Level: level, Operation: opDrive, Message: message})
}

const (
	owesNoneMessage  = "the workspace owes no keep-alive"
	owesAgainMessage = "the workspace owes keep-alives again"
	noSessionWhy     = "it has no session"
	coldGateWhy      = "its session stands at the cold gate"
)

func TestTheOwesNoKeepaliveDecisionIsRecordedAtInfoWhenItChanges(t *testing.T) {
	tests := []struct {
		name      string
		steps     []func(h *harness)
		wantInfo  []string
		wantDebug int
	}{
		{name: "a first decision is recorded at info", steps: []func(h *harness){
			func(h *harness) { delete(h.db.sessions, theWorkspace) },
		}, wantInfo: []string{noSessionWhy}},
		{name: "the same decision again is recorded at debug only", steps: []func(h *harness){
			func(h *harness) { delete(h.db.sessions, theWorkspace) },
			func(h *harness) {},
		}, wantInfo: []string{noSessionWhy}, wantDebug: 1},
		{name: "a changed reason is recorded at info again", steps: []func(h *harness){
			func(h *harness) { h.gate = coldLapsed() },
			func(h *harness) { h.gate = nil; delete(h.db.sessions, theWorkspace) },
		}, wantInfo: []string{coldGateWhy, noSessionWhy}},
		{name: "the same reason after beats were owed again is recorded at info", steps: []func(h *harness){
			func(h *harness) { h.gate = coldLapsed() },
			func(h *harness) { h.gate = nil },
			func(h *harness) { h.gate = coldLapsed() },
		}, wantInfo: []string{coldGateWhy, coldGateWhy}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t, beatable(visible(), 10*time.Minute))

			// Act
			for _, step := range tt.steps {
				step(h)
				h.keeper.schedule(context.Background(), theWorkspace, "test")
			}

			// Assert
			var got []string
			for _, rec := range h.owesNoneRecords("info", owesNoneMessage) {
				if rec.Context["workspace"] != string(theWorkspace) {
					t.Fatalf("record context = %v, want the workspace", rec.Context)
				}
				got = append(got, fmt.Sprint(rec.Context["why"]))
			}
			if !slices.Equal(got, tt.wantInfo) {
				t.Fatalf("info decisions = %q, want %q", got, tt.wantInfo)
			}
			if debug := len(h.owesNoneRecords("debug", owesNoneMessage)); debug != tt.wantDebug {
				t.Fatalf("debug decisions = %d, want %d", debug, tt.wantDebug)
			}
		})
	}
}

func TestOwingKeepalivesAgainIsRecordedAtInfo(t *testing.T) {
	// Arrange
	h := newHarness(t, beatable(visible(), 10*time.Minute))
	h.gate = coldLapsed()
	h.keeper.schedule(context.Background(), theWorkspace, "test")
	h.gate = nil

	// Act
	h.keeper.schedule(context.Background(), theWorkspace, "test")

	// Assert
	again := h.owesNoneRecords("info", owesAgainMessage)
	if len(again) != 1 || again[0].Context["owed_none_because"] != coldGateWhy {
		t.Fatalf("records = %+v, want one naming the decision it ended", again)
	}
}

func TestAWorkspaceThatOwedBeatsAllAlongRecordsNoChange(t *testing.T) {
	// Arrange
	h := newHarness(t, beatable(visible(), 10*time.Minute))

	// Act
	h.keeper.schedule(context.Background(), theWorkspace, "test")

	// Assert
	if again := h.owesNoneRecords("info", owesAgainMessage); len(again) != 0 {
		t.Fatalf("records = %+v, want none", again)
	}
}

func TestAnUnregisteredWorkspaceForgetsItsDecision(t *testing.T) {
	// Arrange
	h := newHarness(t, beatable(visible(), 10*time.Minute))
	delete(h.db.sessions, theWorkspace)
	h.keeper.schedule(context.Background(), theWorkspace, "test")
	delete(h.db.records, theWorkspace)

	// Act
	h.keeper.schedule(context.Background(), theWorkspace, "test")

	// Assert
	if _, kept := h.keeper.noBeat[theWorkspace]; kept {
		t.Fatal("an unregistered workspace's decision was kept")
	}
}

// TestTheKeepaliveScheduleNeverReadsTheFolds pins the ruling structurally: the
// beat plan judges a workspace by wsm.Workspace.KeptWarm alone, and reads no
// section's fold, so collapsing can never withhold a keep-alive.
func TestTheKeepaliveScheduleNeverReadsTheFolds(t *testing.T) {
	// Act
	folds := sourcescan.Sites(t, "bringup.OnlyVisible(")
	warm := sourcescan.Sites(t, ".KeptWarm(")

	// Assert
	if len(folds) != 0 || !slices.Equal(warm, []string{"schedule.go"}) {
		t.Fatalf("OnlyVisible read in %v, KeptWarm read in %v; want the folds read nowhere and KeptWarm in schedule.go", folds, warm)
	}
}
