package drain

import (
	"context"
	"errors"
	"fmt"
	"strings"
	"testing"
	"time"

	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/sessionwhy"
	"agent-repld/internal/shimlife"
	"agent-repld/internal/wsm"
)

// hidden registers one workspace with a live, free session, and hides it.
func (h *harness) backlogged(t *testing.T) ids.WorkspaceID {
	t.Helper()
	return h.hide(t, h.workspace(t, instant))
}

// hide hides ws, as the user hiding it does.
func (h *harness) hide(t *testing.T, ws ids.WorkspaceID) ids.WorkspaceID {
	t.Helper()
	if err := h.db.SetBacklogged(context.Background(), ws, true, time.Unix(1_700_000_000, 0)); err != nil {
		t.Fatalf("SetBacklogged: %v", err)
	}
	return ws
}

// terminal reads ws's session terminal, nil when it has none.
func (h *harness) terminal(t *testing.T, ws ids.WorkspaceID) *wsm.SessionTerminal {
	t.Helper()
	session, _, err := h.db.Session(context.Background(), ws)
	if err != nil {
		t.Fatalf("Session: %v", err)
	}
	return session.Terminal
}

// pending reports whether a stand-down of ws is still pending.
func (h *harness) pending(ws ids.WorkspaceID) bool {
	h.c.mu.Lock()
	defer h.c.mu.Unlock()
	_, ok := h.c.withheld[ws]
	return ok
}

func TestNewRefusesWithoutADetachedLifetime(t *testing.T) {
	// Arrange
	h := newHarness(t)
	deps := h.c.deps
	deps.Detach = nil

	// Act
	_, err := New(deps)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "detached") {
		t.Fatalf("New = %v, want the missing detached lifetime refused", err)
	}
}

func TestABackloggedFreeWorkspaceIsStoodDownAsTheSweepDoes(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.backlogged(t)

	// Act
	h.c.StandDownWithheld(ws, WithheldCacheLapse)

	// Assert
	killed := h.stand.Killed()
	if len(killed) != 1 || killed[0].WS != ws || killed[0].Force {
		t.Fatalf("kills = %+v, want one graceful stand-down", killed)
	}
	if term := h.terminal(t, ws); term == nil || term.Kind != wsm.TerminalParked {
		t.Fatalf("terminal = %+v, want hibernated", term)
	}
}

func TestAWithheldStandDownRecordsItsOwnReason(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.backlogged(t)

	// Act
	h.c.StandDownWithheld(ws, WithheldCacheLapse)

	// Assert
	if term := h.terminal(t, ws); term == nil || !strings.Contains(term.Detail, "hidden, collapsed or closed") {
		t.Fatalf("terminal = %+v, want the withheld reason", term)
	}
}

func TestAWithheldStandDownClearsTheShimPids(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.backlogged(t)

	// Act
	h.c.StandDownWithheld(ws, WithheldCacheLapse)

	// Assert
	session, _, err := h.db.Session(context.Background(), ws)
	if err != nil {
		t.Fatalf("Session: %v", err)
	}
	if session.ShimPID != nil {
		t.Fatalf("shim pid = %v, want cleared", *session.ShimPID)
	}
}

func TestACollapsedWorkspaceIsStoodDown(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.collapsed(t)

	// Act
	h.c.StandDownWithheld(ws, WithheldCacheLapse)

	// Assert
	if killed := h.stand.Killed(); len(killed) != 1 {
		t.Fatalf("kills = %+v, want the collapsed workspace stood down", killed)
	}
}

func TestAClosedWorkspaceIsStoodDown(t *testing.T) {
	// Arrange: a closed workspace is drawn in no tab, whatever its visibility.
	h := newHarness(t)
	ws := h.workspace(t, instant)
	if err := h.db.MarkClosed(context.Background(), ws, wsm.Closure{At: instant, By: wsm.CloseByUser}); err != nil {
		t.Fatalf("MarkClosed: %v", err)
	}

	// Act
	h.c.StandDownWithheld(ws, WithheldCacheLapse)

	// Assert
	if killed := h.stand.Killed(); len(killed) != 1 || killed[0].WS != ws || killed[0].Force {
		t.Fatalf("kills = %+v, want the closed workspace stood down gracefully", killed)
	}
}

// TestTheSelectedBackloggedWorkspaceIsNotStoodDown pins the cold gate that stood
// again after every answer (2026-10-17): a hidden workspace the user selected
// and paid to resume was stood down the moment its session came up, because
// its cache had lapsed long before. The one they are looking at runs.
func TestTheSelectedBackloggedWorkspaceIsNotStoodDown(t *testing.T) {
	tests := []struct {
		name     string
		withhold func(t *testing.T, h *harness) ids.WorkspaceID
	}{
		{name: "hidden", withhold: func(t *testing.T, h *harness) ids.WorkspaceID { return h.backlogged(t) }},
		{name: "collapsed", withhold: func(t *testing.T, h *harness) ids.WorkspaceID { return h.collapsed(t) }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			ws := tt.withhold(t, h)
			if err := h.db.SetCurrent(context.Background(), ws, time.Unix(1_700_000_000, 0)); err != nil {
				t.Fatalf("SetCurrent: %v", err)
			}

			// Act
			h.c.StandDownWithheld(ws, WithheldCacheLapse)

			// Assert
			if killed := h.stand.Killed(); len(killed) != 0 {
				t.Fatalf("kills = %+v, want none for the selected workspace", killed)
			}
			if h.pending(ws) {
				t.Fatal("a dropped stand-down stayed pending")
			}
		})
	}
}

func TestAWorkspaceShownAgainIsNotStoodDown(t *testing.T) {
	// Arrange: asked while hidden, shown again before the attempt reads it.
	h := newHarness(t)
	ws := h.workspace(t, instant)

	// Act
	h.c.StandDownWithheld(ws, WithheldCacheLapse)

	// Assert
	if killed := h.stand.Killed(); len(killed) != 0 {
		t.Fatalf("kills = %+v, want none for a visible workspace", killed)
	}
	if h.pending(ws) {
		t.Fatal("a visible workspace's stand-down stayed pending")
	}
}

func TestABusyWorkspaceIsNotStoodDownUnderItsTurn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.backlogged(t)
	h.freeness.SetFree(ws, false)

	// Act
	h.c.StandDownWithheld(ws, WithheldCacheLapse)

	// Assert
	if killed := h.stand.Killed(); len(killed) != 0 {
		t.Fatalf("kills = %+v, want none under a turn", killed)
	}
	if !h.pending(ws) {
		t.Fatal("the deferred stand-down was not kept pending")
	}
}

func TestADeferredStandDownIsTakenOnTheTurnEndEdge(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.backlogged(t)
	h.freeness.SetFree(ws, false)
	h.c.StandDownWithheld(ws, WithheldCacheLapse)
	h.freeness.SetFree(ws, true)

	// Act
	h.c.RetryWithheld(ws)

	// Assert
	if killed := h.stand.Killed(); len(killed) != 1 {
		t.Fatalf("kills = %+v, want the stand-down taken on the edge", killed)
	}
	if h.pending(ws) {
		t.Fatal("a stand-down taken stayed pending")
	}
}

func TestADeferredStandDownOfAWorkspaceShownAgainIsDropped(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.backlogged(t)
	h.freeness.SetFree(ws, false)
	h.c.StandDownWithheld(ws, WithheldCacheLapse)
	if err := h.db.SetBacklogged(context.Background(), ws, false, time.Unix(1_700_000_000, 0)); err != nil {
		t.Fatalf("SetBacklogged: %v", err)
	}
	h.freeness.SetFree(ws, true)

	// Act
	h.c.RetryWithheld(ws)

	// Assert
	if killed := h.stand.Killed(); len(killed) != 0 {
		t.Fatalf("kills = %+v, want none for a workspace shown again", killed)
	}
	if h.pending(ws) {
		t.Fatal("a dropped stand-down stayed pending")
	}
}

func TestAnEdgeForAWorkspaceWithNothingPendingStandsNothingDown(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.backlogged(t)

	// Act
	h.c.RetryWithheld(ws)

	// Assert
	if killed := h.stand.Killed(); len(killed) != 0 {
		t.Fatalf("kills = %+v, want none without a request", killed)
	}
}

func TestATurnInFlightRefusalKeepsTheStandDownPending(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.backlogged(t)
	h.stand.answer[ws] = &shimv1.HibernateResponse{Result: &shimv1.HibernateResponse_Error{
		Error: &shimv1.HibernateError{Kind: &shimv1.HibernateError_TurnInFlight{TurnInFlight: &shimv1.HibernateTurnInFlight{}}}}}

	// Act
	h.c.StandDownWithheld(ws, WithheldCacheLapse)

	// Assert
	if !h.pending(ws) {
		t.Fatal("a stand-down the shim deferred was not kept pending")
	}
}

func TestAStartUnderWayKeepsTheStandDownPending(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.backlogged(t)
	h.stand.startUnderWay(ws)

	// Act
	h.c.StandDownWithheld(ws, WithheldCacheLapse)

	// Assert
	if !h.pending(ws) || len(h.stand.Killed()) != 0 {
		t.Fatal("a session still starting was stood down or forgotten")
	}
}

func TestARevivalKeepsTheStandDownPending(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.backlogged(t)
	h.reviving.Set(ws, true)

	// Act
	h.c.StandDownWithheld(ws, WithheldCacheLapse)

	// Assert
	if !h.pending(ws) || len(h.stand.Killed()) != 0 {
		t.Fatal("a session a prompt is reviving was stood down or forgotten")
	}
}

func TestAWithheldWorkspaceWithNoSessionHasNothingToStandDown(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.bare(t)
	if err := h.db.SetBacklogged(context.Background(), ws, true, time.Unix(1_700_000_000, 0)); err != nil {
		t.Fatalf("SetBacklogged: %v", err)
	}

	// Act
	h.c.StandDownWithheld(ws, WithheldCacheLapse)

	// Assert
	if h.pending(ws) || len(h.stand.Killed()) != 0 {
		t.Fatal("a workspace with no session was stood down or kept pending")
	}
}

func TestAWithheldWorkspaceWithNoShimHasNothingToStandDown(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.backlogged(t)
	h.stand.standDown(ws)

	// Act
	h.c.StandDownWithheld(ws, WithheldCacheLapse)

	// Assert
	if h.pending(ws) || len(h.stand.Killed()) != 0 {
		t.Fatal("a workspace with no shim was stood down or kept pending")
	}
}

func TestAFailedWithheldStandDownIsRecordedAndDropped(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.backlogged(t)
	h.stand.failKill(ws, errFake)

	// Act
	h.c.StandDownWithheld(ws, WithheldCacheLapse)

	// Assert
	errs := 0
	for _, r := range records(h.log, opWithheld) {
		if r.Level == dlog.LevelError {
			errs++
		}
	}
	if errs != 1 || h.pending(ws) {
		t.Fatalf("withheld error records = %d, pending = %v; want one ERROR and nothing pending", errs, h.pending(ws))
	}
}

func TestADeferredStandDownIsStatedAtInfo(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.backlogged(t)
	h.freeness.SetFree(ws, false)

	// Act
	h.c.StandDownWithheld(ws, WithheldCacheLapse)

	// Assert
	for _, r := range records(h.log, opWithheld) {
		if r.Level == dlog.LevelInfo && strings.Contains(r.Message, "waits for the turn to end") {
			return
		}
	}
	t.Fatalf("records = %+v, want an INFO saying the stand-down waits", records(h.log, opWithheld))
}

func TestAnEdgeDuringARunningAttemptRunsOneMore(t *testing.T) {
	// Arrange: the attempt is parked until the test runs it.
	var parked []func(context.Context)
	h := newHarness(t, func(d *Deps) {
		d.Detach = func(_ ids.WorkspaceID, run func(context.Context)) { parked = append(parked, run) }
	})
	ws := h.backlogged(t)
	h.freeness.SetFree(ws, false)
	h.c.StandDownWithheld(ws, WithheldCacheLapse)
	h.freeness.SetFree(ws, true)

	// Act: the edge arrives while the first attempt has not run yet.
	h.c.RetryWithheld(ws)
	if len(parked) != 1 {
		t.Fatalf("attempts parked = %d, want the edge folded into the running one", len(parked))
	}
	parked[0](context.Background())

	// Assert
	if killed := h.stand.Killed(); len(killed) != 1 {
		t.Fatalf("kills = %+v, want the stand-down taken", killed)
	}
}

// collapsed registers one workspace with a live, free session, and folds its
// repository's section.
func (h *harness) collapsed(t *testing.T) ids.WorkspaceID {
	t.Helper()
	return h.fold(t, h.workspace(t, instant))
}

// fold collapses ws's repository section.
func (h *harness) fold(t *testing.T, ws ids.WorkspaceID) ids.WorkspaceID {
	t.Helper()
	record, err := h.db.Workspace(context.Background(), ws)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	if err := h.db.SetRepositoryFolded(context.Background(), record.Repo, true); err != nil {
		t.Fatalf("SetRepositoryFolded: %v", err)
	}
	return ws
}

// oneShot marks ws a one-shot, as a create with -one-shot records it.
func (h *harness) oneShot(t *testing.T, ws ids.WorkspaceID) {
	t.Helper()
	if err := h.db.PutCreationJob(context.Background(), wsm.CreationJob{Workspace: ws, OneShot: true, CreatedAt: instant}); err != nil {
		t.Fatalf("PutCreationJob: %v", err)
	}
}

// mergePending records a merge request for ws in its own repository's queue.
func (h *harness) mergePending(t *testing.T, ws ids.WorkspaceID) {
	t.Helper()
	record, err := h.db.Workspace(context.Background(), ws)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	if err := h.db.RequestMerge(context.Background(), wsm.RepoKey(record.Dir+"/.git"), ws, wsm.MergeSource{Kind: wsm.MergeSourceOwnBranch}, instant); err != nil {
		t.Fatalf("RequestMerge: %v", err)
	}
}

func TestAFoldSparesWhatTheRecordSpares(t *testing.T) {
	tests := []struct {
		name   string
		spared func(h *harness, t *testing.T) ids.WorkspaceID
		want   wsm.FoldSpare
	}{
		// A one-shot is a created workspace, so a linked worktree.
		{"a one-shot", func(h *harness, t *testing.T) ids.WorkspaceID {
			ws := h.fold(t, h.linkedWorkspace(t, instant))
			h.oneShot(t, ws)
			return ws
		}, wsm.FoldSpareOneShot},
		{"a merge pending", func(h *harness, t *testing.T) ids.WorkspaceID {
			ws := h.collapsed(t)
			h.mergePending(t, ws)
			return ws
		}, wsm.FoldSpareMergePending},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			ws := tt.spared(h, t)

			// Act
			h.c.StandDownWithheld(ws, WithheldCacheLapse)

			// Assert
			if killed := h.stand.Killed(); len(killed) != 0 {
				t.Fatalf("kills = %+v, want none for a spared workspace", killed)
			}
			if term := h.terminal(t, ws); term != nil {
				t.Fatalf("terminal = %+v, want the session left as it stood", term)
			}
			if h.pending(ws) {
				t.Fatal("a spared workspace's stand-down stayed pending")
			}
			named := false
			for _, rec := range records(h.log, opWithheld) {
				if rec.Level == "info" && rec.Context["spare"] == string(tt.want) {
					named = true
				}
			}
			if !named {
				t.Fatalf("no %s record named the spare %q", opWithheld, tt.want)
			}
		})
	}
}

func TestABackloggedOneShotIsStillStoodDown(t *testing.T) {
	// Arrange: the user hid it, which no fold spare reaches.
	h := newHarness(t)
	ws := h.hide(t, h.linkedWorkspace(t, instant))
	h.oneShot(t, ws)

	// Act
	h.c.StandDownWithheld(ws, WithheldCacheLapse)

	// Assert
	if killed := h.stand.Killed(); len(killed) != 1 {
		t.Fatalf("kills = %+v, want the hidden one-shot stood down", killed)
	}
}

func TestAFoldedWorkspaceHeldBackWhileBusyIsStoodDownOnceFree(t *testing.T) {
	// Arrange: busy at the fold, it falls free with nothing in flight and
	// nothing queued, its repository still folded.
	h := newHarness(t)
	ws := h.collapsed(t)
	h.freeness.SetFree(ws, false)
	h.c.StandDownWithheld(ws, WithheldCacheLapse)
	if killed := h.stand.Killed(); len(killed) != 0 {
		t.Fatalf("kills = %+v, want none while busy", killed)
	}
	h.freeness.SetFree(ws, true)

	// Act
	h.c.RetryWithheld(ws)

	// Assert
	if killed := h.stand.Killed(); len(killed) != 1 {
		t.Fatalf("kills = %+v, want the stand-down taken on the free edge", killed)
	}
}

func TestAFoldedWorkspaceThatQueuedAMergeWhileBusyIsSparedOnceFree(t *testing.T) {
	// Arrange: busy at the fold, it finished its turn by queueing a merge.
	h := newHarness(t)
	ws := h.collapsed(t)
	h.freeness.SetFree(ws, false)
	h.c.StandDownWithheld(ws, WithheldCacheLapse)
	h.mergePending(t, ws)
	h.freeness.SetFree(ws, true)

	// Act
	h.c.RetryWithheld(ws)

	// Assert
	if killed := h.stand.Killed(); len(killed) != 0 {
		t.Fatalf("kills = %+v, want none once a merge is pending", killed)
	}
	if h.pending(ws) {
		t.Fatal("a spared workspace's stand-down stayed pending")
	}
}

func TestAWorkspaceWithAMergeQueuedIsNeverStoodDown(t *testing.T) {
	cases := []struct {
		name    string
		arrange func(t *testing.T, h *harness) ids.WorkspaceID
	}{
		{name: "hidden", arrange: func(t *testing.T, h *harness) ids.WorkspaceID { return h.backlogged(t) }},
		{name: "closed", arrange: func(t *testing.T, h *harness) ids.WorkspaceID {
			ws := h.workspace(t, instant)
			if err := h.db.MarkClosed(context.Background(), ws, wsm.Closure{At: instant, By: wsm.CloseByUser}); err != nil {
				t.Fatalf("MarkClosed: %v", err)
			}
			return ws
		}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: its merge waits in a queue.
			h := newHarness(t)
			ws := tc.arrange(t, h)
			queuedAt(t, h, ws, instant)

			// Act
			h.c.StandDownWithheld(ws, WithheldCacheLapse)

			// Assert
			if killed := h.stand.Killed(); len(killed) != 0 {
				t.Fatalf("kills = %+v, want the queued workspace kept", killed)
			}
			if _, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: "info", Operation: opWithheld, Message: "the workspace has a merge queued; its session keeps running"}); !ok {
				t.Fatal("no record says the queued workspace was kept")
			}
		})
	}
}

func TestAWithheldStandDownWhoseQueuesCannotBeReadIsDropped(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.backlogged(t)
	h.c.deps.DB = sweepReadErrDB{DB: h.c.deps.DB, read: "queues", err: errors.New("database is locked")}

	// Act
	h.c.StandDownWithheld(ws, WithheldCacheLapse)

	// Assert
	rec, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: "error", Operation: opWithheld})
	if !ok || !strings.Contains(fmt.Sprint(rec.Context["cause"]), "database is locked") || len(h.stand.Killed()) != 0 {
		t.Fatalf("error record = %+v, %v; want the queues' read failure and no stand-down", rec, ok)
	}
}

func TestAWithheldStandDownIsCountedUnderItsTriggersReason(t *testing.T) {
	tests := []struct {
		name     string
		withhold func(t *testing.T, h *harness) ids.WorkspaceID
		trigger  WithheldTrigger
		want     shimlife.StandDownReason
	}{
		{name: "a fold's", withhold: func(t *testing.T, h *harness) ids.WorkspaceID { return h.collapsed(t) },
			trigger: WithheldCollapse, want: shimlife.StandDownFold},
		{name: "a cache lapse's", withhold: func(t *testing.T, h *harness) ids.WorkspaceID { return h.backlogged(t) },
			trigger: WithheldCacheLapse, want: shimlife.StandDownCacheLapse},
		{name: "an ended merge's", withhold: func(t *testing.T, h *harness) ids.WorkspaceID { return h.backlogged(t) },
			trigger: WithheldMergeEnded, want: shimlife.StandDownMergeEnded},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			ws := tt.withhold(t, h)

			// Act
			h.c.StandDownWithheld(ws, tt.trigger)

			// Assert
			killed := h.stand.Killed()
			if len(killed) != 1 || killed[0].Life != tt.want {
				t.Fatalf("kills = %+v, want one counted as %s", killed, tt.want)
			}
		})
	}
}

func TestAWithheldStandDownStatesItsHold(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, h *harness, ws ids.WorkspaceID)
		want    sessionwhy.Hold
	}{
		{name: "a busy session is held back busy", arrange: func(_ *testing.T, h *harness, ws ids.WorkspaceID) {
			h.freeness.SetFree(ws, false)
		}, want: sessionwhy.HoldDeferredBusy},
		{name: "a workspace shown again is kept because shown", arrange: func(t *testing.T, h *harness, ws ids.WorkspaceID) {
			if err := h.db.SetBacklogged(context.Background(), ws, false, time.Unix(1_700_000_000, 0)); err != nil {
				t.Fatalf("SetBacklogged: %v", err)
			}
		}, want: sessionwhy.HoldWithheldShown},
		{name: "a session stood down holds nothing", arrange: func(*testing.T, *harness, ids.WorkspaceID) {}, want: ""},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			why := sessionwhy.NewTable()
			h := newHarness(t, func(d *Deps) { d.Why = why })
			ws := h.backlogged(t)
			why.SetHold(ws, sessionwhy.HoldDeferredBusy)
			tt.arrange(t, h, ws)

			// Act
			h.c.StandDownWithheld(ws, WithheldCacheLapse)

			// Assert
			if got := why.Facts(ws).Hold; got != tt.want {
				t.Fatalf("hold = %q, want %q", got, tt.want)
			}
		})
	}
}
