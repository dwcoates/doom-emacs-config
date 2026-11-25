package rollout

import (
	"context"
	"errors"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

func TestARenameBounceMovesBetweenTheStandDownAndThePrelaunch(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)

	// Act
	if err := runRelaunch(t, h, ws, ReasonRename); err != nil {
		t.Fatalf("bounce: %v", err)
	}

	// Assert: the shim stands down, the workspace moves, and only then is its
	// replacement spawned, installed and resumed where it moved.
	taken := h.order.Taken()
	kill, move, prelaunch := indexOf(taken, "kill_session"), indexOf(taken, "move"), indexOf(taken, "prelaunch")
	install, resume := indexOf(taken, "install"), indexOf(taken, "resume")
	if kill < 0 || move < 0 || prelaunch < 0 || !(kill < move && move < prelaunch && prelaunch < install && install < resume) {
		t.Fatalf("steps = %v, want stand-down, move, prelaunch, install, resume", taken)
	}
}

func TestARenameBounceOfAnUnservedWorkspaceMovesAndRelaunchesNothing(t *testing.T) {
	// Arrange: no shim serves the workspace.
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.fleet.mu.Lock()
	delete(h.fleet.live, ws)
	h.fleet.mu.Unlock()

	// Act
	err := bounceAndWait(t, h, ws, ReasonRename, false)

	// Assert
	if err != nil {
		t.Fatalf("bounce = %v, want the move to finish it", err)
	}
	taken := h.order.Taken()
	if indexOf(taken, "move") < 0 || indexOf(taken, "prelaunch") >= 0 || indexOf(taken, "resume") >= 0 {
		t.Fatalf("steps = %v, want the move and nothing brought up", taken)
	}
}

func TestARenameBounceWhoseMoveFailsBringsNothingUp(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.moves.applyErr = errors.New("worktree locked")

	// Act
	err := runRelaunch(t, h, ws, ReasonRename)

	// Assert: the failure surfaces and is recorded, nothing is prelaunched,
	// and the restart hold is released.
	if !errors.Is(err, h.moves.applyErr) {
		t.Fatalf("bounce = %v, want the move's failure", err)
	}
	if indexOf(h.order.Taken(), "prelaunch") >= 0 {
		t.Fatalf("steps = %v, want nothing prelaunched after a failed move", h.order.Taken())
	}
	if _, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: opRelaunch,
		Message: "the rename's move failed after the shim stood down; it stays owed, and the next boot finishes it"}); !ok {
		t.Fatalf("relaunch records = %v, want the failed move at ERROR", records(h.log, opRelaunch))
	}
	if _, held, dbErr := h.db.Lease(context.Background(), ws); dbErr != nil || held {
		t.Fatalf("Lease = (held %v, %v), want the restart hold released", held, dbErr)
	}
}

func TestARenameMoveWaitsBehindAMerge(t *testing.T) {
	// Act
	waits := ReasonRename.waitsBehindAMerge()

	// Assert
	if !waits || !ReasonRename.movesFirst() {
		t.Fatalf("ReasonRename waits behind a merge %v, moves first %v; want both", waits, ReasonRename.movesFirst())
	}
}

func TestTakeUpOwedMove(t *testing.T) {
	tests := []struct {
		name      string
		owed      bool
		owedErr   error
		wantAsked bool
		wantLevel string
	}{
		{name: "an owed move is asked of the registry", owed: true, wantAsked: true, wantLevel: dlog.LevelInfo},
		{name: "no owed move asks nothing", wantAsked: false},
		{name: "an unreadable move is an error and asks nothing", owedErr: errors.New("store closed"), wantLevel: dlog.LevelError},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			ws, _ := h.workspace(t)
			h.registry.park = true
			h.moves.owed[ws], h.moves.owedErr = tt.owed, tt.owedErr

			// Act
			h.c.TakeUpOwedMove(context.Background(), ws)

			// Assert
			if asked := renameAsked(h, ws); asked != tt.wantAsked {
				t.Fatalf("rename asked = %v, want %v", asked, tt.wantAsked)
			}
			if tt.wantLevel != "" {
				if _, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: tt.wantLevel, Operation: opOwedMove}); !ok {
					t.Fatalf("records = %v, want one at %s", records(h.log, opOwedMove), tt.wantLevel)
				}
			}
		})
	}
}

// renameAsked reports whether a rename bounce was asked of the registry for WS.
func renameAsked(h *harness, ws ids.WorkspaceID) bool {
	h.registry.mu.Lock()
	defer h.registry.mu.Unlock()
	for _, call := range h.registry.requests {
		if call.WS == ws && call.Req.Reason == string(ReasonRename) {
			return true
		}
	}
	return false
}

func TestAFreshBootsSettleTakesUpEveryOwedMove(t *testing.T) {
	// Arrange: a rename recorded before the stop owes w its move.
	h := newHarness(t)
	ws, dir := h.workspace(t)
	h.registry.park = true
	if err := h.db.Rename(context.Background(), ws, "moved", &wsm.MoveTarget{Branch: "moved", Dir: dir + "-moved"}); err != nil {
		t.Fatalf("Rename: %v", err)
	}
	h.moves.owed[ws] = true

	// Act
	h.c.SettleBoot(context.Background())

	// Assert
	if !renameAsked(h, ws) {
		t.Fatal("the fresh boot did not ask for the owed move")
	}
}

func TestAnAdoptionTakesUpTheWorkspacesOwedMove(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.moves.owed[ws] = true

	// Act
	arm(t, h, ws, Participants{})

	// Assert
	if !renameAsked(h, ws) {
		t.Fatal("the adoption did not take up the workspace's owed move")
	}
}

func TestAnOwedMoveTakeUpTheStandDownCutIsNoFailure(t *testing.T) {
	// Arrange: the daemon is stopping, and the read is cut under it.
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.moves.owedErr = context.Canceled
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act
	h.c.TakeUpOwedMove(ctx, ws)

	// Assert
	if _, failed := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: opOwedMove}); failed {
		t.Fatalf("records = %v, want no error for a cut take-up", records(h.log, opOwedMove))
	}
	if _, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: opOwedMove}); !ok {
		t.Fatalf("records = %v, want the cut recorded at INFO", records(h.log, opOwedMove))
	}
	if renameAsked(h, ws) {
		t.Fatal("a cut take-up asked for the move")
	}
}
