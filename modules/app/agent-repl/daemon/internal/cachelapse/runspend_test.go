package cachelapse

import (
	"context"
	"errors"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

func spendOf(read, output uint64) *conversationv1.VendorTokens {
	return &conversationv1.VendorTokens{CacheRead: read, Output: output}
}

func TestAddRunSpendAddsWhatTheKeepAliveSpendGrewBySinceItsMark(t *testing.T) {
	tests := []struct {
		name string
		// marked takes a mark before the answer; before and after are the
		// workspace's keep-alive spend at the mark and at the answer.
		marked        bool
		before, after *conversationv1.VendorTokens
		want          []wsm.KeepaliveRunTokens
	}{
		{name: "a keep-alive's growth is added", marked: true, before: spendOf(1000, 3), after: spendOf(1900, 5),
			want: []wsm.KeepaliveRunTokens{{CacheRead: 900, Output: 2}}},
		{name: "no growth adds nothing", marked: true, before: spendOf(1000, 3), after: spendOf(1000, 3)},
		{name: "no mark adds nothing", marked: false, before: spendOf(0, 0), after: spendOf(1900, 5)},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := overdue(t)
			h.spent = map[ids.WorkspaceID]*conversationv1.VendorTokens{theWorkspace: tt.before}
			if tt.marked {
				h.keeper.markRunSpend(theWorkspace)
			}
			h.spent[theWorkspace] = tt.after
			// Act
			h.keeper.addRunSpend(context.Background(), h.log.Global(), theWorkspace)
			// Assert
			if len(h.db.runSpends) != len(tt.want) || (len(tt.want) == 1 && h.db.runSpends[0] != tt.want[0]) {
				t.Fatalf("run spends = %+v, want %+v", h.db.runSpends, tt.want)
			}
		})
	}
}

func TestAMarkIsTakenOnceAndMovedByEachAnswer(t *testing.T) {
	// Arrange
	h := overdue(t)
	h.spent = map[ids.WorkspaceID]*conversationv1.VendorTokens{theWorkspace: spendOf(100, 1)}
	h.keeper.markRunSpend(theWorkspace)
	h.spent[theWorkspace] = spendOf(500, 2)
	h.keeper.markRunSpend(theWorkspace)
	// Act
	h.keeper.addRunSpend(context.Background(), h.log.Global(), theWorkspace)
	h.spent[theWorkspace] = spendOf(800, 4)
	h.keeper.addRunSpend(context.Background(), h.log.Global(), theWorkspace)
	// Assert
	want := []wsm.KeepaliveRunTokens{{CacheRead: 400, Output: 1}, {CacheRead: 300, Output: 2}}
	if len(h.db.runSpends) != 2 || h.db.runSpends[0] != want[0] || h.db.runSpends[1] != want[1] {
		t.Fatalf("run spends = %+v, want %+v", h.db.runSpends, want)
	}
}

func TestAFailedRunSpendWriteIsRecordedAtError(t *testing.T) {
	// Arrange
	h := overdue(t)
	h.spent = map[ids.WorkspaceID]*conversationv1.VendorTokens{theWorkspace: spendOf(0, 0)}
	h.keeper.markRunSpend(theWorkspace)
	h.spent[theWorkspace] = spendOf(900, 2)
	h.db.spendErr = errors.New("store unreachable")
	// Act
	h.keeper.addRunSpend(context.Background(), h.log.Global(), theWorkspace)
	// Assert
	rec, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: "error", Operation: opDrive})
	if !ok || rec.Context["cause"] != "store unreachable" || rec.Context["spent"] == nil {
		t.Fatalf("no ERROR record naming the cause and the spend: %v", h.log.Records())
	}
}

func TestASpendNoRunTookIsRecordedAtDebug(t *testing.T) {
	// Arrange
	h := overdue(t)
	h.spent = map[ids.WorkspaceID]*conversationv1.VendorTokens{theWorkspace: spendOf(0, 0)}
	h.keeper.markRunSpend(theWorkspace)
	h.spent[theWorkspace] = spendOf(900, 2)
	h.db.spendRefused = true
	// Act
	h.keeper.addRunSpend(context.Background(), h.log.Global(), theWorkspace)
	// Assert
	if _, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: "debug", Message: "a keep-alive's spend was answered after a turn ended its run; it belongs to no run"}); !ok {
		t.Fatalf("no DEBUG record of the spend no run took: %v", h.log.Records())
	}
}

func TestGrowthPanicsOnAFigureThatWentDown(t *testing.T) {
	defer func() {
		if recover() == nil {
			t.Fatal("growth did not panic on a figure that went down")
		}
	}()
	growth(5, 4)
}

func TestEndRun(t *testing.T) {
	run := wsm.KeepaliveRun{Beats: 7, Spent: wsm.KeepaliveRunTokens{CacheRead: 6300}, End: wsm.KeepaliveRunResumed}
	tests := []struct {
		name  string
		ended *wsm.KeepaliveRun
		want  int
	}{
		{name: "an ended run reaches the telemetry", ended: &run, want: 1},
		{name: "a run that has not ended reaches nothing", ended: nil, want: 0},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := overdue(t)
			h.db.endedRun = tt.ended
			// Act
			h.keeper.endRun(context.Background(), h.log.Global(), theWorkspace, wsm.RunEndingTurnClosed)
			// Assert
			if len(h.telemetry.runs) != tt.want || (tt.want == 1 && h.telemetry.runs[0] != run) {
				t.Fatalf("telemetry runs = %+v, want %d of %+v", h.telemetry.runs, tt.want, run)
			}
		})
	}
}

func TestAFailedRunEndReadIsRecordedAtError(t *testing.T) {
	// Arrange
	h := overdue(t)
	h.db.endErr = errors.New("store unreachable")
	// Act
	h.keeper.endRun(context.Background(), h.log.Global(), theWorkspace, wsm.RunEndingCap)
	// Assert
	rec, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: "error", Operation: opDrive})
	if !ok || rec.Context["cause"] != "store unreachable" || rec.Context["ending"] != int(wsm.RunEndingCap) {
		t.Fatalf("no ERROR record naming the cause and the ending: %v", h.log.Records())
	}
	if len(h.telemetry.runs) != 0 {
		t.Fatalf("a failed read reached the telemetry: %+v", h.telemetry.runs)
	}
}

func TestTheRunsMomentsAskForItsEnd(t *testing.T) {
	tests := []struct {
		name string
		act  func(h *harness)
		want wsm.KeepaliveRunEnding
	}{
		{name: "a turn's close", act: func(h *harness) {
			h.keeper.apply(context.Background(), edge{ws: theWorkspace, kind: edgeTurnClosed, why: "a turn's close was recorded"})
		}, want: wsm.RunEndingTurnClosed},
		{name: "a close or a reopen", act: func(h *harness) {
			h.keeper.apply(context.Background(), edge{ws: theWorkspace, kind: edgeClosure, why: "the workspace was closed or reopened"})
		}, want: wsm.RunEndingClosure},
		{name: "the cap", act: func(h *harness) {
			rec := h.db.records[theWorkspace]
			rec.KeepaliveRunBeats = KeepaliveRunCap
			h.keeper.runCapped(context.Background(), h.log.Global(), theWorkspace, rec, false)
		}, want: wsm.RunEndingCap},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := overdue(t)
			// Act
			tt.act(h)
			// Assert
			found := false
			for _, ending := range h.db.runEndings {
				found = found || ending == tt.want
			}
			if !found {
				t.Fatalf("run endings asked = %v, want %v among them", h.db.runEndings, tt.want)
			}
		})
	}
}

func TestTheEdgesNameTheirRunMoments(t *testing.T) {
	// Arrange
	h := overdue(t)
	// Act
	h.keeper.TurnClosed(theWorkspace)
	h.keeper.ClosureChanged(theWorkspace)
	// Assert
	queued := h.keeper.take()
	if len(queued) != 2 || queued[0].kind != edgeTurnClosed || queued[1].kind != edgeClosure {
		t.Fatalf("queued = %+v, want a turn-closed edge and a closure edge", queued)
	}
}

func TestAJoiningSuccessorWritesNoRunEnd(t *testing.T) {
	// Arrange
	h := overdue(t)
	h.db.readOnly = true
	// Act
	h.keeper.endRun(context.Background(), h.log.Global(), theWorkspace, wsm.RunEndingCap)
	// Assert
	if len(h.db.runEndings) != 0 {
		t.Fatalf("run endings asked = %v, want none on a read-only handle", h.db.runEndings)
	}
	if _, errored := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: "error"}); errored {
		t.Fatalf("records = %+v, want no ERROR for a joining successor", h.log.Records())
	}
}

func TestAJoiningSuccessorRecordsTheWithheldRunEndAtDebug(t *testing.T) {
	// Arrange
	h := overdue(t)
	h.db.readOnly = true
	// Act
	h.keeper.endRun(context.Background(), h.log.Global(), theWorkspace, wsm.RunEndingCap)
	// Assert
	rec, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: "debug", Operation: opDrive,
		Message: "the state handle is not promoted yet; the end of the workspace's keep-alive run is recorded once it writes"})
	if !ok || rec.Context["ending"] != int(wsm.RunEndingCap) {
		t.Fatalf("records = %+v, want the withheld end at DEBUG naming its ending", h.log.Records())
	}
	if got := h.keeper.withheldEnds[theWorkspace]; got != wsm.RunEndingCap {
		t.Fatalf("withheld ending = %v, want %v", got, wsm.RunEndingCap)
	}
}
