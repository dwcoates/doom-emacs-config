package cachelapse

import (
	"context"
	"errors"
	"fmt"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/wsm"
)

const (
	// bootArmed is the boot pass's last record: every workspace scheduled.
	bootArmed = "armed every workspace's next keep-alive"
	// promotedAsked is the record of the withheld records asked again.
	promotedAsked = "the state handle was promoted; asked again for the keep-alive records withheld while this daemon joined"
)

// joining is a harness whose state handle is a joining successor's, with a
// promotion latch the test closes.
func joining(t *testing.T, rec wsm.Workspace) *harness {
	t.Helper()
	h := newHarness(t, rec)
	h.db.readOnly = true
	h.db.promoted = make(chan struct{})
	return h
}

// capped is a visible workspace whose keep-alive run stands at its cap, its
// cache already marked cold.
func capped() wsm.Workspace {
	rec := beatable(visible(), KeepaliveInterval/2)
	rec.KeepaliveRunBeats = KeepaliveRunCap
	rec.CacheColdTurnAt = rec.LastTurnEndedAt
	return rec
}

// runKeeper runs H's keeper until the test ends, and answers its logger and a
// stop that cancels the loop and waits for it.
func runKeeper(t *testing.T, h *harness) (*dlog.TestLogger, func()) {
	t.Helper()
	ctx, cancel := context.WithCancel(context.Background())
	done := make(chan error, 1)
	go func() { done <- h.keeper.Run(ctx) }()
	stop := func() {
		cancel()
		if err := <-done; err != nil {
			t.Fatalf("Run = %v, want nil", err)
		}
	}
	t.Cleanup(cancel)
	return h.log.Global().(*dlog.TestLogger), stop
}

func await(t *testing.T, log *dlog.TestLogger, q dlog.RecordQuery) dlog.Record {
	t.Helper()
	rec, err := log.Await(t.Context(), q)
	if err != nil {
		t.Fatalf("await %+v: %v; records %+v", q, err, log.Records())
	}
	return rec
}

func TestAWithheldRunEndKeepsTheEndingThatDecidesMore(t *testing.T) {
	tests := []struct {
		name          string
		first, second wsm.KeepaliveRunEnding
		want          wsm.KeepaliveRunEnding
	}{
		{name: "the cap outranks a later closure", first: wsm.RunEndingCap, second: wsm.RunEndingClosure, want: wsm.RunEndingCap},
		{name: "the cap outranks an earlier closure", first: wsm.RunEndingClosure, second: wsm.RunEndingCap, want: wsm.RunEndingCap},
		{name: "the closure outranks a later turn's close", first: wsm.RunEndingClosure, second: wsm.RunEndingTurnClosed, want: wsm.RunEndingClosure},
		{name: "the closure outranks an earlier beat", first: wsm.RunEndingBeat, second: wsm.RunEndingClosure, want: wsm.RunEndingClosure},
		{name: "of two that decide alike the first stands", first: wsm.RunEndingBeat, second: wsm.RunEndingTurnClosed, want: wsm.RunEndingBeat},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := overdue(t)
			h.db.readOnly = true
			// Act
			h.keeper.endRun(context.Background(), h.log.Global(), theWorkspace, tt.first)
			h.keeper.endRun(context.Background(), h.log.Global(), theWorkspace, tt.second)
			// Assert
			if got := h.keeper.withheldEnds[theWorkspace]; got != tt.want {
				t.Fatalf("withheld ending = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestAPromotionAsksAgainForTheRunEndTheBootPassWithheld(t *testing.T) {
	// Arrange
	h := joining(t, capped())
	run := wsm.KeepaliveRun{Beats: KeepaliveRunCap, End: wsm.KeepaliveRunCapped}
	h.db.endedRun = &run
	log, stop := runKeeper(t, h)
	await(t, log, dlog.RecordQuery{Level: "info", Operation: opDrive, Message: bootArmed})
	h.db.mu.Lock()
	withheld := len(h.db.runEndings)
	h.db.mu.Unlock()
	if withheld != 0 {
		t.Fatalf("the boot pass asked %d run ends of a read-only handle, want none", withheld)
	}
	// Act
	h.db.promote()
	await(t, log, dlog.RecordQuery{Level: "info", Operation: opDrive, Message: promotedAsked})
	stop()
	// Assert
	if len(h.db.runEndings) == 0 || h.db.runEndings[0] != wsm.RunEndingCap {
		t.Fatalf("run endings asked = %v, want the cap first", h.db.runEndings)
	}
	if len(h.telemetry.runs) != 1 || h.telemetry.runs[0] != run {
		t.Fatalf("telemetry runs = %+v, want the capped run once", h.telemetry.runs)
	}
}

func TestAPromotionWritesTheOwingDecisionTheBootPassWithheld(t *testing.T) {
	// Arrange: a backlogged workspace nobody looks at owes no keep-alive.
	h := joining(t, backlogged())
	log, stop := runKeeper(t, h)
	await(t, log, dlog.RecordQuery{Level: "info", Operation: opDrive, Message: bootArmed})
	h.db.mu.Lock()
	withheld := h.db.owingWrites
	h.db.mu.Unlock()
	if withheld != 0 {
		t.Fatalf("the boot pass wrote %d owing decisions on a read-only handle, want none", withheld)
	}
	// Act
	h.db.promote()
	await(t, log, dlog.RecordQuery{Level: "info", Operation: opDrive, Message: promotedAsked})
	stop()
	// Assert
	if h.db.owingWrites != 1 || h.db.records[theWorkspace].KeepaliveOwedNoneAt == nil {
		t.Fatalf("writes = %d, row = %+v; want the decision that it owes none written once", h.db.owingWrites, h.db.records[theWorkspace])
	}
}

func TestAStoreWritingFromTheStartRecordsNoPromotion(t *testing.T) {
	// Arrange
	h := newHarness(t, capped())
	log, stop := runKeeper(t, h)
	// Act
	await(t, log, dlog.RecordQuery{Level: "debug", Operation: opDrive, Message: "the state handle writes; this keeper withheld nothing while it joined"})
	stop()
	// Assert
	if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "info", Message: promotedAsked}); ok {
		t.Fatalf("records = %+v, want no promotion record for a handle that always wrote", log.Records())
	}
}

func TestARunEndThatFailsAfterThePromotionIsAnError(t *testing.T) {
	// Arrange
	h := joining(t, capped())
	h.db.endErr = errors.New("store unreachable")
	log, stop := runKeeper(t, h)
	await(t, log, dlog.RecordQuery{Level: "info", Operation: opDrive, Message: bootArmed})
	// Act
	h.db.promote()
	rec := await(t, log, dlog.RecordQuery{Level: "error", Operation: opDrive,
		Message: "could not record the end of the workspace's keep-alive run; it is asked again at its next moment"})
	stop()
	// Assert
	if rec.Context["ending"] != int(wsm.RunEndingCap) || rec.Context["cause"] != "store unreachable" {
		t.Fatalf("error record = %+v, want the cap's ending and the cause", rec)
	}
}

func TestARunEndForAWorkspaceForgottenMeanwhileIsRecordedAtInfo(t *testing.T) {
	// Arrange
	h := joining(t, capped())
	h.db.endErr = fmt.Errorf("wsm: workspace %s: %w", theWorkspace, wsm.ErrNotFound)
	log, stop := runKeeper(t, h)
	await(t, log, dlog.RecordQuery{Level: "info", Operation: opDrive, Message: bootArmed})
	// Act
	h.db.promote()
	rec := await(t, log, dlog.RecordQuery{Level: "info", Operation: opDrive,
		Message: "the workspace is no longer registered; it has no keep-alive run to end"})
	stop()
	// Assert
	if rec.Context["ending"] != int(wsm.RunEndingCap) {
		t.Fatalf("record = %+v, want the cap's ending", rec)
	}
	if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: opDrive}); ok {
		t.Fatalf("records = %+v, want no error for a forgotten workspace's run end", log.Records())
	}
}

func TestALatchClosedOnAReadOnlyHandleIsAnError(t *testing.T) {
	// Arrange
	h := overdue(t)
	h.db.readOnly = true
	h.keeper.endRun(context.Background(), h.log.Global(), theWorkspace, wsm.RunEndingCap)
	// Act
	h.keeper.promoted(context.Background())
	// Assert
	if _, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: "error", Operation: opDrive}); !ok {
		t.Fatalf("records = %+v, want the broken latch at ERROR", h.log.Records())
	}
	if len(h.db.runEndings) != 0 || h.keeper.withheldEnds[theWorkspace] != wsm.RunEndingCap {
		t.Fatalf("run endings = %v, withheld = %v; want nothing asked and the end still withheld", h.db.runEndings, h.keeper.withheldEnds)
	}
}
