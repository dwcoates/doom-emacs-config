package merge

import (
	"context"
	"testing"
)

func TestATabCloseThatFailsUnderALiveRunIsRecordedAtError(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	r := steppedRun(h)

	// Act.
	r.closeTab(context.Background(), tabRound{kind: TabTests, n: 1, started: h.clock()}, "passed")

	// Assert.
	if !loggedLike(h, logLike{level: "error", op: "daemon.merge.tab_close", text: "could not record a tab interval's end"}) {
		t.Fatalf("no error record of the failed close: %+v", h.logs.Records())
	}
}

func TestAQueueChangeReadCutShortByTheStandDownIsRecordedAtInfo(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	rawInLine(t, h, theWorkspace)
	h.db.pausedReadErr = context.Canceled

	// Act.
	err := h.o.deps.DB.AdmitMerge(cancelled(), h.repoKey(), theWorkspace)

	// Assert.
	if err != nil {
		t.Fatalf("AdmitMerge: %v", err)
	}
	if !loggedLike(h, logLike{level: "info", op: opQueueEvents, text: "the merge queue's read after its change ended when its context was cancelled"}) {
		t.Fatalf("no info record of the stopped read: %+v", h.logs.Records())
	}
	if loggedLike(h, logLike{level: "error", op: opQueueEvents}) {
		t.Fatalf("a stopped read was recorded as a fault: %+v", h.logs.Records())
	}
}
