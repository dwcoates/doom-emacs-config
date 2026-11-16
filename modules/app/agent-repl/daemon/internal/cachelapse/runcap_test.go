package cachelapse

import (
	"context"
	"errors"
	"math"
	"slices"
	"strings"
	"testing"
	"time"

	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// running is a visible workspace whose last turn closed ten minutes ago, with
// BEATS keep-alives sent since.
func running(beats int) wsm.Workspace {
	rec := beatable(visible(), 10*time.Minute)
	rec.KeepaliveRunBeats = beats
	return rec
}

func TestTheRunCapIsTheBreakEvenOfTheVendorsPrices(t *testing.T) {
	// Arrange
	want := int(math.Ceil(float64(CacheWritePriceTenths-CacheReadPriceTenths) / float64(CacheReadPriceTenths)))

	// Act, Assert
	if KeepaliveRunCap != want || KeepaliveRunCap != 19 {
		t.Fatalf("KeepaliveRunCap = %d, want ceil((write - read) / read) = %d, 19 at the vendor's prices", KeepaliveRunCap, want)
	}
}

func TestScheduleByTheKeepaliveRun(t *testing.T) {
	tests := []struct {
		name   string
		beats  int
		queued bool
		armed  bool
	}{
		{name: "a run one short of its break-even beats again", beats: KeepaliveRunCap - 1, armed: true},
		{name: "a run at its break-even owes no keep-alive", beats: KeepaliveRunCap},
		{name: "a run past its break-even owes no keep-alive", beats: KeepaliveRunCap + 5},
		{name: "a workspace with a merge queued is never capped", beats: KeepaliveRunCap + 5, queued: true, armed: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t, running(tt.beats))
			if tt.queued {
				h.db.queued = []ids.WorkspaceID{theWorkspace}
			}

			// Act
			h.keeper.schedule(context.Background(), theWorkspace, "test")

			// Assert
			if _, armed := h.keeper.beats[theWorkspace]; armed != tt.armed {
				t.Fatalf("beat armed = %v, want %v", armed, tt.armed)
			}
		})
	}
}

func TestACappedRunStatesWhyItOwesNoKeepalive(t *testing.T) {
	// Arrange
	h := newHarness(t, running(KeepaliveRunCap))

	// Act
	h.keeper.schedule(context.Background(), theWorkspace, "test")

	// Assert
	record, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: "info", Operation: opDrive, Message: "the workspace owes no keep-alive"})
	why, _ := record.Context["why"].(string)
	if !ok || !strings.Contains(why, "break-even") || record.Context["workspace"] != string(theWorkspace) {
		t.Fatalf("records = %+v, want the cap's reason for the workspace", h.log.Records())
	}
}

func TestACappedRunMarksTheCacheCold(t *testing.T) {
	// Arrange
	h := newHarness(t, running(KeepaliveRunCap))

	// Act
	h.keeper.schedule(context.Background(), theWorkspace, "test")

	// Assert
	if rec := h.db.records[theWorkspace]; h.db.coldMarks != 1 || !rec.CacheKnownCold() {
		t.Fatalf("cold marks = %d, known cold = %v; want the cache marked cold once", h.db.coldMarks, rec.CacheKnownCold())
	}
}

func TestACappedRunRecordsItsBreakEvenWithTheRunAndThePrices(t *testing.T) {
	// Arrange
	h := newHarness(t, running(KeepaliveRunCap))

	// Act
	h.keeper.schedule(context.Background(), theWorkspace, "test")

	// Assert
	record, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: "info", Operation: opDrive,
		Message: "the workspace's keep-alive run reached its break-even; its cache is left to lapse and marked cold"})
	got := []any{record.Context["workspace"], record.Context["run_beats"], record.Context["run_cap"], record.Context["read_price_tenths"], record.Context["write_price_tenths"]}
	want := []any{string(theWorkspace), KeepaliveRunCap, KeepaliveRunCap, CacheReadPriceTenths, CacheWritePriceTenths}
	if !ok || !slices.Equal(got, want) {
		t.Fatalf("record = %+v (found %v), want %v", record.Context, ok, want)
	}
}

func TestACappedRunAlreadyMarkedColdIsNotMarkedAgain(t *testing.T) {
	// Arrange
	h := newHarness(t, running(KeepaliveRunCap))
	h.keeper.schedule(context.Background(), theWorkspace, "test")

	// Act
	h.keeper.schedule(context.Background(), theWorkspace, "test")

	// Assert
	if h.db.coldMarks != 1 {
		t.Fatalf("cold marks = %d, want 1: a cache already known cold is not marked again", h.db.coldMarks)
	}
}

func TestACappedRunWhoseMarkFailsStillOwesNoKeepalive(t *testing.T) {
	// Arrange
	h := newHarness(t, running(KeepaliveRunCap))
	h.db.coldErr = errors.New("disk full")

	// Act
	h.keeper.schedule(context.Background(), theWorkspace, "test")

	// Assert
	if _, armed := h.keeper.beats[theWorkspace]; armed {
		t.Fatal("a beat was armed for a run at its break-even")
	}
	record, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: "error", Operation: opColdMark})
	if !ok || record.Context["cause"] != "disk full" || !strings.Contains(record.Context["why"].(string), "break-even") {
		t.Fatalf("records = %+v, want the failed mark at ERROR with its cause and the cap's reason", h.log.Records())
	}
}

func TestTheBeatThatReachesTheBreakEvenIsTheRunsLast(t *testing.T) {
	// Arrange: the run is one short, and its beat is due.
	rec := beatable(visible(), KeepaliveInterval+time.Minute)
	rec.KeepaliveRunBeats = KeepaliveRunCap - 1
	h := newHarness(t, rec)
	h.shim.answer = &shimv1.SendKeepaliveSuccess{Outcome: &shimv1.SendKeepaliveSuccess_Sent{Sent: &shimv1.SendKeepaliveSent{}}}
	h.keeper.schedule(context.Background(), theWorkspace, "test")
	h.keeper.beatsDue(context.Background())
	h.clock.Set(now.Add(KeepaliveInterval))

	// Act
	h.keeper.beatsDue(context.Background())

	// Assert
	if h.shim.beats != 1 || h.db.coldMarks != 1 {
		t.Fatalf("beats = %d, cold marks = %d; want the run's last beat sent and no more, its cache marked cold", h.shim.beats, h.db.coldMarks)
	}
}
