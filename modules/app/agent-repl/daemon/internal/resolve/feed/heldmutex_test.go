package feed

import (
	"strings"
	"testing"
	"time"

	"agent-repld/internal/dlog"
)

// heldClock is a clock a test moves by hand.
type heldClock struct{ at time.Time }

func (c *heldClock) now() time.Time { return c.at }

func newHeldMutex() (*heldMutex, *heldClock, *dlog.TestLogger) {
	clock := &heldClock{at: time.UnixMilli(1_700_000_000_000)}
	log := dlog.NewTestLogger()
	return &heldMutex{report: &holdReport{log: log, now: clock.now}}, clock, log
}

// holdFor takes M, lets HELD pass on CLOCK, and releases it.
func holdFor(m *heldMutex, clock *heldClock, held time.Duration) {
	m.Lock()
	clock.at = clock.at.Add(held)
	m.Unlock()
}

func TestAHoldOfTheResolversMutexIsRecordedPastItsBudget(t *testing.T) {
	tests := []struct {
		name     string
		held     time.Duration
		wantHeld bool
	}{
		{name: "a hold under the budget records nothing", held: lockHoldBudget - time.Millisecond},
		{name: "a hold at the budget is recorded", held: lockHoldBudget, wantHeld: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			m, clock, log := newHeldMutex()

			// Act.
			holdFor(m, clock, tt.held)

			// Assert.
			rec, found := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "info", Operation: opLockHeld})
			if found != tt.wantHeld {
				t.Fatalf("hold record found = %v, want %v; records = %+v", found, tt.wantHeld, log.Records())
			}
			if !found {
				return
			}
			if rec.Context["held_ms"] != tt.held.Milliseconds() || rec.Context["budget_ms"] != lockHoldBudget.Milliseconds() {
				t.Fatalf("hold record context = %+v, want held %dms against %dms", rec.Context, tt.held.Milliseconds(), lockHoldBudget.Milliseconds())
			}
		})
	}
}

func TestALongHoldNamesTheFunctionThatTookTheMutex(t *testing.T) {
	// Arrange.
	m, clock, log := newHeldMutex()

	// Act.
	holdFor(m, clock, lockHoldBudget)

	// Assert.
	rec, found := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "info", Operation: opLockHeld})
	if !found {
		t.Fatalf("no hold record; records = %+v", log.Records())
	}
	if by, _ := rec.Context["by"].(string); by != "resolve/feed.holdFor" {
		t.Fatalf("hold by = %q, want the function that called Lock (resolve/feed.holdFor)", by)
	}
}

func TestLongHoldsAreRecordedOncePerInterval(t *testing.T) {
	// Arrange.
	m, clock, log := newHeldMutex()

	// Act: three long holds inside one interval, then one past it.
	holdFor(m, clock, lockHoldBudget)
	holdFor(m, clock, 2*lockHoldBudget)
	holdFor(m, clock, lockHoldBudget)
	clock.at = clock.at.Add(lockHoldEvery)
	holdFor(m, clock, lockHoldBudget)

	// Assert.
	records := dlog.FilterRecords(log.Records(), dlog.RecordQuery{Operation: opLockHeld})
	if len(records) != 2 {
		t.Fatalf("hold records = %d, want one per interval (2); records = %+v", len(records), records)
	}
	if got := records[1].Context["long_holds_since_last"]; got != 2 {
		t.Fatalf("absorbed holds = %v, want 2", got)
	}
	if got := records[1].Context["longest_since_last_ms"]; got != (2 * lockHoldBudget).Milliseconds() {
		t.Fatalf("longest absorbed hold = %v, want %d", got, (2 * lockHoldBudget).Milliseconds())
	}
}

func TestFunctionAtNamesAnUnknownCounterAsUnknown(t *testing.T) {
	// Act.
	got := functionAt(0)

	// Assert.
	if !strings.EqualFold(got, "unknown") {
		t.Fatalf("functionAt(0) = %q, want unknown", got)
	}
}
