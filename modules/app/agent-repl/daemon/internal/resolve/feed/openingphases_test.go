package feed

import (
	"reflect"
	"testing"
	"time"

	"agent-repld/internal/dlog"
)

func TestOpeningPhasesFieldsAreEachPhaseInMilliseconds(t *testing.T) {
	// Arrange.
	p := &openingPhases{
		loadLockWait: 1 * time.Millisecond, lockWait: 2 * time.Millisecond, read: 3 * time.Millisecond,
		records: 4 * time.Millisecond, replayLockWait: 5 * time.Millisecond, replay: 6 * time.Millisecond,
	}

	// Act.
	got := p.fields()

	// Assert.
	want := dlog.Context{
		"load_lock_wait_ms": int64(1), "lock_wait_ms": int64(2), "read_ms": int64(3),
		"records_ms": int64(4), "replay_lock_wait_ms": int64(5), "replay_ms": int64(6),
	}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("fields = %+v, want %+v", got, want)
	}
}

func TestPhaseIsNilOutsideAnOpening(t *testing.T) {
	// Arrange.
	var none *openingPhases
	some := &openingPhases{}

	// Act.
	gotNone, gotSome := phase(none, readOf), phase(some, readOf)

	// Assert.
	if gotNone != nil {
		t.Fatalf("phase of no phases = %p, want nil", gotNone)
	}
	if gotSome != &some.read {
		t.Fatalf("phase = %p, want the read field %p", gotSome, &some.read)
	}
}

func TestATimedLockAddsItsWait(t *testing.T) {
	tests := []struct {
		name  string
		timed bool
		want  time.Duration
	}{
		{name: "a timed take adds the time it waited", timed: true, want: 40 * time.Millisecond},
		{name: "an untimed take adds nothing", timed: false, want: 0},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: the clock moves 40ms between the ask and the take.
			h := newHarness(t)
			reads := 0
			h.resolver.deps.Now = func() time.Time {
				reads++
				return time.UnixMilli(h.nowMs + int64(40*(reads-1)))
			}
			var waited time.Duration
			into := &waited
			if !tt.timed {
				into = nil
			}

			// Act.
			h.resolver.lockTimed(into)
			h.resolver.mu.Unlock()

			// Assert.
			if waited != tt.want {
				t.Fatalf("waited = %v, want %v", waited, tt.want)
			}
		})
	}
}

func TestAnOpeningRecordsWhereItsTimeWent(t *testing.T) {
	tests := []struct {
		name  string
		field string
		slow  func(h *harness, store *fakeHistory)
	}{
		{name: "the shim's history read", field: "read_ms", slow: func(h *harness, store *fakeHistory) {
			store.inRead = func(int) { h.nowMs += 700 }
		}},
		{name: "the durable records read before the replay", field: "records_ms", slow: func(h *harness, _ *fakeHistory) {
			h.inRecords = func() { h.nowMs += 700 }
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			store := h.headsBook()
			tt.slow(h, store)

			// Act.
			h.openPage(rootFeed(), "reader-1")

			// Assert.
			if !h.hasRecordWith(dlog.LevelInfo, "daemon.feed.opening_reads", tt.field, int64(700)) {
				t.Fatalf("no opening record names %s as 700ms; records = %+v", tt.field, h.records())
			}
		})
	}
}
