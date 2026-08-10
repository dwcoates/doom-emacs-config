package main

import (
	"bytes"
	"errors"
	"io"
	"testing"
	"time"

	"claude-repld/internal/dlog"
)

// stepClock advances a fixed amount per read, so a phase's measured duration is
// an asserted number rather than whatever the wall clock happened to do.
func stepClock(step time.Duration) func() time.Time {
	base := time.Unix(0, 0)
	n := 0
	return func() time.Time {
		at := base.Add(time.Duration(n) * step)
		n++
		return at
	}
}

func TestBootPhasesMarkEmitsOneRecordPerPhase(t *testing.T) {
	// Arrange
	var durable bytes.Buffer
	phases := newBootPhases(dlog.New(&durable, io.Discard, false), stepClock(250*time.Millisecond))

	// Act
	phases.Mark("registry-prepare")
	phases.Mark("ready")

	// Assert
	records := decodeRecords(t, &durable)
	if len(records) != 2 {
		t.Fatalf("records = %d, want one per marked phase", len(records))
	}
	for i, want := range []string{"registry-prepare", "ready"} {
		if records[i].Operation != "boot.phase" || records[i].Context["phase"] != want {
			t.Fatalf("record[%d] = %#v, want operation=boot.phase phase=%s", i, records[i], want)
		}
	}
}

func TestBootPhasesMarkMeasuresSincePreviousPhase(t *testing.T) {
	// Arrange
	var durable bytes.Buffer
	phases := newBootPhases(dlog.New(&durable, io.Discard, false), stepClock(250*time.Millisecond))

	// Act — the first mark is 250ms after start, the second another 250ms on.
	phases.Mark("first")
	phases.Mark("second")

	// Assert
	records := decodeRecords(t, &durable)
	if got := records[1].Context["elapsed_ms"]; got != float64(250) {
		t.Fatalf("second phase elapsed_ms = %v, want 250 (its own duration, not since boot)", got)
	}
	if got := records[1].Context["since_boot_ms"]; got != float64(500) {
		t.Fatalf("second phase since_boot_ms = %v, want 500", got)
	}
}

func TestBootPhasesDeferredCompletionIsLogged(t *testing.T) {
	// Arrange
	var durable bytes.Buffer
	phases := newBootPhases(dlog.New(&durable, io.Discard, false), stepClock(100*time.Millisecond))

	// Act
	phases.Deferred("geometry-backfill")(nil)

	// Assert
	records := decodeRecords(t, &durable)
	last := records[len(records)-1]
	if last.Operation != "boot.phase.deferred" || last.Context["outcome"] != "ok" {
		t.Fatalf("deferred completion = %#v, want operation=boot.phase.deferred outcome=ok", last)
	}
}

func TestBootPhasesDeferredFailureIsLoggedAtErrorLevel(t *testing.T) {
	// Arrange
	var durable bytes.Buffer
	phases := newBootPhases(dlog.New(&durable, io.Discard, false), stepClock(100*time.Millisecond))

	// Act
	phases.Deferred("geometry-backfill")(errors.New("git is gone"))

	// Assert — moving work off the boot path must not quieten its failures.
	last := decodeRecords(t, &durable)
	rec := last[len(last)-1]
	if rec.Level != dlog.LevelError || rec.Context["outcome"] != "failed" || rec.Context["error"] != "git is gone" {
		t.Fatalf("deferred failure = %#v, want error level, outcome=failed and the cause", rec)
	}
}
