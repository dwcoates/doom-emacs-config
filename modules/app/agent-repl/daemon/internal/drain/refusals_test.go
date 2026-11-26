package drain

import (
	"testing"
	"time"
)

func TestTheFirstRefusalInAWindowIsRecordedAtOnce(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.bare(t)

	// Act
	h.c.NoteRefusal(ws)

	// Assert
	warns := records(h.log, opRefusal)
	if len(warns) != 1 || warns[0].Level != "warn" {
		t.Fatalf("refusal records = %+v, want one WARN", warns)
	}
}

func TestFurtherRefusalsInsideTheWindowAreSuppressed(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.bare(t)
	h.c.NoteRefusal(ws)

	// Act
	h.c.NoteRefusal(ws)
	h.c.NoteRefusal(ws)

	// Assert
	var warns int
	for _, rec := range records(h.log, opRefusal) {
		if rec.Level == "warn" {
			warns++
		}
	}
	if warns != 1 {
		t.Fatalf("WARN records inside one window = %d, want 1", warns)
	}
}

func TestTheNextWindowsRecordCarriesTheExactSuppressedCount(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.bare(t)
	h.c.NoteRefusal(ws)
	h.c.NoteRefusal(ws)
	h.c.NoteRefusal(ws)

	// Act
	h.clock.Set(instant.Add(2 * time.Minute))
	h.c.NoteRefusal(ws)

	// Assert
	warns := warnRecords(records(h.log, opRefusal))
	if len(warns) != 2 {
		t.Fatalf("WARN records = %d, want 2 (one per window)", len(warns))
	}
	if warns[1].Context["suppressed"] != 2 {
		t.Fatalf("suppressed = %v, want the exact 2 backlogged inside the closed window", warns[1].Context["suppressed"])
	}
}

func TestTheRecordCarriesTheRunningTotalAcrossWindows(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.bare(t)
	h.c.NoteRefusal(ws)
	h.c.NoteRefusal(ws)

	// Act
	h.clock.Set(instant.Add(2 * time.Minute))
	h.c.NoteRefusal(ws)

	// Assert
	warns := warnRecords(records(h.log, opRefusal))
	if warns[1].Context["total"] != 3 {
		t.Fatalf("total = %v, want every refusal since the controller came up (3)", warns[1].Context["total"])
	}
}

func TestASuppressedRefusalStillCountsTowardTheTotal(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.bare(t)
	h.c.NoteRefusal(ws)

	// Act
	h.c.NoteRefusal(ws)

	// Assert
	debugs := records(h.log, opRefusal)
	last := debugs[len(debugs)-1]
	if last.Level != "debug" || last.Context["total"] != 2 {
		t.Fatalf("suppressed record = %+v, want a debug carrying total 2", last)
	}
}

func TestEachRefusalInOneWindowCarriesItsRunningCounts(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.bare(t)
	want := []struct {
		level             string
		suppressed, total int
	}{
		{level: "warn", suppressed: 0, total: 1},
		{level: "debug", suppressed: 1, total: 2},
		{level: "debug", suppressed: 2, total: 3},
	}

	// Act
	for range want {
		h.c.NoteRefusal(ws)
	}

	// Assert
	got := records(h.log, opRefusal)
	if len(got) != len(want) {
		t.Fatalf("refusal records = %+v, want %d", got, len(want))
	}
	for i, w := range want {
		if got[i].Level != w.level || got[i].Context["suppressed"] != w.suppressed || got[i].Context["total"] != w.total {
			t.Fatalf("record %d = %s suppressed %v total %v, want %s suppressed %d total %d",
				i, got[i].Level, got[i].Context["suppressed"], got[i].Context["total"], w.level, w.suppressed, w.total)
		}
	}
}
