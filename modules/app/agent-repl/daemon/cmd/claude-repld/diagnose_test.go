package main

import (
	"strings"
	"testing"

	"claude-repld/internal/dlog"
)

// TestRecordGoroutineDumpLandsAtError pins that the dump reaches the run log at
// ERROR, which is where a later diagnosis reads: Emacs discards the daemon's
// stderr, so the runtime's own rendering goes nowhere.
func TestRecordGoroutineDumpLandsAtError(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()

	// Act.
	recordGoroutineDump(log, "daemon.cmd.sigquit", "dumping", nil)

	// Assert.
	records := log.Records()
	if len(records) != 1 {
		t.Fatalf("records = %+v, want exactly one", records)
	}
	if records[0].Level != dlog.LevelError || records[0].Operation != "daemon.cmd.sigquit" {
		t.Fatalf("record = %+v, want an error daemon.cmd.sigquit record", records[0])
	}
	dump, ok := records[0].Context["goroutine_dump"].(string)
	if !ok || !strings.Contains(dump, "goroutine") {
		t.Fatalf("record context = %+v, want a goroutine dump in it", records[0].Context)
	}
}

// TestRecordGoroutineDumpCountsTheGoroutines pins the count beside the dump: a
// truncated dump still has to say how many goroutines the daemon held.
func TestRecordGoroutineDumpCountsTheGoroutines(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()

	// Act.
	recordGoroutineDump(log, "daemon.cmd.sigquit", "dumping", nil)

	// Assert.
	count, ok := log.Records()[0].Context["goroutines"].(int)
	if !ok || count < 1 {
		t.Fatalf("record context = %+v, want a positive goroutine count", log.Records()[0].Context)
	}
}

// TestRecordGoroutineDumpKeepsTheCallersContext pins that the dump does not
// displace what the caller said: the bound that expired travels with it.
func TestRecordGoroutineDumpKeepsTheCallersContext(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()

	// Act.
	recordGoroutineDump(log, "daemon.cmd.boot", "stalled", dlog.Context{"bound_ms": int64(90000)})

	// Assert.
	if got := log.Records()[0].Context["bound_ms"]; got != int64(90000) {
		t.Fatalf("record context bound_ms = %v, want the caller's 90000", got)
	}
}
