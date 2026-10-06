package main

import (
	"strings"
	"testing"
	"time"

	"claude-repld/internal/dlog"
)

func TestTeardownReportNamesEveryStepInOrder(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	c := newTeardownClock(log)
	c.step("join_loops", func() {})
	c.step("state_close", func() {})

	// Act
	c.report()

	// Assert
	records := log.Records()
	if len(records) != 1 || records[0].Operation != "daemon.cmd.teardown" || records[0].Level != "info" {
		t.Fatalf("records = %+v, want one INFO daemon.cmd.teardown", records)
	}
	steps, _ := records[0].Context["steps"].(string)
	if !strings.HasPrefix(steps, "join_loops=") || !strings.Contains(steps, " state_close=") {
		t.Fatalf("steps = %q, want join_loops then state_close", steps)
	}
}

func TestTeardownReportCarriesTheTimeSinceServingStopped(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	c := newTeardownClock(log)
	c.begin(time.Now().Add(-time.Second))

	// Act
	c.report()

	// Assert
	got, ok := log.Records()[0].Context["since_serving_stopped_ms"].(int64)
	if !ok || got < 1000 {
		t.Fatalf("since_serving_stopped_ms = %v, want at least 1000", log.Records()[0].Context["since_serving_stopped_ms"])
	}
}

func TestTeardownBeginKeepsTheFirstMoment(t *testing.T) {
	// Arrange
	c := newTeardownClock(dlog.NewTestLogger())
	first := time.Now().Add(-time.Second)
	c.begin(first)

	// Act
	c.begin(time.Now())

	// Assert
	if !c.began.Equal(first) {
		t.Fatalf("began = %v, want the first begin %v", c.began, first)
	}
}

func TestTeardownMarkSinceBeginTimesFromTheStop(t *testing.T) {
	// Arrange
	c := newTeardownClock(dlog.NewTestLogger())
	c.begin(time.Now().Add(-time.Second))

	// Act
	c.markSinceBegin("serve_stop")

	// Assert
	if len(c.steps) != 1 || c.steps[0].name != "serve_stop" || c.steps[0].took < time.Second {
		t.Fatalf("steps = %+v, want serve_stop of at least 1s", c.steps)
	}
}
