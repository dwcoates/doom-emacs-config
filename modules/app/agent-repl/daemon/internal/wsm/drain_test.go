package wsm

import (
	"context"
	"errors"
	"testing"
	"time"
)

func TestPutDrainScheduleRoundTrips(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	deadline := instant.Add(10 * time.Minute)

	// Act
	if err := s.PutDrainSchedule(context.Background(), DrainSchedule{Reason: "deploy", Deadline: deadline, SetAt: instant}); err != nil {
		t.Fatalf("PutDrainSchedule: %v", err)
	}
	got, err := s.DrainSchedule(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("DrainSchedule: %v", err)
	}
	if got == nil || got.Reason != "deploy" || !got.Deadline.Equal(deadline) || !got.SetAt.Equal(instant) {
		t.Fatalf("schedule = %+v, want the recorded facts", got)
	}
}

func TestPutDrainScheduleReplacesTheOneInForce(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	if err := s.PutDrainSchedule(context.Background(), DrainSchedule{Reason: "deploy", Deadline: instant, SetAt: instant}); err != nil {
		t.Fatalf("PutDrainSchedule: %v", err)
	}

	// Act
	if err := s.PutDrainSchedule(context.Background(), DrainSchedule{Reason: "operator", Deadline: instant, SetAt: instant}); err != nil {
		t.Fatalf("PutDrainSchedule: %v", err)
	}

	// Assert
	got, err := s.DrainSchedule(context.Background())
	if err != nil {
		t.Fatalf("DrainSchedule: %v", err)
	}
	if got == nil || got.Reason != "operator" {
		t.Fatalf("schedule = %+v, want the replacement", got)
	}
	if n := scalar[int](t, s, `SELECT count(*) FROM drain_schedule`); n != 1 {
		t.Fatalf("%d schedules are in force, want exactly 1", n)
	}
}

func TestPutDrainScheduleRefusesAnEmptyReason(t *testing.T) {
	// Arrange
	s, log := testStore(t)

	// Act
	err := s.PutDrainSchedule(context.Background(), DrainSchedule{Deadline: instant, SetAt: instant})

	// Assert
	if err == nil {
		t.Fatalf("PutDrainSchedule with no reason succeeded")
	}
	if !loggedOperation(log, "daemon.wsm.put_drain_schedule", "error") {
		t.Fatalf("the refusal was not logged at error: %v", log.Records())
	}
}

func TestPutDrainScheduleRefusesAMissingDeadline(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	err := s.PutDrainSchedule(context.Background(), DrainSchedule{Reason: "deploy", SetAt: instant})

	// Assert
	if err == nil {
		t.Fatalf("PutDrainSchedule with no deadline succeeded")
	}
}

func TestClearDrainScheduleCancelsTheOneInForce(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	if err := s.PutDrainSchedule(context.Background(), DrainSchedule{Reason: "deploy", Deadline: instant, SetAt: instant}); err != nil {
		t.Fatalf("PutDrainSchedule: %v", err)
	}

	// Act
	if err := s.ClearDrainSchedule(context.Background()); err != nil {
		t.Fatalf("ClearDrainSchedule: %v", err)
	}

	// Assert
	got, err := s.DrainSchedule(context.Background())
	if err != nil {
		t.Fatalf("DrainSchedule: %v", err)
	}
	if got != nil {
		t.Fatalf("schedule = %+v after clearing, want none", got)
	}
}

func TestClearDrainScheduleIsIdempotent(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	err := s.ClearDrainSchedule(context.Background())

	// Assert — the post-state the caller asked for is the one that holds.
	if err != nil {
		t.Fatalf("ClearDrainSchedule with none in force: %v", err)
	}
}

func TestDrainScheduleReportsNoneWhenUnset(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	got, err := s.DrainSchedule(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("DrainSchedule: %v", err)
	}
	if got != nil {
		t.Fatalf("schedule = %+v, want none", got)
	}
}

func TestDrainScheduleFailsWholeOnACorruptReason(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	if err := s.PutDrainSchedule(context.Background(), DrainSchedule{Reason: "deploy", Deadline: instant, SetAt: instant}); err != nil {
		t.Fatalf("PutDrainSchedule: %v", err)
	}
	corrupt(t, s, `UPDATE drain_schedule SET reason = '' WHERE id = 1`)

	// Act
	got, err := s.DrainSchedule(context.Background())

	// Assert
	var refusal *DecodeError
	if !errors.As(err, &refusal) || refusal.Table != "drain_schedule" || refusal.Field != "reason" {
		t.Fatalf("DrainSchedule = %v, want a *DecodeError naming drain_schedule.reason", err)
	}
	if got != nil {
		t.Fatalf("schedule = %+v alongside the refusal, want none", got)
	}
	if !loggedOperation(log, "daemon.wsm.drain_schedule", "error") {
		t.Fatalf("the decode failure was not logged at error: %v", log.Records())
	}
}
