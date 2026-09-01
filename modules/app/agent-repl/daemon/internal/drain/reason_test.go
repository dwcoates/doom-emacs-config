package drain

import (
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"claude-repld/internal/wsm"
)

func TestEncodeReasonRoundTripsTheOperatorNote(t *testing.T) {
	// Arrange
	reason := &agentreplv1.DrainReason{
		Kind: &agentreplv1.DrainReason_Operator{
			Operator: &agentreplv1.DrainReasonOperator{Note: "swapping the disk"},
		},
	}

	// Act
	encoded, err := EncodeReason(reason)
	if err != nil {
		t.Fatalf("EncodeReason: %v", err)
	}
	decoded, err := DecodeReason(encoded)

	// Assert
	if err != nil {
		t.Fatalf("DecodeReason: %v", err)
	}
	if decoded.GetOperator().GetNote() != "swapping the disk" {
		t.Fatalf("note = %q, want the operator's own sentence back", decoded.GetOperator().GetNote())
	}
}

func TestEncodeReasonRefusesAnArmlessReason(t *testing.T) {
	// Arrange
	reason := &agentreplv1.DrainReason{}

	// Act
	_, err := EncodeReason(reason)

	// Assert
	if err == nil {
		t.Fatalf("EncodeReason accepted a reason with no arm")
	}
}

func TestEncodeReasonRefusesABlankOperatorNote(t *testing.T) {
	// Arrange
	reason := &agentreplv1.DrainReason{
		Kind: &agentreplv1.DrainReason_Operator{Operator: &agentreplv1.DrainReasonOperator{}},
	}

	// Act
	_, err := EncodeReason(reason)

	// Assert
	if err == nil {
		t.Fatalf("EncodeReason accepted a blank operator note")
	}
}

func TestDecodeReasonRefusesAStoredRowThatNamesNoArm(t *testing.T) {
	// Arrange
	stored := "{}"

	// Act
	_, err := DecodeReason(stored)

	// Assert
	if err == nil {
		t.Fatalf("DecodeReason accepted a stored reason naming no arm")
	}
}

func TestScheduleIDIsStableAcrossReadsOfTheSameSchedule(t *testing.T) {
	// Arrange
	s := wsm.DrainSchedule{Reason: "{}", Deadline: instant.Add(time.Hour), SetAt: instant}

	// Act
	first, second := ScheduleID(s), ScheduleID(s)

	// Assert
	if first != second || first == "" {
		t.Fatalf("schedule id = %q then %q, want one stable non-empty id", first, second)
	}
}

func TestScheduleIDDistinguishesTwoSchedules(t *testing.T) {
	// Arrange
	earlier := wsm.DrainSchedule{SetAt: instant}
	later := wsm.DrainSchedule{SetAt: instant.Add(time.Second)}

	// Act
	got := ScheduleID(earlier) == ScheduleID(later)

	// Assert
	if got {
		t.Fatalf("two schedules put in force at different instants share an id")
	}
}
