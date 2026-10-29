package workspace

import (
	"context"
	"errors"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
)

var visitStart = time.Unix(1_800_000_000, 0)

func TestSettingsVisitsLeftForTakesEveryVisitButTheCurrent(t *testing.T) {
	// Arrange
	v := NewSettingsVisits()
	v.Note("w1", visitStart)
	v.Note("w2", visitStart.Add(time.Minute))

	// Act
	left := v.LeftFor("w2")

	// Assert
	if len(left) != 1 || !left["w1"].Equal(visitStart) {
		t.Fatalf("left = %v, want only w1's visit", left)
	}
}

func TestSettingsVisitsATakenVisitIsGone(t *testing.T) {
	// Arrange
	v := NewSettingsVisits()
	v.Note("w1", visitStart)
	v.LeftFor("w2")

	// Act
	left := v.LeftFor("w2")

	// Assert
	if len(left) != 0 {
		t.Fatalf("left = %v, want nothing the second time", left)
	}
}

func TestSettingsVisitsTheCurrentsVisitStands(t *testing.T) {
	// Arrange
	v := NewSettingsVisits()
	v.Note("w1", visitStart)
	v.LeftFor("w1")

	// Act
	left := v.LeftFor("w2")

	// Assert
	if len(left) != 1 {
		t.Fatalf("left = %v, want w1's visit kept while it was current", left)
	}
}

func TestSettingsVisitsALaterStartReplacesTheInstant(t *testing.T) {
	// Arrange
	v := NewSettingsVisits()
	v.Note("w1", visitStart)
	v.Note("w1", visitStart.Add(time.Hour))

	// Act
	left := v.LeftFor("w2")

	// Assert
	if !left["w1"].Equal(visitStart.Add(time.Hour)) {
		t.Fatalf("left = %v, want the later start", left)
	}
}

func TestASwitchAwayStandsDownTheVisitItLeft(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.workspace("w2", t.TempDir())
	if err := f.verbs.Select(context.Background(), "w1", RecordSelection); err != nil {
		t.Fatalf("Select w1: %v", err)
	}
	f.visits.Note("w1", visitStart)

	// Act
	if err := f.verbs.Select(context.Background(), "w2", RecordSelection); err != nil {
		t.Fatalf("Select w2: %v", err)
	}

	// Assert
	if len(f.visitDrain.visits) != 1 || f.visitDrain.visits[0].WS != "w1" || !f.visitDrain.visits[0].Since.Equal(visitStart) {
		t.Fatalf("visit stand-downs = %v, want w1's since its start", f.visitDrain.visits)
	}
}

func TestAReselectStandsNothingDown(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	if err := f.verbs.Select(context.Background(), "w1", RecordSelection); err != nil {
		t.Fatalf("Select: %v", err)
	}
	f.visits.Note("w1", visitStart)

	// Act
	if err := f.verbs.Select(context.Background(), "w1", RecordSelection); err != nil {
		t.Fatalf("Select again: %v", err)
	}

	// Assert
	if len(f.visitDrain.visits) != 0 {
		t.Fatalf("visit stand-downs = %v, want none for a re-selection", f.visitDrain.visits)
	}
}

func TestAnEffortPickWithNoShimKeepsThePickAndStartsTheShim(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.cards.efforts, f.cards.hasEfforts = []conversationv1.AgentEffortLevel{effortHigh}, true
	f.fleet.effortErr = &ShimRefusal{Verb: "SetSessionEffort", Arm: ArmShimNoSession}

	// Act
	err := f.verbs.SetEffort(context.Background(), "w1", effortHigh)

	// Assert
	if err != nil || f.topbarPicked != effortHigh || len(f.queue.settingsRevivals) != 1 || f.queue.settingsRevivals[0] != "w1" {
		t.Fatalf("err %v, picked %v, revivals %v: want the pick kept and one settings start", err, f.topbarPicked, f.queue.settingsRevivals)
	}
}

func TestAnEffortPickThatCannotStartTheShimFails(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.cards.efforts, f.cards.hasEfforts = []conversationv1.AgentEffortLevel{effortHigh}, true
	f.fleet.effortErr = &ShimRefusal{Verb: "SetSessionEffort", Arm: ArmShimNoSession}
	f.queue.reviveErr = errors.New("no revival wired")

	// Act
	err := f.verbs.SetEffort(context.Background(), "w1", effortHigh)

	// Assert
	if err == nil {
		t.Fatal("SetEffort answered success though the shim could not be started")
	}
}
