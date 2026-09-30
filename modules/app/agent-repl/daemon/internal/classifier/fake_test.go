package classifier

import (
	"context"
	"testing"
)

func TestFakeJudgeInterjectsOnTheExplicitFastPath(t *testing.T) {
	// Arrange
	j := NewFake()
	// Act
	got, err := j.Judge(context.Background(), "running", "abort")
	// Assert
	if err != nil {
		t.Fatalf("Judge: %v", err)
	}
	if got.Route != RouteInterrupt || !got.FastPath {
		t.Fatalf("verdict = %+v, want an interrupting fast-path verdict", got)
	}
}

func TestFakeJudgeInterjectsOnTheScriptedMarker(t *testing.T) {
	// Arrange
	j := NewFake()
	// Act
	got, err := j.Judge(context.Background(), "running", "please also [interject] here")
	// Assert
	if err != nil {
		t.Fatalf("Judge: %v", err)
	}
	if got.Route != RouteInterrupt {
		t.Fatalf("verdict = %+v, want an interrupting verdict", got)
	}
	if got.FastPath {
		t.Fatal("the marker is not the explicit-interrupt fast path")
	}
}

func TestFakeJudgeJoinsTheRunningTurnOnTheAfterToolCallMarker(t *testing.T) {
	// Arrange
	j := NewFake()
	// Act
	got, err := j.Judge(context.Background(), "running", "also cover the edge case [after-tool-call]")
	// Assert
	if err != nil {
		t.Fatalf("Judge: %v", err)
	}
	if got.Route != RouteAfterToolCall || got.FastPath {
		t.Fatalf("verdict = %+v, want an after-tool-call verdict off the marker", got)
	}
}

func TestFakeJudgeHoldsEveryOtherPrompt(t *testing.T) {
	// Arrange
	j := NewFake()
	// Act
	got, err := j.Judge(context.Background(), "running", "what does this function do?")
	// Assert
	if err != nil {
		t.Fatalf("Judge: %v", err)
	}
	if got.Route != RouteQueue {
		t.Fatalf("verdict = %+v, want a holding verdict", got)
	}
}

func TestFakeJudgeStatesAReasonOnEveryVerdict(t *testing.T) {
	// Arrange
	j := NewFake()
	// Act
	got, err := j.Judge(context.Background(), "running", "an ordinary follow-up")
	// Assert
	if err != nil {
		t.Fatalf("Judge: %v", err)
	}
	if got.Reason == "" {
		t.Fatal("a verdict must carry the evidence the tray shows")
	}
}
