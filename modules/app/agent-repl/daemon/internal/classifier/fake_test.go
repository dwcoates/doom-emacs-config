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
	if !got.Interject || !got.FastPath {
		t.Fatalf("verdict = %+v, want an interjecting fast-path verdict", got)
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
	if !got.Interject {
		t.Fatalf("verdict = %+v, want an interjecting verdict", got)
	}
	if got.FastPath {
		t.Fatal("the marker is not the explicit-interrupt fast path")
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
	if got.Interject {
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
