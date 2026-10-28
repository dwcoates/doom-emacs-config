package workspace

import (
	"context"
	"slices"
	"testing"

	"agent-repld/internal/shimlife"
)

func TestAStartIsCountedByTheCauseItsCallerStated(t *testing.T) {
	tests := []struct {
		name string
		ctx  context.Context
		want string
	}{
		{name: "a stated cause", ctx: shimlife.WithStart(context.Background(), shimlife.StartPrompt), want: "w1 prompt ok"},
		{name: "no cause stated", ctx: context.Background(), want: "w1 unstated ok"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFleetFixture(t)
			ws := f.workspace("w1")

			// Act.
			if err := f.fleet.Start(tt.ctx, ws.ID); err != nil {
				t.Fatalf("Start: %v", err)
			}

			// Assert.
			if !slices.Equal(f.shimLife.starts, []string{tt.want}) {
				t.Fatalf("starts = %v, want [%s]", f.shimLife.starts, tt.want)
			}
		})
	}
}

func TestAStartOfALiveSessionIsNotCounted(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	if len(f.shimLife.starts) != 1 {
		t.Fatalf("starts = %v, want the first start alone", f.shimLife.starts)
	}
}

func TestAStandDownIsCountedByTheReasonItsCallerStated(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Act.
	if err := f.fleet.Stop(shimlife.WithStandDown(context.Background(), shimlife.StandDownKill), ws.ID, true); err != nil {
		t.Fatalf("Stop: %v", err)
	}

	// Assert.
	if !slices.Equal(f.shimLife.standDowns, []string{"w1 kill"}) {
		t.Fatalf("stand-downs = %v, want the kill", f.shimLife.standDowns)
	}
}

func TestAColdGatesShimStandDownIsCountedAsTheGates(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := resumable(f)
	f.client.response = coldResponse()

	// Act.
	if err := f.fleet.Start(context.Background(), ws); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	if !slices.Contains(f.shimLife.standDowns, "w1 cold_gate") {
		t.Fatalf("stand-downs = %v, want the gate's", f.shimLife.standDowns)
	}
}

func TestARestartStatesItsStartsCause(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.hasSession = false

	// Act.
	if err := f.verbs.Restart(context.Background(), "w1"); err != nil {
		t.Fatalf("Restart: %v", err)
	}

	// Assert.
	if !slices.Equal(f.fleet.startCauses, []shimlife.StartCause{shimlife.StartRestart}) {
		t.Fatalf("start causes = %v, want the restart's", f.fleet.startCauses)
	}
}

func TestAnAnsweredGateIsCountedByHowItEnded(t *testing.T) {
	tests := []struct {
		name string
		err  error
		want string
	}{
		{name: "the session came up", want: "w1 pay resumed"},
		{name: "the re-open failed", err: errFake, want: "w1 pay failed"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			standingGate(f)
			f.fleet.resumeErr = tt.err

			// Act.
			if err := f.verbs.AnswerColdGate(context.Background(), "w1", payAnswer()); err != nil {
				t.Fatalf("AnswerColdGate: %v", err)
			}

			// Assert.
			if !slices.Equal(f.gateAnswers.answers, []string{tt.want}) {
				t.Fatalf("answers = %v, want [%s]", f.gateAnswers.answers, tt.want)
			}
		})
	}
}
