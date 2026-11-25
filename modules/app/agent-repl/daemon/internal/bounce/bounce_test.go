package bounce

import (
	"errors"
	"fmt"
	"testing"
)

func TestGateNamesItself(t *testing.T) {
	tests := []struct {
		name string
		gate Gate
		want string
	}{
		{name: "the zero value is the freeness gate", gate: Gate(0), want: "freeness"},
		{name: "the dispatch-quiet gate", gate: GateDispatchQuiet, want: "dispatch_quiet"},
		{name: "a gate the vocabulary does not know", gate: Gate(99), want: "unknown"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the gate is the case's.

			// Act.
			got := tc.gate.String()

			// Assert.
			if got != tc.want {
				t.Fatalf("Gate(%d).String() = %q, want %q", tc.gate, got, tc.want)
			}
		})
	}
}

func TestAHandoffIsEmptyOnlyWhenItCarriesNothing(t *testing.T) {
	tests := []struct {
		name    string
		handoff Handoff
		want    bool
	}{
		{name: "nothing carried", handoff: Handoff{}, want: true},
		{name: "a queued act", handoff: Handoff{Acts: []HandoffAct{{Kind: "compact"}}}, want: false},
		{name: "a running cut", handoff: Handoff{Cut: &HandoffCut{Turn: "t"}}, want: false},
		{name: "a semantic head", handoff: Handoff{Head: "t"}, want: false},
		{name: "an interrupting status", handoff: Handoff{Interrupting: true}, want: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the handoff is the case's.

			// Act.
			got := tc.handoff.Empty()

			// Assert.
			if got != tc.want {
				t.Fatalf("Empty() = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestOutcomeOfClassifiesADoneError(t *testing.T) {
	tests := []struct {
		name string
		err  error
		want Outcome
	}{
		{name: "no error is a finished bounce", err: nil, want: OutcomeFinished},
		{name: "the unregistered sentinel", err: ErrUnregistered, want: OutcomeUnregistered},
		{name: "a wrapped unregistered sentinel", err: fmt.Errorf("queue: %w", ErrUnregistered), want: OutcomeUnregistered},
		{name: "the handed-across sentinel", err: ErrHandedAcross, want: OutcomeHandedAcross},
		{name: "a wrapped handed-across sentinel", err: fmt.Errorf("carry: %w", ErrHandedAcross), want: OutcomeHandedAcross},
		{name: "the deferred sentinel", err: ErrDeferred, want: OutcomeDeferred},
		{name: "a wrapped deferred sentinel", err: fmt.Errorf("relaunch: %w", ErrDeferred), want: OutcomeDeferred},
		{name: "any other error is a failure", err: errors.New("prelaunch refused"), want: OutcomeFailed},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the error is the case's.

			// Act.
			got := OutcomeOf(tc.err)

			// Assert.
			if got != tc.want {
				t.Fatalf("OutcomeOf(%v) = %d, want %d", tc.err, got, tc.want)
			}
		})
	}
}
