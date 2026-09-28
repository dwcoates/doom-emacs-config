package bounce

import "testing"

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
