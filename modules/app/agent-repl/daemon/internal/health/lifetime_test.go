package health

import (
	"testing"
)

func TestEveryLifetimeIsEitherMomentaryOrStandingOnAnEdge(t *testing.T) {
	for kind, lifetime := range faultLifetimes {
		t.Run(kind, func(t *testing.T) {
			// Arrange, Act
			momentaryWithEdges := lifetime.Momentary && len(lifetime.Edges) > 0
			standingWithout := !lifetime.Momentary && len(lifetime.Edges) == 0

			// Assert
			if momentaryWithEdges {
				t.Fatalf("%q is momentary and names edges %v; a momentary kind closes as it is recorded", kind, lifetime.Edges)
			}
			if standingWithout {
				t.Fatalf("%q stands with no recovery edge; nothing would ever close it", kind)
			}
		})
	}
}

func TestClosesOnAnswersTheMomentaryEdgeForAMomentaryKind(t *testing.T) {
	// Arrange, Act
	got := ClosesOn(KindBounceDisposition, EdgeRecorded)

	// Assert
	if !got {
		t.Fatalf("ClosesOn(%q, recorded) = false, want true", KindBounceDisposition)
	}
}

func TestClosesOnAnswersNothingForAnUndeclaredKind(t *testing.T) {
	// Arrange, Act
	got := ClosesOn("no_such_kind", EdgeHealthyAttach)

	// Assert
	if got {
		t.Fatalf("ClosesOn(undeclared kind) = true, want false")
	}
}

func TestAShimStoodDownClosesOnlyTheShimsOwnVerdict(t *testing.T) {
	tests := []struct {
		kind string
		want bool
	}{
		{kind: KindNetworkUnreachable, want: true},
		{kind: KindShimReported, want: true},
		{kind: KindShimDied, want: false},
		{kind: KindResumeFailed, want: false},
		{kind: KindColdGateReopenFailed, want: false},
	}
	for _, tt := range tests {
		t.Run(tt.kind, func(t *testing.T) {
			// Arrange, Act
			got := ClosesOn(tt.kind, EdgeShimStoodDown)

			// Assert
			if got != tt.want {
				t.Fatalf("ClosesOn(%q, shim_stood_down) = %v, want %v", tt.kind, got, tt.want)
			}
		})
	}
}
