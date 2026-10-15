package promptqueue

import (
	"errors"
	"testing"

	"agent-repld/internal/bounce"
)

func TestEdgeSetRejudgesOnlyOnAnEdgeThatEndsWhatTheRunWasDeferredBehind(t *testing.T) {
	behindLease := bounce.ErrDeferredBehindLease
	behindWork := errDeferredRun
	tests := []struct {
		name   string
		edges  []freenessEdge
		runErr error
		want   bool
	}{
		{name: "no edge, deferred behind live work", runErr: behindWork, want: false},
		{name: "a work edge, deferred behind live work", edges: []freenessEdge{edgeWork}, runErr: behindWork, want: true},
		{name: "only a lease edge, deferred behind live work", edges: []freenessEdge{edgeLease}, runErr: behindWork, want: false},
		{name: "both edges, deferred behind live work", edges: []freenessEdge{edgeLease, edgeWork}, runErr: behindWork, want: true},
		{name: "no edge, deferred behind a lease", runErr: behindLease, want: false},
		{name: "a lease edge, deferred behind a lease", edges: []freenessEdge{edgeLease}, runErr: behindLease, want: true},
		{name: "a work edge, deferred behind a lease", edges: []freenessEdge{edgeWork}, runErr: behindLease, want: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			var set edgeSet
			for _, edge := range tc.edges {
				set.add(edge)
			}

			// Act
			got := set.rejudges(tc.runErr)

			// Assert
			if got != tc.want {
				t.Fatalf("rejudges = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestEdgeSetPanicsOnAnUnknownEdge(t *testing.T) {
	// Arrange
	var set edgeSet
	defer func() {
		// Assert
		if recover() == nil {
			t.Fatalf("add accepted an unknown edge")
		}
	}()

	// Act
	set.add(freenessEdge(99))
}

func TestErrDeferredBehindLeaseIsADeferral(t *testing.T) {
	// Act
	got := bounce.OutcomeOf(bounce.ErrDeferredBehindLease)

	// Assert
	if got != bounce.OutcomeDeferred || !errors.Is(bounce.ErrDeferredBehindLease, bounce.ErrDeferred) {
		t.Fatalf("OutcomeOf = %v, want OutcomeDeferred", got)
	}
}
