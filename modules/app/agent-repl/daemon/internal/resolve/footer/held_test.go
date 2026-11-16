package footer

import "testing"

func TestAHeldMergesStepIsMarkedHeld(t *testing.T) {
	tests := []struct {
		name string
		held bool
	}{
		{name: "a held merge's step carries the held mark", held: true},
		{name: "a running merge's step carries none"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)

			// Act
			h.r.SetMerge(testWS, MergeFacts{State: "merging", Step: StepTesting, Held: tt.held})

			// Assert
			merging := h.view(t).GetStrip().GetStatus().GetMerging()
			if merging.GetTesting() == nil || (merging.GetHeld() != nil) != tt.held {
				t.Fatalf("merging = %+v, want the testing step held %v", merging, tt.held)
			}
		})
	}
}
