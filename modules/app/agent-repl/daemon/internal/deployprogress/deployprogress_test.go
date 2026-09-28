package deployprogress

import "testing"

func TestPhaseSpellsItselfForTheRecords(t *testing.T) {
	tests := []struct {
		name  string
		phase Phase
		want  string
	}{
		{"the build", Building, "building"},
		{"the install", Installing, "installing"},
		{"the service restarts", RestartingServices, "restarting_services"},
		{"the handover", HandingOver, "handing_over"},
		{"the end", Updated, "updated"},
		{"the zero value", Phase(0), "unknown"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			phase := tt.phase

			// Act
			got := phase.String()

			// Assert
			if got != tt.want {
				t.Fatalf("String() = %q, want %q", got, tt.want)
			}
		})
	}
}
