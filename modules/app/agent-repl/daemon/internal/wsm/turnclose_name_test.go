package wsm

import (
	"testing"
)

func TestTurnCloseStringNamesEveryDeclaredClose(t *testing.T) {
	tests := []struct {
		how  TurnClose
		want string
	}{
		{how: CloseCompleted, want: "completed"},
		{how: CloseFailed, want: "failed"},
		{how: CloseKilled, want: "killed"},
		{how: CloseOrphaned, want: "orphaned"},
		{how: CloseAgentDied, want: "agent_died"},
		{how: CloseFolded, want: "folded"},
		{how: CloseWithdrawn, want: "withdrawn"},
	}
	for _, tt := range tests {
		t.Run(tt.want, func(t *testing.T) {
			// Act
			got := tt.how.String()
			// Assert
			if got != tt.want {
				t.Fatalf("String() = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestTurnCloseStringNamesAnUndeclaredCloseByItsNumber(t *testing.T) {
	// Act
	got := TurnClose(42).String()
	// Assert
	if got != "close(42)" {
		t.Fatalf("String() = %q, want close(42)", got)
	}
}
