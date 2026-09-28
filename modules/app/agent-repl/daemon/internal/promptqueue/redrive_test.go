package promptqueue

import (
	"context"
	"testing"

	"claude-repld/internal/dlog"
)

func TestIsRedriveReadsTheMark(t *testing.T) {
	tests := []struct {
		name string
		ctx  context.Context
		want bool
	}{
		{name: "an unmarked submission", ctx: context.Background(), want: false},
		{name: "a re-driven submission", ctx: WithRedrive(context.Background()), want: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := IsRedrive(tc.ctx)

			// Assert
			if got != tc.want {
				t.Fatalf("IsRedrive = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestRefusalLevelRecordsAtTheArmsLevelOrDebugForARedrive(t *testing.T) {
	tests := []struct {
		name      string
		ctx       context.Context
		wantLevel string
	}{
		{name: "a live caller's refusal keeps the arm's level", ctx: context.Background(), wantLevel: "warn"},
		{name: "a re-driven refusal is debug", ctx: WithRedrive(context.Background()), wantLevel: "debug"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			log := dlog.NewTestLogger()

			// Act
			refusalLevel(tc.ctx, log, log.Warn)("daemon.promptqueue.submit", "refused", nil)

			// Assert
			records := log.Records()
			if len(records) != 1 || records[0].Level != tc.wantLevel {
				t.Fatalf("records = %+v, want one at %s", records, tc.wantLevel)
			}
		})
	}
}
