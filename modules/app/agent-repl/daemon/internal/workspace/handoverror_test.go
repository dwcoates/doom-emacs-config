package workspace

import (
	"context"
	"errors"
	"fmt"
	"testing"

	"agent-repld/internal/bounce"
	"agent-repld/internal/bringup"
	"agent-repld/internal/rollout"
	"agent-repld/internal/shimclient"
)

func TestHandedOver(t *testing.T) {
	cases := []struct {
		name string
		err  error
		want bool
	}{
		{name: "a transferring_away refusal", err: &Refusal{Arm: ArmTransferringAway}, want: true},
		{name: "a not_yet_adopted refusal", err: &Refusal{Arm: ArmNotYetAdopted}, want: true},
		{name: "a bring-up this daemon did not serve", err: fmt.Errorf("start: %w", bringup.ErrNotServed), want: true},
		{name: "a supervisor standing down", err: fmt.Errorf("start: %w", shimclient.ErrStandingDown), want: true},
		{name: "a workspace not yet adopted", err: fmt.Errorf("own: %w", rollout.ErrNotYetAdopted), want: true},
		{name: "a workspace moved away", err: fmt.Errorf("bounce: %w", bounce.ErrMovedAway), want: true},
		{name: "a daemon exiting", err: fmt.Errorf("git: %w", context.Canceled), want: true},
		{name: "any other refusal", err: &Refusal{Arm: ArmUnknownRepository}, want: false},
		{name: "an internal failure", err: errors.New("fatal: worktree add failed"), want: false},
		{name: "a deadline", err: fmt.Errorf("git: %w", context.DeadlineExceeded), want: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the cases.

			// Act.
			got := HandedOver(tc.err)

			// Assert.
			if got != tc.want {
				t.Fatalf("HandedOver(%v) = %v, want %v", tc.err, got, tc.want)
			}
		})
	}
}
