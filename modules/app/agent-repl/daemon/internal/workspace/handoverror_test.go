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

func TestStandingRefusal(t *testing.T) {
	cases := []struct {
		name         string
		err          error
		wantStanding Standing
		wantRefused  bool
	}{
		{name: "a transferring_away refusal", err: fmt.Errorf("refresh: %w", &Refusal{Arm: ArmTransferringAway}), wantStanding: StandingTransferringAway, wantRefused: true},
		{name: "a not_yet_adopted refusal", err: fmt.Errorf("refresh: %w", &Refusal{Arm: ArmNotYetAdopted}), wantStanding: StandingNotYetAdopted, wantRefused: true},
		{name: "any other refusal", err: &Refusal{Arm: ArmUnknownWorkspace}, wantStanding: StandingOwned},
		{name: "a daemon exiting", err: fmt.Errorf("git: %w", context.Canceled), wantStanding: StandingOwned},
		{name: "a workspace not yet adopted that no verb refused", err: fmt.Errorf("own: %w", rollout.ErrNotYetAdopted), wantStanding: StandingOwned},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the cases.

			// Act.
			standing, refused := StandingRefusal(tc.err)

			// Assert.
			if standing != tc.wantStanding || refused != tc.wantRefused {
				t.Fatalf("StandingRefusal(%v) = (%v, %v), want (%v, %v)", tc.err, standing, refused, tc.wantStanding, tc.wantRefused)
			}
		})
	}
}
