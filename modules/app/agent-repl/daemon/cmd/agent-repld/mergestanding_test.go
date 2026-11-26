package main

import (
	"context"
	"errors"
	"fmt"
	"testing"

	"agent-repld/internal/ids"
	"agent-repld/internal/merge"
	"agent-repld/internal/workspace"
)

// stubOwnership answers one standing, or an error, for every workspace.
type stubOwnership struct {
	standing workspace.Standing
	err      error
}

func (o stubOwnership) Standing(context.Context, ids.WorkspaceID) (workspace.Standing, error) {
	return o.standing, o.err
}

func (o stubOwnership) AwaitAdopted(context.Context, ids.WorkspaceID) (workspace.Standing, error) {
	return o.standing, o.err
}

func (stubOwnership) Created(ids.WorkspaceID) {}

func TestTheMergeQueueReadsTheStandingTheVerbsRefuseOn(t *testing.T) {
	tests := []struct {
		name      string
		ownership stubOwnership
		want      merge.ServingStanding
		wantErr   bool
	}{
		{name: "owned is served here", ownership: stubOwnership{standing: workspace.StandingOwned}, want: merge.ServedHere},
		{name: "transferring_away", ownership: stubOwnership{standing: workspace.StandingTransferringAway}, want: merge.TransferringAway},
		{name: "not_yet_adopted", ownership: stubOwnership{standing: workspace.StandingNotYetAdopted}, want: merge.NotYetAdopted},
		{name: "a standing the ownership cannot read is answered", ownership: stubOwnership{err: errors.New("unrecognized standing")}, wantErr: true},
		{name: "a standing with no merge queue spelling is answered", ownership: stubOwnership{standing: workspace.Standing(99)}, wantErr: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s := mergeStanding{ownership: tt.ownership}

			// Act
			got, err := s.ServingStanding("ws-1")

			// Assert
			if (err != nil) != tt.wantErr || (!tt.wantErr && got != tt.want) {
				t.Fatalf("ServingStanding = (%v, %v), want (%v, error %v)", got, err, tt.want, tt.wantErr)
			}
		})
	}
}

func TestMergeStandingRefusalReadsTheVerbsTypedArms(t *testing.T) {
	tests := []struct {
		name        string
		err         error
		want        merge.ServingStanding
		wantRefused bool
	}{
		{name: "a transferring_away refusal", err: fmt.Errorf("refresh: %w", &workspace.Refusal{Arm: workspace.ArmTransferringAway}), want: merge.TransferringAway, wantRefused: true},
		{name: "a not_yet_adopted refusal", err: fmt.Errorf("refresh: %w", &workspace.Refusal{Arm: workspace.ArmNotYetAdopted}), want: merge.NotYetAdopted, wantRefused: true},
		{name: "any other refusal", err: &workspace.Refusal{Arm: workspace.ArmUnknownWorkspace}, want: merge.ServedHere},
		{name: "a cancelled context", err: fmt.Errorf("git: %w", context.Canceled), want: merge.ServedHere},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: the cases.

			// Act
			got, refused := mergeStandingRefusal(tt.err)

			// Assert
			if got != tt.want || refused != tt.wantRefused {
				t.Fatalf("mergeStandingRefusal(%v) = (%v, %v), want (%v, %v)", tt.err, got, refused, tt.want, tt.wantRefused)
			}
		})
	}
}
