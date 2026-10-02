package wsm

import (
	"errors"
	"strings"
	"testing"
)

func TestLayoutErrorNamesTheDirection(t *testing.T) {
	tests := []struct {
		name string
		file int
		want string
	}{
		{name: "newer file", file: 9, want: "newer than"},
		{name: "older file", file: 0, want: "older than"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			err := &LayoutError{Path: "/tmp/wsm.db", File: tc.file, Binary: 1}

			// Act
			got := err.Error()

			// Assert
			if !strings.Contains(got, tc.want) {
				t.Fatalf("Error() = %q, want it to contain %q", got, tc.want)
			}
		})
	}
}

func TestDecodeErrorNamesTheColumnWhenItHasOne(t *testing.T) {
	// Arrange
	err := &DecodeError{Table: "held_prompts", Row: "turn-1", Field: "hold_kind", Err: errors.New("bad")}

	// Act
	got := err.Error()

	// Assert
	if !strings.Contains(got, "held_prompts") || !strings.Contains(got, "turn-1") || !strings.Contains(got, "hold_kind") {
		t.Fatalf("Error() = %q, want the table, row and column named", got)
	}
}

func TestDecodeErrorNamesOnlyTheRowWithoutAColumn(t *testing.T) {
	// Arrange
	err := &DecodeError{Table: "layout", Row: "1", Err: errors.New("bad")}

	// Act
	got := err.Error()

	// Assert
	if strings.Contains(got, "field") {
		t.Fatalf("Error() = %q, want no column named", got)
	}
}

func TestDecodeErrorUnwrapsItsCause(t *testing.T) {
	// Arrange
	cause := errors.New("root cause")
	err := &DecodeError{Table: "turns", Row: "t", Err: cause}

	// Act / Assert
	if !errors.Is(err, cause) {
		t.Fatalf("errors.Is(err, cause) = false, want the cause reachable")
	}
}

func TestLeaseHeldErrorNamesTheStandingHolder(t *testing.T) {
	// Arrange
	err := &LeaseHeldError{Workspace: "ws-1", Lease: "lease-1", Holder: HolderMerge, Policy: PolicyRefuse}

	// Act
	got := err.Error()

	// Assert
	if !strings.Contains(got, "merge") || !strings.Contains(got, "refuse") || !strings.Contains(got, "lease-1") {
		t.Fatalf("Error() = %q, want the holder, policy and lease named", got)
	}
}

func TestServingErrorDistinguishesAnUnservedWorkspace(t *testing.T) {
	// Arrange
	err := &ServingError{Workspace: "ws-1", Claimant: "instance-1"}

	// Act
	got := err.Error()

	// Assert
	if !strings.Contains(got, "no instance") {
		t.Fatalf("Error() = %q, want it to say no instance serves the workspace", got)
	}
}

func TestMergeQueuedErrorNamesThePlaceAlreadyHeld(t *testing.T) {
	// Arrange
	err := &MergeQueuedError{Repo: "/repo", Workspace: "ws-1", Position: 3, State: MergeAdmitted}

	// Act
	got := err.Error()

	// Assert
	if !strings.Contains(got, "admitted") || !strings.Contains(got, "position 3") {
		t.Fatalf("Error() = %q, want the state and position named", got)
	}
}

func TestLeaseHolderStringNamesEveryArm(t *testing.T) {
	tests := []struct {
		holder LeaseHolder
		want   string
	}{
		{holder: HolderMerge, want: "merge"},
		{holder: HolderRestart, want: "restart"},
		{holder: HolderDrain, want: "drain"},
		{holder: HolderHibernate, want: "hibernate"},
	}
	for _, tc := range tests {
		t.Run(tc.want, func(t *testing.T) {
			// Arrange / Act / Assert
			if got := tc.holder.String(); got != tc.want {
				t.Fatalf("String() = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestLeasePolicyStringNamesEveryArm(t *testing.T) {
	tests := []struct {
		policy LeasePolicy
		want   string
	}{
		{policy: PolicyRefuse, want: "refuse"},
		{policy: PolicyHold, want: "hold"},
		{policy: PolicyParked, want: "parked"},
	}
	for _, tc := range tests {
		t.Run(tc.want, func(t *testing.T) {
			// Arrange / Act / Assert
			if got := tc.policy.String(); got != tc.want {
				t.Fatalf("String() = %q, want %q", got, tc.want)
			}
		})
	}
}

// TestHoldKindStoredValuesAreStable pins the integers held_prompts.hold_kind
// persists: a hold written by an older build must decode as the same kind, so
// a rename of an identifier may never move its value.
func TestHoldKindStoredValuesAreStable(t *testing.T) {
	tests := []struct {
		name string
		hold HoldKind
		want int
	}{
		{name: "shutdown", hold: HoldShutdown, want: 0},
		{name: "reconnect (stored as session_starting before the rename)", hold: HoldReconnect, want: 1},
		{name: "build refresh", hold: HoldBuildRefresh, want: 2},
		{name: "merge", hold: HoldMerge, want: 3},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange / Act / Assert
			if got := int(tc.hold); got != tc.want {
				t.Fatalf("stored value = %d, want %d", got, tc.want)
			}
		})
	}
}

func TestHoldKindStringNamesEveryArm(t *testing.T) {
	tests := []struct {
		hold HoldKind
		want string
	}{
		{hold: HoldShutdown, want: "shutdown"},
		{hold: HoldReconnect, want: "reconnect"},
		{hold: HoldBuildRefresh, want: "build_refresh"},
	}
	for _, tc := range tests {
		t.Run(tc.want, func(t *testing.T) {
			// Arrange / Act / Assert
			if got := tc.hold.String(); got != tc.want {
				t.Fatalf("String() = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestClassificationArmStringNamesEveryArm(t *testing.T) {
	tests := []struct {
		arm  ClassificationArm
		want string
	}{
		{arm: ArmClassifying, want: "classifying"},
		{arm: ArmInterject, want: "interject"},
		{arm: ArmHoldForTurnEnd, want: "hold_for_turn_end"},
		{arm: ArmUninterruptibleTurn, want: "uninterruptible_turn"},
		{arm: ArmClassificationError, want: "classification_error"},
	}
	for _, tc := range tests {
		t.Run(tc.want, func(t *testing.T) {
			// Arrange / Act / Assert
			if got := tc.arm.String(); got != tc.want {
				t.Fatalf("String() = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestUndeclaredArmsAreInvalid(t *testing.T) {
	tests := []struct {
		name string
		got  bool
	}{
		{name: "lease holder", got: LeaseHolder(99).valid()},
		{name: "lease policy", got: LeasePolicy(99).valid()},
		{name: "priority", got: Priority(99).valid()},
		{name: "hold kind", got: HoldKind(99).valid()},
		{name: "turn close", got: TurnClose(99).valid()},
		{name: "classification arm", got: ClassificationArm(99).valid()},
		{name: "merge queue state", got: MergeQueueState(99).valid()},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange / Act / Assert
			if tc.got {
				t.Fatalf("an out-of-range %s reported itself valid", tc.name)
			}
		})
	}
}
