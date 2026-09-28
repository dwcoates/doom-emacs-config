package ladder

import (
	"slices"
	"sort"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/vocab"
)

// probeOf answers every claim in claims and declines every other one, drawing
// each claim as its own name.
func probeOf(claims ...Claim) func(Claim) (string, bool) {
	return func(c Claim) (string, bool) {
		return string(c), slices.Contains(claims, c)
	}
}

func idleDrawing() string { return string(Idle) }

func TestResolveTakesTheStrongestClaimTheFactsMake(t *testing.T) {
	cases := []struct {
		name   string
		merge  string
		parked bool
		claims []Claim
		want   Claim
	}{
		{name: "nothing claims, so the walk ends at idle", want: Idle},
		{name: "a merge in flight outranks a broken route", merge: "merging", claims: []Claim{Merging, Disconnected}, want: Merging},
		{name: "a merge stopped on a conflict outranks a broken route", merge: "conflict", claims: []Claim{MergeConflict, Disconnected}, want: MergeConflict},
		{name: "a broken route outranks a failed merge", merge: "failed", claims: []Claim{MergeFailed, Disconnected}, want: Disconnected},
		{name: "a broken route outranks a landed merge", merge: "merged", claims: []Claim{Merged, Disconnected}, want: Disconnected},
		{name: "a failed merge outranks a vendor block", merge: "failed", claims: []Claim{MergeFailed, Blocked}, want: MergeFailed},
		{name: "a closing refusal outranks a failed merge", merge: "failed", claims: []Claim{MergeFailed, Closing}, want: Closing},
		{name: "a vendor block outranks a permission ask", claims: []Claim{Blocked, Waiting}, want: Blocked},
		{name: "a permission ask outranks a turn in flight", claims: []Claim{Waiting, Thinking}, want: Waiting},
		{name: "a turn in flight outranks idle", claims: []Claim{Thinking}, want: Thinking},
		{name: "a parked session skips the link rung", parked: true, claims: []Claim{Disconnected}, want: Idle},
		{name: "a parked session still stands on its merge", merge: "failed", parked: true, claims: []Claim{MergeFailed, Disconnected}, want: MergeFailed},
		{name: "a merge rung the merge state does not claim is closed", merge: "", claims: []Claim{Merging}, want: Idle},
		{name: "a merge rung other than the standing one is closed", merge: "merged", claims: []Claim{MergeFailed}, want: Idle},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			probe := probeOf(tc.claims...)

			// Act
			got, drawn := Resolve(tc.merge, tc.parked, probe, idleDrawing)

			// Assert
			if got != tc.want || drawn != string(tc.want) {
				t.Fatalf("Resolve = (%q, %q), want %q", got, drawn, tc.want)
			}
		})
	}
}

func TestMergeClaimPlacesEveryMergeState(t *testing.T) {
	cases := []struct {
		state string
		want  Claim
	}{
		{state: "", want: ""},
		{state: "none", want: ""},
		{state: "enqueuing", want: Merging},
		{state: "queued", want: Merging},
		{state: "merging", want: Merging},
		{state: "conflict", want: MergeConflict},
		{state: "parked", want: MergeConflict},
		{state: "failed", want: MergeFailed},
		{state: "merged", want: Merged},
		{state: "a state this build does not name", want: Merging},
	}
	for _, tc := range cases {
		t.Run(tc.state, func(t *testing.T) {
			// Act
			got := MergeClaim(tc.state)

			// Assert
			if got != tc.want {
				t.Fatalf("MergeClaim(%q) = %q, want %q", tc.state, got, tc.want)
			}
		})
	}
}

func TestOrderEndsAtIdle(t *testing.T) {
	// Act
	last := Order[len(Order)-1]

	// Assert
	if last != Idle {
		t.Fatalf("the ladder ends at %q, want idle: the bottom rung always answers", last)
	}
}

func TestRosterArmsCoverTheProtoOneof(t *testing.T) {
	// Arrange
	arms, err := vocab.OneofArmNames((&frontendv1.RosterRow{}).ProtoReflect().Descriptor(), "status")
	if err != nil {
		t.Fatalf("OneofArmNames: %v", err)
	}
	got := RosterArms()

	// Act
	sort.Strings(arms)
	sort.Strings(got)

	// Assert
	if !slices.Equal(got, arms) {
		t.Fatalf("placed roster arms = %v, want every RosterRow.status arm %v", got, arms)
	}
}

func TestEveryRosterArmIsPlacedOnALadderRung(t *testing.T) {
	for _, arm := range RosterArms() {
		t.Run(arm, func(t *testing.T) {
			// Act
			claim, _ := RosterArmClaim(arm)

			// Assert
			if claim != Inactive && !slices.Contains(Order, claim) {
				t.Fatalf("roster arm %q projects onto %q, which is not a rung", arm, claim)
			}
		})
	}
}

func TestRosterArmClaimRefusesAnUnplacedArm(t *testing.T) {
	// Act
	_, ok := RosterArmClaim("teleporting")

	// Assert
	if ok {
		t.Fatal("RosterArmClaim placed an arm the roster has no oneof arm for")
	}
}

func TestFooterClaimPlacesEveryFooterArm(t *testing.T) {
	// Arrange: one status per arm the generated oneof declares.
	oneof := (&frontendv1.FooterStatus{}).ProtoReflect().Descriptor().Oneofs().ByName("status")
	fields := oneof.Fields()
	for i := 0; i < fields.Len(); i++ {
		field := fields.Get(i)
		t.Run(string(field.Name()), func(t *testing.T) {
			status := &frontendv1.FooterStatus{}
			m := status.ProtoReflect()
			m.Set(field, m.NewField(field))

			// Act
			claim, ok := FooterClaim(status)

			// Assert
			if !ok || !slices.Contains(Order, claim) {
				t.Fatalf("FooterClaim(%s) = (%q, %v), want a ladder rung", field.Name(), claim, ok)
			}
		})
	}
}

func TestFooterClaimPlacesTheWakeupFallbackOnIdle(t *testing.T) {
	// Arrange
	status := &frontendv1.FooterStatus{Status: &frontendv1.FooterStatus_Waiting{
		Waiting: &frontendv1.FooterStatusWaiting{
			Substatus: &frontendv1.FooterStatusWaiting_Wakeup{Wakeup: &frontendv1.FooterSubStatusWaitingWakeup{}}},
	}}

	// Act
	claim, _ := FooterClaim(status)

	// Assert
	if claim != Idle {
		t.Fatalf("FooterClaim(waiting · wakeup) = %q, want idle", claim)
	}
}

func TestFooterClaimRefusesAnUnsetStatus(t *testing.T) {
	// Act
	_, ok := FooterClaim(&frontendv1.FooterStatus{})

	// Assert
	if ok {
		t.Fatal("FooterClaim placed a status with no arm set")
	}
}

func TestAwaitingBringUp(t *testing.T) {
	cases := []struct {
		name         string
		linkSeen     bool
		turnInFlight bool
		want         bool
	}{
		{name: "a turn on a route never seen awaits the bring-up", turnInFlight: true, want: true},
		{name: "a turn on a seen route does not", linkSeen: true, turnInFlight: true},
		{name: "no turn on a route never seen does not", turnInFlight: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := AwaitingBringUp(tc.linkSeen, tc.turnInFlight)

			// Assert
			if got != tc.want {
				t.Fatalf("AwaitingBringUp(%v, %v) = %v, want %v", tc.linkSeen, tc.turnInFlight, got, tc.want)
			}
		})
	}
}
