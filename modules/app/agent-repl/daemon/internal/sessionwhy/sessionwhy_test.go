package sessionwhy

import (
	"testing"
	"time"

	"agent-repld/internal/ids"
)

var now = time.Unix(1_700_000_000, 0)

func TestClassify(t *testing.T) {
	later, earlier := now.Add(time.Minute), now.Add(-time.Minute)
	tests := []struct {
		name  string
		work  Work
		facts Facts
		want  Reason
	}{
		{name: "a turn in flight is working whatever else holds", work: Work{Turn: true, Detached: true}, facts: Facts{Owner: OwnerInView}, want: Working},
		{name: "live detached work with no turn", work: Work{Detached: true}, facts: Facts{Owner: OwnerCold}, want: DetachedWork},
		{name: "the user looking at it", facts: Facts{Owner: OwnerInView, Hold: HoldDeferredBusy}, want: InView},
		{name: "remote control on", facts: Facts{Owner: OwnerRemoteControl}, want: RemoteControl},
		{name: "a settings visit", facts: Facts{Owner: OwnerVisit}, want: SettingsVisit},
		{name: "an armed park still to come", facts: Facts{Owner: OwnerCold, ParkDue: later, Hold: HoldDeferredBusy}, want: ParkGrace},
		{name: "a keep-alive owner past its grace is up for a keep-alive", facts: Facts{Owner: OwnerCycle, ParkDue: earlier}, want: Keepalive},
		{name: "a stand-down held back busy", facts: Facts{Owner: OwnerCold, Hold: HoldDeferredBusy}, want: DeferredBusy},
		{name: "a keep-alive owner held back busy", facts: Facts{Owner: OwnerCycle, Hold: HoldDeferredBusy}, want: DeferredBusy},
		{name: "a withheld stand-down that found it shown", facts: Facts{Owner: OwnerCold, Hold: HoldWithheldShown}, want: WithheldShown},
		{name: "left to its cache's lapse", facts: Facts{Owner: OwnerLapse}, want: CacheLapsePending},
		{name: "left to the idle sweep", facts: Facts{Owner: OwnerSweep}, want: IdleSweepPending},
		{name: "a cold park whose grace is out", facts: Facts{Owner: OwnerCold, ParkDue: earlier}, want: ParkOverdue},
		{name: "a cold park with none armed", facts: Facts{Owner: OwnerCold}, want: ParkOverdue},
		{name: "no owner at all", want: NoOwner},
		{name: "no owner but held back busy", facts: Facts{Hold: HoldDeferredBusy}, want: DeferredBusy},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := Classify(tt.work, tt.facts, now)

			// Assert
			if got != tt.want {
				t.Fatalf("Classify = %s, want %s", got, tt.want)
			}
		})
	}
}

func TestEveryReasonIsListedOnce(t *testing.T) {
	// Arrange
	seen := map[Reason]bool{}

	// Act and Assert
	for _, r := range All {
		if seen[r] {
			t.Fatalf("reason %s is listed twice", r)
		}
		seen[r] = true
	}
	if len(seen) != 13 {
		t.Fatalf("All lists %d reasons, want 13", len(seen))
	}
}

func TestTheTableKeepsEachFactApart(t *testing.T) {
	// Arrange
	table := NewTable()

	// Act
	table.SetOwner("a", OwnerCold)
	table.SetParkDue("a", now)
	table.SetHold("a", HoldWithheldShown)
	table.ClearParkDue("a")

	// Assert
	if got := table.Facts("a"); got != (Facts{Owner: OwnerCold, Hold: HoldWithheldShown}) {
		t.Fatalf("facts = %+v, want the owner and the hold, no park armed", got)
	}
}

func TestForgetDropsEveryFact(t *testing.T) {
	// Arrange
	table := NewTable()
	table.SetOwner("a", OwnerCycle)
	table.SetHold("a", HoldDeferredBusy)

	// Act
	table.Forget("a")

	// Assert
	if got := table.Facts("a"); got != (Facts{}) {
		t.Fatalf("facts = %+v, want none", got)
	}
}

func TestClearingTheLastFactLeavesNoEntry(t *testing.T) {
	// Arrange
	table := NewTable()
	table.SetHold("a", HoldDeferredBusy)

	// Act
	table.ClearHold("a")

	// Assert
	if len(table.facts) != 0 {
		t.Fatalf("entries = %v, want none once every fact is cleared", table.facts)
	}
}

func TestANilTableHoldsNothing(t *testing.T) {
	// Arrange
	var table *Table

	// Act: none of these panics.
	table.SetOwner("a", OwnerCold)
	table.SetParkDue("a", now)
	table.SetHold("a", HoldDeferredBusy)
	table.ClearHold("a")
	table.Forget("a")

	// Assert
	if got := table.Facts("a"); got != (Facts{}) {
		t.Fatalf("facts = %+v, want none", got)
	}
}

func TestTheCensusClassifiesEveryHeldSession(t *testing.T) {
	// Arrange
	table := NewTable()
	table.SetOwner("b", OwnerRemoteControl)
	census := Census{
		Live: func() []ids.WorkspaceID { return []ids.WorkspaceID{"a", "b", "gone"} },
		Work: func(ws ids.WorkspaceID) (Work, bool) {
			switch ws {
			case "a":
				return Work{Turn: true}, true
			case "b":
				return Work{}, true
			default:
				return Work{}, false
			}
		},
		Table: table,
		Now:   func() time.Time { return now },
	}

	// Act
	got := census.Up()

	// Assert
	if len(got) != 2 || got["a"] != Working || got["b"] != RemoteControl {
		t.Fatalf("up = %v, want a working, b in remote control, and nothing for a session no longer held", got)
	}
}
