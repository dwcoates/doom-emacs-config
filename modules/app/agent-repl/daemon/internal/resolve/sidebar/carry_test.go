package sidebar_test

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/shimclient"
)

// carriedChanges counts the status and visibility changes R recorded after
// the first FROM records; its registry holds theWS alone.
func carriedChanges(r sidebarResolver, from int) (status, visibility int) {
	for _, rec := range r.surfaces.Records()[from:] {
		switch rec.Operation {
		case "daemon.sidebar.status_arm_changed":
			status++
		case "daemon.sidebar.visibility_changed":
			visibility++
		}
	}
	return status, visibility
}

// joinedMirroring is a joining daemon's roster over theWS, folded, drawing
// the incumbent's ARM through the mirror; it answers how many records stood
// then.
func joinedMirroring(t *testing.T, arm string) (sidebarResolver, int) {
	t.Helper()
	r := arrange(t)
	r.SetRegistry(foldedOne())
	r.SetIncumbentArms(map[ids.WorkspaceID]string{theWS: arm})
	if got := statusName(onlyRow(t, r)); got != arm {
		t.Fatalf("arranged status = %q, want the incumbent's %q", got, arm)
	}
	return r, len(r.surfaces.Records())
}

// A HANDOVER IN WHICH NOTHING ABOUT THE WORKSPACE CHANGED CHANGES NOTHING ON
// ITS ROW: the mirror lets go at ownership, the adoption's facts arrive one by
// one, and the row draws the incumbent's arm throughout, then its own, the
// same (carry.go). One case per sequence the 2026-11-24 06:20 handover drew.
func TestAHandoverThatChangesNothingChangesNoRow(t *testing.T) {
	cases := []struct {
		name     string
		arm      string
		adoption func(t *testing.T, r sidebarResolver)
	}{
		{
			name: "a merge waiting in line is never drawn done before its merge is taken back",
			arm:  "merge_queued",
			adoption: func(t *testing.T, r sidebarResolver) {
				turnFinished(r, theWS)
				r.SetMerge(theWS, footer.MergeFacts{State: "queued", Step: footer.StepEnqueued, QueuePlace: 1, QueueWaiting: 2, QueuedAt: epoch})
				r.SettleFacts(theWS)
			},
		},
		{
			name: "a cold-gated row is never drawn starting while its bring-up raises the gate again",
			arm:  "cold_gate",
			adoption: func(t *testing.T, r sidebarResolver) {
				r.SetBringingUp(theWS, true)
				r.SettleFacts(theWS)
				footerOf(t, r).SetColdGate(theWS, footer.ColdGate{Standing: true, Cost: footer.FiguredLine{Lead: "cold"}})
				r.FooterStatusChanged(theWS)
				r.SetBringingUp(theWS, false)
			},
		},
		{
			name: "an open asked again across the handover draws no inactive",
			arm:  "done",
			adoption: func(t *testing.T, r sidebarResolver) {
				r.SetOpening(theWS, true)
				turnFinished(r, theWS)
				r.SetOpening(theWS, false)
				r.SettleFacts(theWS)
			},
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			r, from := joinedMirroring(t, tc.arm)

			// Act: ownership lets the mirror go, then the adoption runs.
			r.SetIncumbentArms(nil)
			tc.adoption(t, r)

			// Assert
			status, visibility := carriedChanges(r, from)
			if got := statusName(onlyRow(t, r)); got != tc.arm || status != 0 || visibility != 0 {
				t.Fatalf("status %q with %d status and %d visibility changes; want %q and none", got, status, visibility, tc.arm)
			}
		})
	}
}

// A carried row is drawn from its own facts once they are settled, so a real
// change after the adoption is published as ever.
func TestASettledRowDrawsItsOwnFacts(t *testing.T) {
	// Arrange
	r, _ := joinedMirroring(t, "merge_queued")
	r.SetIncumbentArms(nil)
	turnFinished(r, theWS)
	r.SettleFacts(theWS)

	// Act: the merge starts on this daemon.
	r.SetMerge(theWS, footer.MergeFacts{State: "merging", Step: footer.StepRebasing, Total: 1, QueuedAt: epoch})

	// Assert
	if got := statusName(onlyRow(t, r)); got != "merging" {
		t.Fatalf("status = %q, want the merge this daemon runs", got)
	}
}

// The carried arm is let go only once both hold: the facts are declared in
// and no bring-up stands.
func TestACarriedArmWaitsForTheFactsAndTheBringUp(t *testing.T) {
	cases := []struct {
		name    string
		settle  bool
		bringUp bool
		want    string
	}{
		{name: "not settled keeps the carried arm", settle: false, bringUp: false, want: "cold_gate"},
		{name: "settled with a bring-up standing keeps the carried arm", settle: true, bringUp: true, want: "cold_gate"},
		{name: "settled with no bring-up draws its own arm", settle: true, bringUp: false, want: "ready"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			r, _ := joinedMirroring(t, "cold_gate")
			r.SetIncumbentArms(nil)
			r.OnLink(theWS, shimclient.LinkConnected)
			r.OnSessionStarted(theWS, &conversationv1.SessionStarted{VendorSessionId: "vendor-1"})
			r.SetBringingUp(theWS, tc.bringUp)

			// Act
			if tc.settle {
				r.SettleFacts(theWS)
			}

			// Assert
			if got := statusName(onlyRow(t, r)); got != tc.want {
				t.Fatalf("status = %q, want %q", got, tc.want)
			}
		})
	}
}

// Facts declared in before the mirror lets go leave nothing to carry, and
// the takeover's settle lets go of every carried row.
func TestSettlingBeforeTheLetGoAndAtTheTakeover(t *testing.T) {
	cases := []struct {
		name string
		act  func(r sidebarResolver)
	}{
		{name: "facts settled before the mirror lets go carry nothing", act: func(r sidebarResolver) {
			r.SettleFacts(theWS)
			r.SetIncumbentArms(nil)
		}},
		{name: "the takeover's settle lets go of a carried row", act: func(r sidebarResolver) {
			r.SetIncumbentArms(nil)
			r.SettleAllFacts()
		}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			r, _ := joinedMirroring(t, "cold_gate")
			r.OnLink(theWS, shimclient.LinkConnected)
			r.OnSessionStarted(theWS, &conversationv1.SessionStarted{VendorSessionId: "vendor-1"})

			// Act
			tc.act(r)

			// Assert
			if got := statusName(onlyRow(t, r)); got != "ready" {
				t.Fatalf("status = %q, want the row's own ready", got)
			}
			if _, found := dlog.FindRecord(r.surfaces.Records(), dlog.RecordQuery{Operation: "daemon.sidebar.arm_carried"}); found && tc.name == "facts settled before the mirror lets go carry nothing" {
				t.Fatalf("a row whose facts were already in was carried")
			}
		})
	}
}
