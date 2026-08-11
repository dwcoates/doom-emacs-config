package sessioncontroller

import (
	"context"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
)

// THE SUBMITTER DECIDES, AND IT DECIDES AT THE SUBMIT. Only the daemon's own two
// machine submitters fail to count as engagement; everything else is somebody
// wanting something from the workspace.
func TestSubmitterEngagement(t *testing.T) {
	tests := []struct {
		name string
		who  submitter
		want bool
	}{
		{"a user prompt is engagement", submitterUser, true},
		{"a merge driving the session is engagement", submitterMergeLeaseHolder, true},
		{"a revival the user chose is engagement", submitterRevival, true},
		{"a bounce's owed turn resumption is engagement", submitterTurnResumption, true},
		{"a cache keep-alive ping is NOT engagement", submitterKeepAlive, false},
		{"a warm compaction is NOT engagement", submitterWarmCompaction, false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := tc.who.engagement()

			// Assert.
			if got != tc.want {
				t.Fatalf("submitter %s engagement = %t, want %t", tc.who, got, tc.want)
			}
		})
	}
}

// A KEEP-ALIVE PING'S END MOVES THE CACHE CLOCK AND NOT THE ENGAGEMENT ONE.
// This is the defect's fix stated end to end: before it, this same boundary
// reset the only clock there was, and a session pinged every cache lifetime
// never reached the idle cutoff.
func TestAPingsTurnEndLeavesTheEngagementClockAlone(t *testing.T) {
	// Arrange.
	m, _, hib, _, _ := coldPingRig(t)
	before := hib.lastEngagement("s1")
	turnID := submitPingUnderTurn(t, m)
	d := controllerFor(t, m)

	// Act — the bound turn-end hook, which is production's own closure.
	d.consumer.onTurnEnded(turnID, coldPingLastTurnEnd+coldPingElapsedMs)

	// Assert.
	if got := hib.lastEngagement("s1"); got != before {
		t.Fatalf("engagement clock moved to %d for keep-alive turn %s; a ping is the daemon talking to itself, not somebody using the workspace",
			got, turnID)
	}
}

// AND IT STILL MOVES THE CACHE CLOCK, which is the half that must NOT change:
// the ping refreshes the prompt cache, so the next ping is due a cache lifetime
// after this boundary.
func TestAPingsTurnEndStillMovesTheCacheClock(t *testing.T) {
	// Arrange.
	m, _, hib, _, _ := coldPingRig(t)
	turnID := submitPingUnderTurn(t, m)
	d := controllerFor(t, m)
	boundary := coldPingLastTurnEnd + coldPingElapsedMs

	// Act.
	d.consumer.onTurnEnded(turnID, boundary)

	// Assert.
	if got := hib.lastTurnEnd("s1"); got != boundary {
		t.Fatalf("cache clock = %d, want the ping's boundary %d; the ping schedule measures from it", got, boundary)
	}
}

// A REAL PROMPT'S TURN END MOVES BOTH. The mirror of the two tests above.
func TestAUserTurnsEndMovesTheEngagementClock(t *testing.T) {
	// Arrange.
	m, _, hib, _, _ := coldPingRig(t)
	if err := m.SubmitPrompt(context.Background(), "ws", "req_user", "hello", "",
		corev1.PromptOrigin_PROMPT_ORIGIN_USER_SENT); err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}
	d := controllerFor(t, m)
	boundary := coldPingLastTurnEnd + coldPingElapsedMs

	// Act.
	d.consumer.onTurnEnded("req_user", boundary)

	// Assert.
	if got := hib.lastEngagement("s1"); got != boundary {
		t.Fatalf("engagement clock = %d, want the user turn's boundary %d", got, boundary)
	}
}

// AN UNMARKED TURN COUNTS AS ENGAGEMENT. Only the machine submitters leave a
// mark, so an unmarked turn is a real one by construction — and the two ways to
// be wrong are not symmetric: counting a ping as engagement delays a teardown,
// counting real work as machinery reaps a workspace somebody is using.
func TestAnUnmarkedTurnCountsAsEngagement(t *testing.T) {
	// Arrange.
	m, _, _, _, _ := coldPingRig(t)
	d := controllerFor(t, m)

	// Act.
	got := m.engagementTurn(d, "req_never_marked")

	// Assert.
	if !got {
		t.Fatal("an unmarked turn was treated as machinery; only a declared machine turn may fail to count as engagement")
	}
}

// A TURN END WITH NO ID COUNTS AS ENGAGEMENT, for the same asymmetry: an
// untraceable boundary is not evidence that the daemon produced it.
func TestAnUnidentifiedTurnEndCountsAsEngagement(t *testing.T) {
	// Arrange — a machine turn IS marked, so the answer cannot come from there.
	m, _, _, _, _ := coldPingRig(t)
	d := controllerFor(t, m)
	m.noteMachineTurn(d, "ka_marked", submitterKeepAlive)

	// Act.
	got := m.engagementTurn(d, "")

	// Assert.
	if !got {
		t.Fatal("an unidentified turn end was treated as machinery")
	}
}

// THE MARK IS CONSUMED BY THE TURN IT NAMES. A mark left standing would make the
// NEXT turn — a real one — look like machinery and hold the cutoff open forever.
func TestAMachineTurnMarkIsConsumedByItsOwnEnd(t *testing.T) {
	// Arrange.
	m, _, _, _, _ := coldPingRig(t)
	d := controllerFor(t, m)
	m.noteMachineTurn(d, "ka_1", submitterKeepAlive)
	if m.engagementTurn(d, "ka_1") {
		t.Fatal("the marked machine turn counted as engagement")
	}

	// Act — the next turn, a real one.
	got := m.engagementTurn(d, "req_user")

	// Assert.
	if !got {
		t.Fatal("a real turn after a machine turn was still treated as machinery; the mark was not consumed")
	}
}

// A FAILED SUBMIT RETRACTS THE MARK, so a turn the shim never took cannot be
// left waiting to answer for an id.
func TestAMachineTurnMarkIsRetractedOnAFailedSubmit(t *testing.T) {
	// Arrange.
	m, _, _, _, _ := coldPingRig(t)
	d := controllerFor(t, m)
	m.noteMachineTurn(d, "ka_1", submitterKeepAlive)

	// Act.
	m.forgetMachineTurn(d, "ka_1")

	// Assert.
	if !m.engagementTurn(d, "ka_1") {
		t.Fatal("a retracted machine-turn mark still answered for its id")
	}
}

// A SECOND MACHINE TURN ON TOP OF A STANDING ONE IS AN INVARIANT VIOLATION and
// says so. The claims are supposed to make it impossible, and absorbing it
// silently would let the older turn's end move the engagement clock as though
// somebody had asked for it.
func TestASecondMachineTurnIsReportedAsAnInvariantViolation(t *testing.T) {
	// Arrange.
	m, _, _, _, capture := coldPingRig(t)
	d := controllerFor(t, m)
	m.noteMachineTurn(d, "ka_1", submitterKeepAlive)

	// Act.
	m.noteMachineTurn(d, "wc_1", submitterWarmCompaction)

	// Assert.
	if !capture.contains("INVARIANT VIOLATION — machine turn wc_1") {
		t.Fatal("a second machine turn overwrote the first with no report; the exclusivity it breaks has no other symptom")
	}
}
