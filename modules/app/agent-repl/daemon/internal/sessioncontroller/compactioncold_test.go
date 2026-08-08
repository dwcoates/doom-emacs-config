package sessioncontroller

import (
	"context"
	"testing"

	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/errclass"
	"claude-repld/internal/keepalive"
	"claude-repld/internal/registry"
)

// coldCompactionRig is a live session with a daemon compaction already claimed
// under turnID, which is the state every terminal result the alarm judges
// arrives into.
func coldCompactionRig(t *testing.T, kind compactionKind, turnID string) (*Manager, *sessionController, *logCapture) {
	t.Helper()
	m, _, _, _, capture := coldPingRig(t)
	d := controllerFor(t, m)
	m.mu.Lock()
	err := m.claimDaemonCompactionLocked(d, daemonCompaction{turnID: turnID, kind: kind})
	m.mu.Unlock()
	if err != nil {
		t.Fatalf("claim the %s compaction: %v", kind, err)
	}
	return m, d, capture
}

// usageOf is one result's usage in the SDK's three disjoint input buckets, in
// the order the Usage contract states them.
// costOf builds one turn's reduced result cost directly from the canonical
// buckets, for a case that is about a THRESHOLD rather than about the vendor
// conversion. The conversion itself is proved by newTurnResultCost's own tests.
func costOf(turnID string, freshInput, cacheWrite, cacheRead uint64) turnResultCost {
	return turnResultCost{turnID: turnID, usage: &frontendv1.TokenUsage{
		InputHits:   &frontendv1.TokenCacheHits{Read: cacheRead},
		InputMisses: &frontendv1.TokenCacheMisses{Written: cacheWrite, Unwritten: freshInput},
	}}
}

// mustTurnResultCost converts one vendor usage and fails the test if the vendor
// reported a counter the canonical shape cannot hold.
func mustTurnResultCost(t *testing.T, turnID string, usage *datav1.Usage) turnResultCost {
	t.Helper()
	cost, err := newTurnResultCost(turnID, usage)
	if err != nil {
		t.Fatalf("newTurnResultCost(%q) = %v", turnID, err)
	}
	return cost
}

func usageOf(input, cacheCreation, cacheRead int64) *datav1.Usage {
	return &datav1.Usage{
		InputTokens:              input,
		CacheCreationInputTokens: cacheCreation,
		CacheReadInputTokens:     cacheRead,
	}
}

// retainedCard reads the failure card the consumer retained under uuid, or nil.
func retainedCard(d *sessionController, uuid string) *frontendv1.FailureCardView {
	item := d.consumer.retainedFailure(uuid)
	return item.GetFailureCard()
}

// A DAEMON COMPACTION THAT READ THE CONVERSATION COLD RAISES A CARD. The
// compaction succeeded and the conversation is smaller, so without the card the
// user was billed for a full-context re-ingest with nothing anywhere saying so.
func TestColdCompactionPushesAFailureCard(t *testing.T) {
	// Arrange.
	const turnID = "warm-compact:s1:abc"
	m, d, _ := coldCompactionRig(t, compactionWarm, turnID)

	// Act.
	m.noteDaemonCompactionCost(d, costOf(turnID, uint64(keepalive.ColdCompactionUncachedTokens+1), 0, 0))

	// Assert.
	card := retainedCard(d, d.consumer.coldCompactionUUID(turnID))
	if card == nil {
		t.Fatal("a cold compaction pushed no failure card; the cost defect reaches no surface the user has")
	}
	if errclass.TypeName(card) != string(errclass.TypeCompactionColdRead) {
		t.Fatalf("card error_type = %q, want %q", errclass.TypeName(card), errclass.TypeCompactionColdRead)
	}
	if errclass.CardTone(card) != errclass.ToneLocal {
		t.Fatalf("card error_class = %s, want INTERNAL; nothing the vendor did was wrong, the daemon's scheduling was", errclass.CardTone(card))
	}
}

// THE CARD CARRIES THE RAW USAGE BREAKDOWN. A reader asked to believe a
// compaction read cold needs the arithmetic, not only the verdict: the bare
// conclusion cannot be checked and cannot be reconstructed from a bill later.
func TestColdCompactionCardCarriesTheUsageBreakdown(t *testing.T) {
	// Arrange.
	const turnID = "warm-compact:s1:abc"
	m, d, _ := coldCompactionRig(t, compactionWarm, turnID)

	// Act.
	m.noteDaemonCompactionCost(d, mustTurnResultCost(t, turnID, usageOf(11, 1_500_000, 40)))

	// Assert.
	detail := retainedCard(d, d.consumer.coldCompactionUUID(turnID)).GetDetail()
	for _, want := range []string{
		"input_tokens=11",
		"cache_creation_input_tokens=1500000",
		"cache_read_input_tokens=40",
		"uncached_input_tokens=1500011",
	} {
		if !contains(detail, want) {
			t.Fatalf("card source_detail %q is missing %q", detail, want)
		}
	}
}

// THE FINDING IS RECORDED AT ERROR LEVEL WITH EVERY IDENTITY. A cost defect
// found weeks later in a bill is reconstructed from exactly these fields, and a
// reader who has to hand-correlate the workspace against another line has
// already lost.
func TestColdCompactionRecordsTheSessionAndWorkspaceIdentity(t *testing.T) {
	// Arrange.
	const turnID = "warm-compact:s1:abc"
	m, d, capture := coldCompactionRig(t, compactionWarm, turnID)

	// Act.
	m.noteDaemonCompactionCost(d, costOf(turnID, uint64(keepalive.ColdCompactionUncachedTokens+1), 0, 0))

	// Assert.
	if !capture.contains("DAEMON COMPACTION READ COLD") {
		t.Fatal("the cold read was not recorded")
	}
	for _, want := range []string{`ws="ws"`, "session=s1", "turn_id=" + turnID, "kind=warm"} {
		if !capture.contains(want) {
			t.Fatalf("no record carried %q; the finding cannot be attributed without it", want)
		}
	}
}

// AT OR BELOW THE THRESHOLD THE ALARM IS SILENT. The threshold is exclusive,
// matching every other cost comparison in this daemon.
func TestWarmCompactionAtTheThresholdRaisesNoCard(t *testing.T) {
	// Arrange.
	const turnID = "warm-compact:s1:abc"
	m, d, capture := coldCompactionRig(t, compactionWarm, turnID)

	// Act.
	m.noteDaemonCompactionCost(d, costOf(turnID, uint64(keepalive.ColdCompactionUncachedTokens), 0, 0))

	// Assert.
	if card := retainedCard(d, d.consumer.coldCompactionUUID(turnID)); card != nil {
		t.Fatalf("a compaction exactly at the %d threshold raised %+v; the comparison is exclusive",
			keepalive.ColdCompactionUncachedTokens, card)
	}
	if capture.contains("DAEMON COMPACTION READ COLD") {
		t.Fatal("a compaction at the threshold was recorded as cold")
	}
}

// THE WARM CASE IS RECORDED TOO. The alarm's silence is otherwise
// indistinguishable from the alarm never having been asked, and this line is
// the only evidence the feature is working.
func TestWarmCompactionRecordsItsWarmRead(t *testing.T) {
	// Arrange.
	const turnID = "warm-compact:s1:abc"
	m, d, capture := coldCompactionRig(t, compactionWarm, turnID)

	// Act.
	m.noteDaemonCompactionCost(d, mustTurnResultCost(t, turnID, usageOf(4, 0, 1_500_000)))

	// Assert.
	if !capture.contains("daemon compaction read WARM") {
		t.Fatal("a warm compaction left no record; the feature working looks exactly like the feature not running")
	}
}

// THE ALARM COVERS THE REVIVAL COMPACTION TOO, and that is the incident it was
// built from: a compact-first revival runs after the cache has already expired
// by construction, so it is the likeliest cold read in the daemon.
func TestColdReviveCompactionRaisesTheSameAlarm(t *testing.T) {
	// Arrange.
	const turnID = "revive-compact:s1:abc"
	m, d, capture := coldCompactionRig(t, compactionRevive, turnID)

	// Act.
	m.noteDaemonCompactionCost(d, mustTurnResultCost(t, turnID, usageOf(2, 1_500_000, 0)))

	// Assert.
	if retainedCard(d, d.consumer.coldCompactionUUID(turnID)) == nil {
		t.Fatal("a cold revival compaction raised no card; the measured 1.5-million-token incident would still be silent")
	}
	if !capture.contains("kind=revive") {
		t.Fatal("the record did not name the revival as the compaction that read cold")
	}
}

// A RESULT FOR SOME OTHER TURN IS NOT THE COMPACTION'S. Two independent facts
// must agree — the accounting reducer's attribution and this session's own
// claim — because attributing a cost by elimination is exactly how an expensive
// user turn would be reported as a broken compaction.
func TestColdReadAlarmIgnoresAResultForAnotherTurn(t *testing.T) {
	// Arrange.
	const turnID = "warm-compact:s1:abc"
	m, d, capture := coldCompactionRig(t, compactionWarm, turnID)

	// Act: an expensive USER turn while the compaction is claimed.
	m.noteDaemonCompactionCost(d, costOf("req_user", uint64(keepalive.ColdCompactionUncachedTokens*100), 0, 0))

	// Assert.
	if capture.contains("DAEMON COMPACTION READ COLD") {
		t.Fatal("a user turn's cost was reported as a cold compaction; an expensive user turn is a cost report, not a defect in this feature")
	}
}

// WITH NO CLAIM STANDING NOTHING IS JUDGED. A session that is not running a
// daemon compaction has no compaction whose cost could be wrong.
func TestColdReadAlarmIgnoresAResultWithNoCompactionClaimed(t *testing.T) {
	// Arrange: a live session with no compaction claimed at all.
	m, _, _, _, capture := coldPingRig(t)
	d := controllerFor(t, m)

	// Act.
	m.noteDaemonCompactionCost(d, costOf("some-turn", uint64(keepalive.ColdCompactionUncachedTokens*100), 0, 0))

	// Assert.
	if capture.contains("DAEMON COMPACTION READ COLD") {
		t.Fatal("a turn was judged as a compaction with no compaction claimed")
	}
}

// A SECOND CLAIM IS REFUSED RATHER THAN OVERWRITING. Overwriting would silently
// hand the first compaction's cost to the second one's record, which is the
// misattribution the claim exists to prevent.
func TestClaimingASecondDaemonCompactionIsRefused(t *testing.T) {
	// Arrange.
	m, d, _ := coldCompactionRig(t, compactionWarm, "warm-compact:s1:first")

	// Act.
	m.mu.Lock()
	err := m.claimDaemonCompactionLocked(d, daemonCompaction{turnID: "revive-compact:s1:second", kind: compactionRevive})
	m.mu.Unlock()

	// Assert.
	if err == nil {
		t.Fatal("a second daemon compaction claim was accepted; the first compaction's cost would be recorded against the second")
	}
}

// THE CLAIM IS RETIRED ONLY BY ITS OWN TURN. A late end for some other turn
// must not release a claim this compaction still owns.
func TestReleasingADaemonCompactionMatchesOnTheTurnID(t *testing.T) {
	// Arrange.
	const turnID = "warm-compact:s1:abc"
	m, d, _ := coldCompactionRig(t, compactionWarm, turnID)

	// Act.
	m.mu.Lock()
	releasedOther := m.releaseDaemonCompactionLocked(d, "some-other-turn")
	releasedOwn := m.releaseDaemonCompactionLocked(d, turnID)
	m.mu.Unlock()

	// Assert.
	if releasedOther {
		t.Fatal("another turn's end released the compaction's claim")
	}
	if !releasedOwn {
		t.Fatal("the compaction's own turn did not release its claim; the claim would outlive the compaction")
	}
}

// THE REVIVAL'S COMPACTION IS CLAIMED AT SUBMIT, so its terminal result can
// never arrive with nothing to match it against.
func TestReviveCompactionIsClaimedForTheColdReadAlarm(t *testing.T) {
	// Arrange.
	m, _, _ := reviveRig(t, registry.HibernationCauseCacheExpired)

	// Act.
	if err := m.ReviveSession(context.Background(), "ws", ReviveModeCompactAll); err != nil {
		t.Fatalf("ReviveSession compact_first: %v", err)
	}

	// Assert.
	m.mu.Lock()
	claim := m.byWS["ws"].daemonCompaction
	m.mu.Unlock()
	if claim == nil || claim.kind != compactionRevive {
		t.Fatalf("the revival compaction's claim = %+v, want a revive claim; its result would be unjudged", claim)
	}
}
