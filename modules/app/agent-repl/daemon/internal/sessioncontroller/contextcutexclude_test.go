package sessioncontroller

import (
	"fmt"
	"strings"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/daemonturn"
)

// contextCutExcludeConsumer is a bare translation consumer. The exclusion is a
// pure function of the delta's own request ids, so no ledger and no store are
// wired: that independence is the point of deciding on the turn id.
func contextCutExcludeConsumer(t *testing.T, logf func(string, ...any)) *consumer {
	t.Helper()
	return newConsumer("ws", "s1", &fakePusher{}, &fakeApplier{}, nil,
		newFakeClearCompactStore(), emptyTurnAccountingStore{}, logf, nil, nil, nil, nil, nil)
}

// resultItem is a turn's terminal result as it reaches the feed — the item the
// webapp draws as a duration chip, and the one this exclusion exists to keep
// off a turn the user cannot see.
func resultItem(uuid, requestID string) *frontendv1.ConversationItem {
	return &frontendv1.ConversationItem{
		Uuid: uuid, RequestId: requestID,
		Item: &frontendv1.ConversationItem_Agent{Agent: &frontendv1.AgentEmission{
			Emission: &frontendv1.AgentEmission_TurnResult{TurnResult: nil},
		}},
	}
}

// A DAEMON CONTEXT CUT LEAVES NO RESIDUE. Every item a warm compaction or a
// revival cut contributes is turn plumbing — the terminal result the
// feed drew as a bare `6s` badge, and the CLI's own notice when the compaction
// declined — and none of it is anybody's conversation.
func TestDaemonContextCutExclusionWithholdsEveryItemOfTheDaemonsOwnCut(t *testing.T) {
	tests := []struct {
		name      string
		requestID string
	}{
		{name: "warm compaction", requestID: daemonturn.WarmCompactPrefix + "s_1:abcd"},
		{name: "compact-first revival", requestID: daemonturn.ReviveCompactPrefix + "s_1:abcd"},
		{name: "clear-first revival", requestID: daemonturn.ReviveClearPrefix + "s_1:abcd"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			c := contextCutExcludeConsumer(t, func(string, ...any) {})
			cd := excludeDelta(
				resultItem("r1", tt.requestID),
				excludeItem("a1", tt.requestID, 1_000),
			)

			// Act.
			withheld := c.withholdDaemonContextCut(cd)

			// Assert.
			if withheld != 2 || len(cd.GetItems()) != 0 {
				t.Fatalf("withheld=%d remaining=%v, want every item of the daemon's own compaction withheld",
					withheld, itemUUIDs(cd))
			}
		})
	}
}

// A REAL TURN KEEPS ITS CHIP. A turn the user asked for that produced no answer
// — one that only ran tools, or one that was interrupted — is the case the chip
// exists for: it is the only thing on the feed saying that turn ran at all.
func TestDaemonContextCutExclusionShowsARealTurnsResult(t *testing.T) {
	// Arrange.
	c := contextCutExcludeConsumer(t, func(string, ...any) {})
	cd := excludeDelta(resultItem("r1", "req_user"))

	// Act.
	withheld := c.withholdDaemonContextCut(cd)

	// Assert.
	if withheld != 0 || len(cd.GetItems()) != 1 {
		t.Fatalf("withheld=%d remaining=%v, want the user's own turn result shown",
			withheld, itemUUIDs(cd))
	}
}

// THE COMPACTION'S OWN DIVIDER SURVIVES, and it survives structurally: the
// vendor's `compact_boundary` is a file-plane record carrying no request id,
// and an empty id is never the daemon's. The divider is the one thing a
// compaction owes the user, so an exclusion that took it would trade an orphan
// chip for a silently shortened conversation.
func TestDaemonContextCutExclusionShowsTheIDLessCompactionDivider(t *testing.T) {
	// Arrange.
	c := contextCutExcludeConsumer(t, func(string, ...any) {})
	cd := excludeDelta(&frontendv1.ConversationItem{
		Uuid: "d1",
		Item: &frontendv1.ConversationItem_ContextCompacted{
			ContextCompacted: &corev1.ContextCompacted{},
		},
	})

	// Act.
	withheld := c.withholdDaemonContextCut(cd)

	// Assert.
	if withheld != 0 || len(cd.GetItems()) != 1 {
		t.Fatalf("withheld=%d remaining=%v, want the compaction divider shown",
			withheld, itemUUIDs(cd))
	}
}

// THE RE-DRIVEN TURN KEEPS ITS RESULT. Only the daemon's own instruction is
// hidden on a resumption (frontend/internalresume.go); the work that follows is
// the continuation the user is owed, and it is a visible answer with a chip
// that belongs to it.
func TestDaemonContextCutExclusionShowsAnInternalResumeResult(t *testing.T) {
	// Arrange.
	c := contextCutExcludeConsumer(t, func(string, ...any) {})
	cd := excludeDelta(resultItem("r1", "resume-after-restart:/ws/turn:0"))

	// Act.
	withheld := c.withholdDaemonContextCut(cd)

	// Assert.
	if withheld != 0 || len(cd.GetItems()) != 1 {
		t.Fatalf("withheld=%d remaining=%v, want the re-driven turn's result shown",
			withheld, itemUUIDs(cd))
	}
}

// ONE CENSUS RECORD, carrying what was taken and what is left. A compaction
// turn's items arrive across several deltas, so the exclusion accounts for
// itself once per delta rather than once per item.
func TestDaemonContextCutExclusionLogsOneCensusRecord(t *testing.T) {
	// Arrange.
	var logged []string
	c := contextCutExcludeConsumer(t, func(format string, args ...any) {
		logged = append(logged, strings.TrimSpace(fmt.Sprintf(format, args...)))
	})
	cd := excludeDelta(
		resultItem("r1", daemonturn.WarmCompactPrefix+"s_1:abcd"),
		excludeItem("u1", "req_user", 1_000),
	)

	// Act.
	c.withholdDaemonContextCut(cd)

	// Assert.
	if len(logged) != 1 {
		t.Fatalf("logged %d records, want exactly one census record: %v", len(logged), logged)
	}
	for _, want := range []string{"daemon context cut items WITHHELD", "withheld=1", "remaining=1"} {
		if !strings.Contains(logged[0], want) {
			t.Errorf("census record %q missing %q", logged[0], want)
		}
	}
}

// EVERY MINTING SITE PRODUCES AN ID THE EXCLUSION RECOGNIZES. The mint and the
// verdict are the two halves of one guarantee, and they live in different
// packages — so a site that hand-rolls its own prefix would submit a turn the
// curator renders, which is precisely the defect this file exists to close, and
// it would show up nowhere else.
func TestEveryDaemonContextCutMintIsRecognizedByTheExclusion(t *testing.T) {
	// Arrange — every mode that cuts, plus the warm compaction's own mint.
	cuttingModes := []ReviveMode{
		ReviveModeCompactAll,
		ReviveModeCompactResponses,
		ReviveModeCompactPrompts,
		ReviveModeCompactPromptsAndResponses,
		ReviveModeClear,
	}
	minted := map[string]string{}
	warmID, err := newWarmCompactRequestID("s_1")
	if err != nil {
		t.Fatalf("newWarmCompactRequestID: %v", err)
	}
	minted["warm compaction"] = warmID
	for _, mode := range cuttingModes {
		cut, err := mode.cut()
		if err != nil {
			t.Fatalf("%s.cut(): %v", mode, err)
		}
		id, err := newReviveCutRequestID(cut.requestIDPrefix, "s_1")
		if err != nil {
			t.Fatalf("newReviveCutRequestID(%s): %v", mode, err)
		}
		minted[mode.String()] = id
	}

	// Act + Assert — each minted id must be withheld from a delta of its own.
	for name, requestID := range minted {
		t.Run(name, func(t *testing.T) {
			c := contextCutExcludeConsumer(t, func(string, ...any) {})
			cd := excludeDelta(resultItem("r1", requestID))

			withheld := c.withholdDaemonContextCut(cd)

			if withheld != 1 || len(cd.GetItems()) != 0 {
				t.Fatalf("minted id %q: withheld=%d remaining=%v, want the daemon's own cut withheld",
					requestID, withheld, itemUUIDs(cd))
			}
		})
	}
}
