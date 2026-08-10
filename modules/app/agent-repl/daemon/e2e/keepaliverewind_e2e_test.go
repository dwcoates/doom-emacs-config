// THE REWIND: erasing the ping from the model's context before the user's next
// prompt lands, without erasing it from the record.
//
// The keep-alive ping keeps a cache warm, and the price of that is a turn in
// the vendor transcript that the user never asked for. Left there, it would be
// context every subsequent turn pays for and a bubble no reader can explain. So
// before the next real prompt is submitted, the conversation is rewound to the
// last real turn boundary: a truncated copy of the transcript under a NEW
// vendor session id, byte-identical to the prefix already sent to the vendor,
// so the prompt lands on the cache the ping kept warm.
//
// TWO RECORDS DIVERGE HERE ON PURPOSE, and most of this file is about keeping
// that divergence honest:
//
//   - THE MODEL'S CONTEXT loses the ping entirely. That is the point.
//   - THE DURABLE RECORD keeps everything. The ping's turn is WITHHELD from
//     replay, never deleted — a store that dropped rows would make the
//     conversation unauditable and the seq space gappy.
//
// See hibernationharness_test.go for the store tail, and
// keepalivequeue_e2e_test.go for the hold that precedes every rewind here.
package e2e

import (
	"database/sql"
	"fmt"
	"strings"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// rewound is one completed rewind, with both ends of the lineage and the ping
// that caused it.
type rewound struct {
	session *keepAliveSession
	// previous is the retired vendor session id; next is the truncated copy the
	// conversation continues under.
	previous, next string
	pingTurnID     string
	// heldText is the prompt whose delivery the rewind was performed for.
	heldText string
}

// rewindByKeepAlivePing drives a full rewind: a ping fires, the user's prompt
// is held by it, and the delivery of that prompt is what makes the daemon
// rewind. It returns once the daemon has published the new identity.
func rewindByKeepAlivePing(t *testing.T, s *keepAliveSession) rewound {
	t.Helper()
	previous := s.vendorID
	writeDefaultRewindFixture(t, s)
	held := heldByKeepAlivePing(t, s, "the prompt the rewind is performed for")
	next := vendorSessionID(t, s.h, s.sessionID,
		func(id string) bool { return id != "" && id != previous },
		fmt.Sprintf("a conversation identity other than %q", previous))
	// The production sidecar tails the truncated copy and backfills its
	// records into the NEW seq space; the harness runs no sidecar, so the
	// backfill is replayed here — after the flip, exactly when the real one
	// would begin tailing the new file.
	ingestTranscriptAsSidecar(t, s, next)
	// Await the held prompt's own reply before returning: its items flow
	// through the daemon's pump AFTER the new space's SessionRewound (seq
	// order), and the pump applies SessionRewound inline — so a visible reply
	// proves the rewind's ledger attribution has committed, and every
	// caller's ledger/replay read is ordered behind it.
	awaitAll(t, s.conn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"the held prompt's reply on the rewound conversation": func(frame *frontendv1.FrontendFrame) bool {
			for _, item := range deltaItems(frame, s.cwd) {
				if strings.Contains(assistantText(item), echoOf(held.text)) {
					return true
				}
			}
			return false
		},
	})
	return rewound{session: s, previous: previous, next: next, pingTurnID: held.pingTurnID, heldText: held.text}
}

// writeDefaultRewindFixture writes the standard rewindable transcript shape
// (one real turn, then the ping's lines) unless the calling test already
// wrote its own fixture. The fake shim writes no vendor transcript, and the
// daemon's rewind reads the transcript from disk (session.TranscriptPath) —
// without one it degrades loudly to un-rewound delivery and no identity ever
// flips.
func writeDefaultRewindFixture(t *testing.T, s *keepAliveSession) {
	t.Helper()
	if _, exists := readFixtureTranscript(t, s.cwd, s.vendorID); exists {
		return
	}
	writeFixtureTranscript(t, s.cwd, s.vendorID, []string{
		userLine("real-prompt", "", s.vendorID, "do the thing"),
		assistantLine("real-final", "real-prompt", s.vendorID, "done"),
		userLine("ping-prompt", "real-final", s.vendorID, keepAlivePingText),
		assistantLine("ping-response", "ping-prompt", s.vendorID, "."),
	})
}

// --- (9) the flip ---------------------------------------------------------------

// TestE2EARewindFlipsTheConversationIdentityAndItsSeqMarksTogether covers THE
// ATOMIC FLIP, by the only evidence an acceptance test can hold: the three
// facts that must move as one have all moved.
//
// The uuid names a seq space; both marks are positions WITHIN a seq space. A
// flip that moved the uuid and left either mark behind would have the daemon
// re-subscribing to a brand-new space from a high-water mark belonging to a
// retired one — skipping the rewound conversation's entire history — or serving
// resyncs from a replay floor that names an event in a space nobody reads.
//
// (That the update is one transaction rather than three writes is a structural
// property, pinned where the write is; this is the post-condition it exists to
// guarantee.)
func TestE2EARewindFlipsTheConversationIdentityAndItsSeqMarksTogether(t *testing.T) {
	// Arrange — a session with BOTH marks meaningfully non-zero, so a mark that
	// survived the flip is visible as a survivor rather than as a zero that
	// happened to be right.
	s := newKeepAliveSession(t, testKeepAlivePolicy(), withGatedKeepAlivePing())
	store := dialStoreProducer(t)
	store.write(sidecarClearEvent(s.vendorID, "e2e-rewind-floor"))
	awaitItem(t, s.conn, s.cwd, "the injected clear", isClear)
	retired := readSessionRow(t, s.h, s.sessionID)
	if retired.newestClearOrCompactSeq == 0 || retired.lastSeq == 0 {
		t.Fatalf("premise not established: the pre-rewind row is %+v, want both seq marks non-zero", retired)
	}

	// Act
	rw := rewindByKeepAlivePing(t, s)

	// Assert
	row := readSessionRow(t, s.h, s.sessionID)
	if row.claudeSessionID != rw.next {
		t.Errorf("registry claude_session_id = %q, want the rewound conversation %q", row.claudeSessionID, rw.next)
	}
	if row.newestClearOrCompactSeq != 0 {
		t.Errorf("registry newest_clear_or_compact_seq = %d after the flip, want 0: it named a seq in the retired space, so carrying it forward floors every future resync at an event nothing will ever serve",
			row.newestClearOrCompactSeq)
	}
	if row.lastSeq >= retired.lastSeq {
		t.Errorf("registry last_seq = %d after the flip, want a mark inside the NEW space (the retired one stood at %d): re-subscribing a fresh seq space from a retired space's high-water mark skips the rewound conversation entirely",
			row.lastSeq, retired.lastSeq)
	}
}

// --- the announcement -----------------------------------------------------------

// TestE2EARewoundConversationOpensWithSessionRewound covers ORDER: the record
// of why the identity changed is the FIRST thing in the space it explains, so a
// reader of the store alone never encounters a conversation that begins
// mid-history with no account of where it came from.
func TestE2EARewoundConversationOpensWithSessionRewound(t *testing.T) {
	// Arrange + Act
	s := newKeepAliveSession(t, testKeepAlivePolicy(), withGatedKeepAlivePing())
	rw := rewindByKeepAlivePing(t, s)

	// Assert — the first event the new seq space serves.
	tail := tailStore(t, rw.next)
	first := tail.await(t, "the first event of the rewound conversation", func(*corev1.Event) bool { return true })
	if first.GetSessionRewound() == nil {
		t.Fatalf("the rewound conversation opens with %T, want SessionRewound: the explanation must precede what it explains", first.GetPayload())
	}
}

// TestE2EASessionRewoundNamesTheLineageItReplaces covers RECONSTRUCTABILITY: a
// vendor-session lineage must be recoverable from the store alone, so the
// record names both ends and the exact point the truncation kept.
func TestE2EASessionRewoundNamesTheLineageItReplaces(t *testing.T) {
	// Arrange + Act
	s := newKeepAliveSession(t, testKeepAlivePolicy(), withGatedKeepAlivePing())
	rw := rewindByKeepAlivePing(t, s)

	// Assert
	record := awaitSessionRewound(t, rw.next)
	if got := record.GetPreviousVendorSessionId(); got != rw.previous {
		t.Errorf("SessionRewound previous_vendor_session_id = %q, want the retired conversation %q", got, rw.previous)
	}
	if got := record.GetNewVendorSessionId(); got != rw.next {
		t.Errorf("SessionRewound new_vendor_session_id = %q, want the conversation it continues under, %q", got, rw.next)
	}
	if record.GetRetainedLeafUuid() == "" {
		t.Error("SessionRewound retained_leaf_uuid is empty: without the last retained record there is no way to say where the cut fell, and a cut that cannot be located cannot be shown to be at a turn boundary")
	}
}

// TestE2EASessionRewoundNamesTheDroppedKeepAliveTurns covers the CAUSE'S OWN
// EVIDENCE: the turns the truncation removed, by the ids every other consumer
// knows them under (SubmitPrompt.request_id = TurnStarted.turn_id). Store
// consumers mark exactly those superseded; a rewind that dropped turns without
// naming them would leave the record claiming turns that replay no longer has.
func TestE2EASessionRewoundNamesTheDroppedKeepAliveTurns(t *testing.T) {
	// Arrange + Act
	s := newKeepAliveSession(t, testKeepAlivePolicy(), withGatedKeepAlivePing())
	rw := rewindByKeepAlivePing(t, s)

	// Assert
	discard := awaitSessionRewound(t, rw.next).GetKeepAliveDiscard()
	if discard == nil {
		t.Fatal("SessionRewound carries no keep_alive_discard reason: discarding trailing keep-alive turns is the only producer of a rewind today")
	}
	found := false
	for _, id := range discard.GetDroppedTurnIds() {
		if id == rw.pingTurnID {
			found = true
		}
	}
	if !found {
		t.Errorf("dropped_turn_ids = %v, want the ping's own turn %q: the ping is exactly what the truncation removed", discard.GetDroppedTurnIds(), rw.pingTurnID)
	}
}

// TestE2EADroppedKeepAliveTurnIsClosedInTheTurnLedger covers the LEDGER: the
// daemon's own record of the turn must not be left open by a truncation that
// removed the turn from under it, or the workspace keeps a claim nothing can
// ever close.
//
// AMBIGUITY, STATED RATHER THAN RESOLVED: the design says dropped turns are
// "marked superseded in the turn ledger" and the ledger's existing vocabulary
// for a close the daemon performed without a real boundary is end_cause. This
// asserts the claim is closed and the close is ATTRIBUTED — which is what the
// design's words require — without inventing the token that attributes it.
func TestE2EADroppedKeepAliveTurnIsClosedInTheTurnLedger(t *testing.T) {
	// Arrange + Act
	s := newKeepAliveSession(t, testKeepAlivePolicy(), withGatedKeepAlivePing())
	rw := rewindByKeepAlivePing(t, s)

	// Assert
	var endSeq sql.NullInt64
	var endCause string
	err := s.h.stateDB.QueryRow(
		`SELECT end_seq, end_cause FROM turn_lifecycle_claim WHERE turn_id = ?`, rw.pingTurnID,
	).Scan(&endSeq, &endCause)
	if err != nil {
		t.Fatalf("read the turn ledger claim for the dropped keep-alive turn %q: %v", rw.pingTurnID, err)
	}
	if !endSeq.Valid {
		t.Errorf("the dropped keep-alive turn %q is still OPEN in the turn ledger: the truncation removed the turn, so nothing will ever arrive to close the claim", rw.pingTurnID)
	}
	if endCause == "" {
		t.Errorf("the dropped keep-alive turn %q was closed with no end_cause: a close the daemon performed because it superseded the turn must be distinguishable from one a real TurnEnded produced", rw.pingTurnID)
	}
}

// --- replay: withheld, not deleted ------------------------------------------------

// TestE2EAFullReplayCarriesNoKeepAliveTurn covers THE FRONTEND'S VIEW after the
// fact: a client that mounts later and asks for everything gets the
// conversation without the plumbing.
func TestE2EAFullReplayCarriesNoKeepAliveTurn(t *testing.T) {
	// Arrange
	s := newKeepAliveSession(t, testKeepAlivePolicy(), withGatedKeepAlivePing())
	rewindByKeepAlivePing(t, s)

	// Act — exactly what a reloaded webview sends.
	fresh, state := dialForReplay(t, s.h, s.sessionID, s.cwd)
	replayed := replayItems(t, fresh, state, s.cwd, "r-replay")

	// Assert
	if len(replayed) == 0 {
		t.Fatal("the replay carried no conversation items at all: the rewind retains the real conversation, it does not discard it")
	}
	for _, item := range replayed {
		if text := assistantText(item); text != "" && strings.Contains(text, keepAlivePingText) {
			t.Errorf("the replay carried the keep-alive turn (uuid=%q): a ping is plumbing, and a client asking for history must never be shown it", item.GetUuid())
		}
	}
}

// TestE2EAFullReplayStillCarriesTheRealTurnsFromBeforeTheRewind covers the
// other half of the same read: the truncation cuts at the last REAL turn
// boundary, so everything the user actually did is still there. A rewind that
// took real history with it would be data loss dressed up as an optimization.
func TestE2EAFullReplayStillCarriesTheRealTurnsFromBeforeTheRewind(t *testing.T) {
	// Arrange — a distinctive real turn before the ping, present in BOTH
	// records: the stream plane (runRealTurn) and the transcript fixture the
	// rewind truncates. The fake shim writes no transcript, so what the CLI
	// would have appended for the turn is written here — without it the
	// truncated copy could never carry the turn into the new seq space, no
	// matter how correct the cut is.
	s := newKeepAliveSession(t, testKeepAlivePolicy(), withGatedKeepAlivePing())
	writeFixtureTranscript(t, s.cwd, s.vendorID, []string{
		userLine("before-user", "", s.vendorID, "before-the-rewind"),
		assistantLine("before-echo", "before-user", s.vendorID, echoOf("before-the-rewind")+"default]"),
		userLine("ping-prompt", "before-echo", s.vendorID, keepAlivePingText),
		assistantLine("ping-response", "ping-prompt", s.vendorID, "."),
	})
	s.runRealTurn(t, "r-before", "before-the-rewind")
	rewindByKeepAlivePing(t, s)

	// Act
	fresh, state := dialForReplay(t, s.h, s.sessionID, s.cwd)
	replayed := replayItems(t, fresh, state, s.cwd, "r-replay")

	// Assert
	for _, item := range replayed {
		if strings.Contains(assistantText(item), echoOf("before-the-rewind")) {
			return
		}
	}
	t.Fatal("the replay lost the real turn that preceded the rewind: the cut falls at the last real turn boundary, so everything the user did survives it")
}

// TestE2EAKeepAliveTurnStaysInTheStoreAfterTheRewind covers WITHHELD, NOT
// DELETED — the divergence this file's header names. The frontend must never
// see the ping; the durable record must never lose it. Deleting rows would make
// the conversation unauditable and leave the retired seq space gappy, which is
// the one thing the store's per-session sequence promises it is not.
func TestE2EAKeepAliveTurnStaysInTheStoreAfterTheRewind(t *testing.T) {
	// Arrange
	s := newKeepAliveSession(t, testKeepAlivePolicy(), withGatedKeepAlivePing())
	rw := rewindByKeepAlivePing(t, s)

	// Act — read the RETIRED conversation from its beginning, as an auditor
	// would: the space is closed, so this is its whole and final content.
	retired := tailStore(t, rw.previous)

	// Assert
	started := retired.await(t, "the keep-alive turn still recorded in the retired conversation", func(ev *corev1.Event) bool {
		ping := keepAlivePing(ev)
		return ping != nil && ping.GetTurnId() == rw.pingTurnID
	})
	if got := keepAlivePing(started).GetPromptPreview(); got != keepAlivePingText {
		t.Errorf("the retained keep-alive turn's prompt is %q, want the ping verbatim %q: the record keeps what was actually sent", got, keepAlivePingText)
	}
}

// TestE2ETheLiveStreamNeverShowedTheKeepAliveTurnEither covers the LIVE half of
// the exclusion, which is a separate code path from the replay above: a client
// connected throughout sees the rewind happen and still never sees the ping.
func TestE2ETheLiveStreamNeverShowedTheKeepAliveTurnEither(t *testing.T) {
	// Arrange
	s := newKeepAliveSession(t, testKeepAlivePolicy(), withGatedKeepAlivePing())
	held := heldByKeepAlivePing(t, s, "live-stream-exclusion")

	// Assert — read live until the held prompt's own reply (which can only
	// arrive after the ping has ended and the rewind has run), rejecting any
	// item carrying the ping.
	reject := func(frame *frontendv1.FrontendFrame) string {
		for _, item := range deltaItems(frame, s.cwd) {
			if text := assistantText(item); text != "" && strings.Contains(text, keepAlivePingText) {
				return "a live ConversationDelta carried the keep-alive turn: " + item.GetUuid()
			}
		}
		return ""
	}
	awaitAll(t, s.conn, reject, map[string]func(*frontendv1.FrontendFrame) bool{
		"the held prompt's reply on the rewound conversation": func(frame *frontendv1.FrontendFrame) bool {
			for _, item := range deltaItems(frame, s.cwd) {
				if strings.Contains(assistantText(item), echoOf(held.text)) {
					return true
				}
			}
			return false
		},
	})
}

// awaitSessionRewound reads the rewound conversation until its SessionRewound
// record arrives, and returns it.
func awaitSessionRewound(t *testing.T, vendorSessionID string) *corev1.SessionRewound {
	t.Helper()
	tail := tailStore(t, vendorSessionID)
	ev := tail.await(t, "the SessionRewound record", func(ev *corev1.Event) bool {
		return ev.GetSessionRewound() != nil
	})
	return ev.GetSessionRewound()
}
