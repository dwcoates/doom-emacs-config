// WHERE THE CUT MAY FALL, and what happens when it may not fall anywhere.
//
// The rewind's whole safety argument is that the truncated transcript is a
// byte-identical PREFIX of what the vendor already has, cut at a real turn
// boundary. Every edge in this file is a shape of transcript where finding that
// boundary is not the obvious thing:
//
//   - A TOOL RESULT IS TYPED `user`. The CLI records a tool_result as a user
//     line, so "cut before the last user line" would slice a real turn in half,
//     leaving the vendor with a tool_use whose result had been amputated.
//   - A SIDECHAIN INTERLEAVES. Subagent traffic is written into the same file
//     as it happens, so the trailing records are not a single linear turn and
//     no cut can be shown to be a prefix. There is no safe truncation, so the
//     rewind is REFUSED — loudly — and the user's prompt is still delivered on
//     the un-rewound conversation, because a prompt is never the thing that
//     gets dropped when the daemon cannot do its housekeeping.
//   - A PING TURN WAS INTERRUPTED. Its trailing records are a partial turn with
//     no reply, which is exactly what the cut is supposed to remove.
//
// WHY THE TRANSCRIPTS ARE FIXTURES. These shapes are properties of the CLI's
// on-disk format, and a `--fake` run has no vendor writing that file at all
// (clearcompact_e2e_test.go's header). The fixtures are written at the path the
// daemon's own encoding resolves (session.TranscriptPath), under the harness's
// isolated $HOME, so the code under test reads them exactly where it reads a
// real one.
//
// ONE ASSUMPTION IS STATED RATHER THAN HIDDEN: subagent traffic is marked with
// `isSidechain` on the in-session lines. The sidecar also handles sidechains
// that live in their own agent-*.jsonl files; if the refusal keys off those
// instead, this fixture is the wrong shape and the test says so by failing.
package e2e

import (
	"encoding/json"
	"strings"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// --- transcript fixture lines ---------------------------------------------------

func transcriptJSON(obj map[string]any) string {
	raw, err := json.Marshal(obj)
	if err != nil {
		panic("e2e: encode transcript fixture line: " + err.Error())
	}
	return string(raw)
}

// userLine is an ordinary user prompt record.
func userLine(uuid, parent, vendorSessionID, text string) string {
	return transcriptJSON(map[string]any{
		"type":       "user",
		"uuid":       uuid,
		"parentUuid": parent,
		"sessionId":  vendorSessionID,
		"timestamp":  "2026-08-04T12:00:00.000Z",
		"message":    map[string]any{"role": "user", "content": text},
	})
}

// toolResultLine is a TOOL RESULT, which the CLI records as a `user` line. It
// is the record that makes "the last user line" the wrong cut point.
func toolResultLine(uuid, parent, vendorSessionID, toolUseID string) string {
	return transcriptJSON(map[string]any{
		"type":       "user",
		"uuid":       uuid,
		"parentUuid": parent,
		"sessionId":  vendorSessionID,
		"timestamp":  "2026-08-04T12:00:02.000Z",
		"message": map[string]any{"role": "user", "content": []any{map[string]any{
			"type": "tool_result", "tool_use_id": toolUseID, "content": "ok",
		}}},
	})
}

// assistantLine is an assistant reply record.
func assistantLine(uuid, parent, vendorSessionID, text string) string {
	return transcriptJSON(map[string]any{
		"type":       "assistant",
		"uuid":       uuid,
		"parentUuid": parent,
		"sessionId":  vendorSessionID,
		"timestamp":  "2026-08-04T12:00:01.000Z",
		"message": map[string]any{
			"role": "assistant", "type": "message", "id": "msg_" + uuid,
			"model":   "claude-opus-4-8",
			"content": []any{map[string]any{"type": "text", "text": text}},
		},
	})
}

// toolUseLine is an assistant record that CALLS a tool, so the tool_result
// user line that follows it belongs to the same turn.
func toolUseLine(uuid, parent, vendorSessionID, toolUseID string) string {
	return transcriptJSON(map[string]any{
		"type":       "assistant",
		"uuid":       uuid,
		"parentUuid": parent,
		"sessionId":  vendorSessionID,
		"timestamp":  "2026-08-04T12:00:01.500Z",
		"message": map[string]any{
			"role": "assistant", "type": "message", "id": "msg_" + uuid,
			"model": "claude-opus-4-8",
			"content": []any{map[string]any{
				"type": "tool_use", "id": toolUseID, "name": "Bash", "input": map[string]any{"command": "true"},
			}},
		},
	})
}

// sidechainLine is subagent traffic written into the session's own transcript.
func sidechainLine(uuid, parent, vendorSessionID, text string) string {
	return transcriptJSON(map[string]any{
		"type":        "assistant",
		"uuid":        uuid,
		"parentUuid":  parent,
		"sessionId":   vendorSessionID,
		"isSidechain": true,
		"timestamp":   "2026-08-04T12:00:03.000Z",
		"message": map[string]any{
			"role": "assistant", "type": "message", "id": "msg_" + uuid,
			"model":   "claude-opus-4-8",
			"content": []any{map[string]any{"type": "text", "text": text}},
		},
	})
}

// --- the tool-result boundary ------------------------------------------------------

// TestE2EARewindDoesNotCutAtTheToolResultUserLine covers THE `user`-TYPED TOOL
// RESULT: the last real turn used a tool, so it ends with an assistant reply
// that FOLLOWS a user-typed tool_result. A cut that treated the last user line
// as the boundary would leave the vendor holding a tool_use whose result had
// been amputated — a transcript the CLI cannot resume.
func TestE2EARewindDoesNotCutAtTheToolResultUserLine(t *testing.T) {
	// Arrange — a real tool-using turn, then a keep-alive turn behind it.
	s := newKeepAliveSession(t, testKeepAlivePolicy(), withGatedKeepAlivePing())
	const (
		realPrompt   = "real-user-prompt"
		toolUse      = "real-tool-use"
		toolResult   = "real-tool-result"
		realFinal    = "real-final-assistant"
		pingPrompt   = "ping-prompt"
		pingResponse = "ping-response"
	)
	writeFixtureTranscript(t, s.cwd, s.vendorID, []string{
		userLine(realPrompt, "", s.vendorID, "do the thing"),
		toolUseLine(toolUse, realPrompt, s.vendorID, "toolu_1"),
		toolResultLine(toolResult, toolUse, s.vendorID, "toolu_1"),
		assistantLine(realFinal, toolResult, s.vendorID, "done"),
		userLine(pingPrompt, realFinal, s.vendorID, keepAlivePingText),
		assistantLine(pingResponse, pingPrompt, s.vendorID, "."),
	})

	// Act
	rw := rewindByKeepAlivePing(t, s)

	// Assert — the cut kept the whole real turn, tool round-trip included.
	record := awaitSessionRewound(t, rw.next)
	switch got := record.GetRetainedLeafUuid(); got {
	case realFinal:
		// The last record of the last REAL turn: correct.
	case toolResult, toolUse:
		t.Errorf("the rewind retained up to %q — inside the real turn's tool round-trip. A tool_result is typed `user`, so cutting at \"the last user line\" slices a real turn in half and leaves the vendor a tool_use with no result", got)
	default:
		t.Errorf("the rewind retained up to %q, want the last real turn's final record %q", got, realFinal)
	}
}

// --- the sidechain interleave -------------------------------------------------------

// TestE2EASidechainInterleaveRefusesTheRewind covers NO SAFE CUT: with subagent
// records interleaved among the trailing ones, no truncation can be shown to be
// a prefix of what the vendor holds. The daemon refuses rather than guessing,
// and refusing means the conversation identity does not move.
func TestE2EASidechainInterleaveRefusesTheRewind(t *testing.T) {
	// Arrange
	s := newKeepAliveSession(t, testKeepAlivePolicy(), withGatedKeepAlivePing())
	writeFixtureTranscript(t, s.cwd, s.vendorID, sidechainInterleavedTranscript(s.vendorID))
	before := s.vendorID

	// Act — a ping, and a real prompt held by it whose delivery would rewind.
	held := heldByKeepAlivePing(t, s, "delivered-without-a-rewind")

	// Assert — the prompt ran, and the conversation identity never moved.
	awaitAll(t, s.conn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"the held prompt's own reply": func(frame *frontendv1.FrontendFrame) bool {
			for _, item := range deltaItems(frame, s.cwd) {
				if strings.Contains(assistantText(item), echoOf(held.text)) {
					return true
				}
			}
			return false
		},
	})
	if got := readSessionRow(t, s.h, s.sessionID).claudeSessionID; got != before {
		t.Errorf("the conversation identity moved to %q despite an interleaved sidechain making the cut unprovable (was %q): a rewind the daemon cannot show to be a prefix must be refused, not attempted", got, before)
	}
}

// TestE2EASidechainInterleaveStillDeliversThePrompt covers WHO PAYS for the
// refusal: nobody. Housekeeping the daemon cannot safely do is housekeeping it
// skips — the user's prompt is delivered on the un-rewound conversation, and
// the only cost is that the ping stays in the context until something else
// clears it.
func TestE2EASidechainInterleaveStillDeliversThePrompt(t *testing.T) {
	// Arrange
	s := newKeepAliveSession(t, testKeepAlivePolicy(), withGatedKeepAlivePing())
	writeFixtureTranscript(t, s.cwd, s.vendorID, sidechainInterleavedTranscript(s.vendorID))

	// Act
	held := heldByKeepAlivePing(t, s, "still-delivered-un-rewound")

	// Assert — the durable record of the delivery is in the ORIGINAL seq space,
	// which is the un-rewound conversation.
	s.store.await(t, "the held prompt's TurnStarted in the un-rewound conversation", func(ev *corev1.Event) bool {
		started := userTurnStart(ev)
		return started != nil && strings.Contains(started.GetPromptPreview(), held.text)
	})
}

// TestE2EARefusedRewindIsAnnouncedRatherThanSwallowed covers LOUDLY. A rewind
// the daemon declined is a state the operator must be able to see: the ping
// remains in the model's context, so every later turn silently costs more. The
// SessionRewound record is the only announcement channel the contract has, and
// a refusal must not produce one that claims a truncation happened.
func TestE2EARefusedRewindIsAnnouncedRatherThanSwallowed(t *testing.T) {
	// Arrange
	s := newKeepAliveSession(t, testKeepAlivePolicy(), withGatedKeepAlivePing())
	writeFixtureTranscript(t, s.cwd, s.vendorID, sidechainInterleavedTranscript(s.vendorID))

	// Act
	held := heldByKeepAlivePing(t, s, "refusal-is-announced")

	// Assert — no SessionRewound is written for a rewind that did not happen.
	// The held prompt's own turn is the sentinel: it is submitted by the same
	// step that would have written the record.
	s.store.awaitSentinel(t, "the held prompt's turn",
		func(ev *corev1.Event) string {
			if ev.GetSessionRewound() != nil {
				return "a SessionRewound was recorded for a rewind that was refused: the record claims a truncation the vendor transcript never received"
			}
			return ""
		},
		func(ev *corev1.Event) bool {
			started := userTurnStart(ev)
			return started != nil && strings.Contains(started.GetPromptPreview(), held.text)
		})
}

// sidechainInterleavedTranscript is a trailing region no cut can be proved a
// prefix of: a real turn, then a keep-alive turn with subagent records written
// among it.
func sidechainInterleavedTranscript(vendorSessionID string) []string {
	return []string{
		userLine("sc-real-prompt", "", vendorSessionID, "do the thing"),
		assistantLine("sc-real-reply", "sc-real-prompt", vendorSessionID, "done"),
		userLine("sc-ping-prompt", "sc-real-reply", vendorSessionID, keepAlivePingText),
		sidechainLine("sc-side-1", "sc-ping-prompt", vendorSessionID, "subagent thinking"),
		assistantLine("sc-ping-reply", "sc-ping-prompt", vendorSessionID, "."),
		sidechainLine("sc-side-2", "sc-ping-reply", vendorSessionID, "subagent still going"),
	}
}

// --- the interrupted ping -------------------------------------------------------------

// TestE2EATrailingInterruptedPingTurnIsDroppedByTheCut covers THE PARTIAL TURN:
// a ping that was interrupted leaves a user record with no reply behind it.
// That is precisely what the cut removes — and it must be removed, because a
// transcript whose last record is an unanswered prompt is the shape the vendor
// will treat as an outstanding request.
func TestE2EATrailingInterruptedPingTurnIsDroppedByTheCut(t *testing.T) {
	// Arrange — the ping turn is a prompt with nothing after it.
	s := newKeepAliveSession(t, testKeepAlivePolicy(), withGatedKeepAlivePing())
	const realFinal = "partial-real-reply"
	writeFixtureTranscript(t, s.cwd, s.vendorID, []string{
		userLine("partial-real-prompt", "", s.vendorID, "do the thing"),
		assistantLine(realFinal, "partial-real-prompt", s.vendorID, "done"),
		userLine("partial-ping-prompt", realFinal, s.vendorID, keepAlivePingText),
	})

	// Act
	rw := rewindByKeepAlivePing(t, s)

	// Assert
	record := awaitSessionRewound(t, rw.next)
	if got := record.GetRetainedLeafUuid(); got != realFinal {
		t.Errorf("the rewind retained up to %q, want the last real turn's reply %q: an interrupted ping leaves an unanswered prompt as the trailing record, and leaving it in place hands the vendor an outstanding request nobody made", got, realFinal)
	}
}

// TestE2EAMissingVendorTranscriptRefusesTheRewindRatherThanInventingOne covers
// the DEGENERATE input: there is nothing on disk to truncate. Producing a
// "truncated copy" of a transcript that does not exist would hand the vendor a
// conversation with no history at all, silently discarding everything the user
// has done — so this refuses, exactly as the sidechain case does.
func TestE2EAMissingVendorTranscriptRefusesTheRewindRatherThanInventingOne(t *testing.T) {
	// Arrange — a `--fake` session writes no transcript, which is the premise.
	s := newKeepAliveSession(t, testKeepAlivePolicy(), withGatedKeepAlivePing())
	if _, exists := readFixtureTranscript(t, s.cwd, s.vendorID); exists {
		t.Fatalf("premise not established: a transcript already exists for %s", s.vendorID)
	}
	before := s.vendorID

	// Act
	held := heldByKeepAlivePing(t, s, "no-transcript-to-truncate")

	// Assert — the prompt is still delivered, on the identity it was already on.
	s.store.await(t, "the held prompt's TurnStarted in the un-rewound conversation", func(ev *corev1.Event) bool {
		started := userTurnStart(ev)
		return started != nil && strings.Contains(started.GetPromptPreview(), held.text)
	})
	if got := readSessionRow(t, s.h, s.sessionID).claudeSessionID; got != before {
		t.Errorf("the conversation identity moved to %q with no transcript to truncate (was %q): the vendor would resume a conversation with no history", got, before)
	}
}
