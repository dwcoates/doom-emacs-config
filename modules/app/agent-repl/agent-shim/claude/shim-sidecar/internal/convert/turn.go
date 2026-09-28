package convert

// turn.go — WHICH TURN A TRANSCRIPT RECORD BELONGS TO, stated only when the
// vendor's own records name it.
//
// Every entry the sidecar writes carries StoreEntry.turn when the turn is
// known, and NO turn otherwise — never a guess. The only turns this plane can
// name are the ones it MINTS: an adopted external prompt's turn is the prompt
// record's own uuid (user.go, externalPrompt) — for an edited or re-sent
// version of an unanswered prompt, the FIRST version's (resend.go). An
// agent-repl turn's id is the daemon's, and nothing the vendor writes carries
// it, so the records of such a turn are left unstamped here and the store keeps
// the stamp the stream plane wrote (the store's first-stamp rule).
//
// THE ATTRIBUTION IS THE TRANSCRIPT'S OWN STRUCTURE, the same links the
// keep-alive rule reads (keepalive.go), never arrival order:
//
//   - A record that OPENS an external prompt opens that turn, and its
//     `promptId` is remembered as the turn's.
//   - A USER record carrying a `promptId` belongs to that prompt's turn: the
//     external turn it names, or none when the prompt it names opened a turn
//     this plane cannot name (an agent-repl prompt) or predates this file's
//     window.
//   - A USER record that opens a prompt this plane does not emit (an agent-repl
//     prompt, a sidechain commission) opens a turn with no nameable id.
//   - EVERY OTHER RECORD FOLLOWS ITS PARENT (`parentUuid`); a record with no
//     parent belongs to no nameable turn.
//
// WHAT IT HOLDS: the uuids of records that belong to a nameable turn, and the
// promptIds of the prompts that opened one. An agent-repl session's file holds
// neither, so the memory is the adopted transcripts' alone.

import (
	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
)

// turnScope is the per-file memory the rule needs.
type turnScope struct {
	// records maps a record's uuid to the nameable turn it belongs to. Only
	// records of a nameable turn are kept; absence means none.
	records map[string]string
	// prompts maps a promptId to the nameable turn it opened, or to "" for a
	// prompt that opened a turn this plane cannot name.
	prompts map[string]string
}

func newTurnScope() turnScope {
	return turnScope{records: map[string]string{}, prompts: map[string]string{}}
}

// resolve answers the nameable turn one record belongs to, remembering it so the
// record's own children resolve in turn. `opened` is the turn the record's own
// conversion opened (an external prompt's uuid), or "" when it opened none.
func (s turnScope) resolve(f keepaliveFacts, opened string) string {
	var turn string
	switch {
	case opened != "":
		turn = opened
		if f.promptID != "" {
			s.prompts[f.promptID] = opened
		}
	case f.kind == "user" && f.promptID != "":
		known, seen := s.prompts[f.promptID]
		if !seen && f.prompt {
			// A prompt this plane did not emit: its turn has no id here, and
			// the records that carry its promptId have none either.
			s.prompts[f.promptID] = ""
		}
		turn = known
	case f.kind == "user" && f.prompt:
		// A prompt by its shape (an older CLI writes no promptId) that opened
		// no nameable turn.
		turn = ""
	case f.parent != "":
		turn = s.records[f.parent]
	}
	if turn != "" && f.uuid != "" {
		s.records[f.uuid] = turn
	}
	return turn
}

// stampTurn stamps every entry one record produced with that record's turn, at
// the one place all of a record's entries pass (Converter.Line). An unknown
// turn stamps nothing.
func stampTurn(entries []*storev1.StoreEntry, turn string) {
	if turn == "" {
		return
	}
	for _, entry := range entries {
		entry.Turn = &conversationv1.TurnId{Value: turn}
	}
}
