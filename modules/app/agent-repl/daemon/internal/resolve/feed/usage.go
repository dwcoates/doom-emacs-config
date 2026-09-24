package feed

import (
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/figures"

	"google.golang.org/protobuf/proto"
)

// THE RESPONSE'S COST CORNER IS THIS TURN'S OWN WORK — NOT THE CONTEXT WINDOW.
// Two different figures are drawn from the same wire counters:
//   - The TOPBAR shows the CONTEXT WINDOW: a standing sum that grows as context
//     is (re-)cached and reused. That is a DIFFERENT resolver; nothing here
//     touches it.
//   - A response BUBBLE is TURN-SPECIFIC: the tokens THAT TURN actually
//     generated. So the stamp counts only fresh new input (input_tokens =
//     InputMisses.Unwritten) plus output (output_tokens), and it EXCLUDES the
//     two cached-context buckets:
//       * InputHits.Read (cache_read_input_tokens) — cached context REUSED, not
//         produced this turn; a turn that reuses a huge context did little work.
//       * InputMisses.Written (cache_creation_input_tokens) — context being
//         (re-)cached; again the context window growing, not the turn's work.
//
// Usage still rides EXACTLY ONE unit per API response — the unit for the
// response's FIRST content block, which for the vendor's observed
// `[thinking, text]` shape is the THINKING unit, not the prose one. So the
// grouping-by-API-response stays: every activity is filed under the API
// response it arrived in, and each response's turn-scoped tokens are recorded
// when its usage-carrying unit lands. But the STAMP a bubble draws is the SUM
// of those tokens across every API response of the SAME TURN — a tool-use loop
// produces several — and it resets per turn because each response is attributed
// to the turn it was filed under. When a later API response grows the turn's
// total, the turn's EARLIER bubbles are re-pushed so every bubble of a turn
// shows the same turn total (see restampTurnBubbles).

// fileAPIResponse files one unit under the API response it arrived in,
// attributes that response to the turn in flight, and records the response's
// turn-scoped token count when the unit is the one carrying its usage. It
// reports the turn the recorded usage belongs to and whether a usage figure was
// recorded, so the caller can re-stamp that turn's bubbles.
//
// A unit arriving WITH usage opens a new response; every unit after it, until
// the next usage-carrying one, belongs to that same response. Units seen
// before any usage sit in response zero. A unit is filed ONCE: its later
// frames restate the same unit and must never re-open a response, though a
// unit that states its usage on a later frame stamps the response it was
// already filed under.
func (s *wsState) fileAPIResponse(unit string, usage *conversationv1.TokenUsage) (turn string, recorded bool) {
	response, filed := s.unitAPIResponse[unit]
	if !filed {
		if usage != nil {
			s.apiResponseSeq++
		}
		response = s.apiResponseSeq
		s.unitAPIResponse[unit] = response
	}
	// Attribute the response to the turn it is being drawn under — the same
	// turn rows themselves are stamped with (turnStamp), set on the prompt that
	// opened the turn both live and on replay. Recorded on the FIRST filing so
	// the response's turn is known even before its usage frame arrives, and
	// never re-attributed afterward: a response keeps the turn it opened under.
	if _, attributed := s.apiResponseTurn[response]; !attributed && s.rowTurn() != nil {
		s.apiResponseTurn[response] = string(*s.rowTurn())
	}
	if usage == nil {
		return "", false
	}
	// TURN-SCOPED TOKENS: fresh new input plus output only. The two cached-
	// context buckets are the context window, not this turn's work, so they are
	// deliberately excluded (see the file header).
	misses := usage.GetInputMisses()
	s.apiResponseTurnTokens[response] = misses.GetUnwritten() + usage.GetOutputTokens()
	return s.apiResponseTurn[response], true
}

// turnTokenTotal sums the turn-scoped tokens of every API response belonging to
// the given turn, reporting whether any of them stated usage.
func (s *wsState) turnTokenTotal(turn string) (total uint64, stated bool) {
	for resp, tokens := range s.apiResponseTurnTokens {
		if s.apiResponseTurn[resp] != turn {
			continue
		}
		total += tokens
		stated = true
	}
	return total, stated
}

// turnStampText is the formatted turn total, empty when the turn stated no
// usage — absence draws no stamp, never a zero.
func (s *wsState) turnStampText(turn string) string {
	total, stated := s.turnTokenTotal(turn)
	if !stated {
		return ""
	}
	return figures.Tokens(total)
}

// apiResponseStamp answers the formatted stamp a unit's bubble shows: the SUM
// of turn-scoped tokens across every API response belonging to the SAME TURN as
// this unit's response. Empty when that turn has stated no usage. Summing by
// turn is inherently reset per turn: a response filed under a later turn never
// rolls into an earlier turn's total.
func (s *wsState) apiResponseStamp(unit string) string {
	response, filed := s.unitAPIResponse[unit]
	if !filed {
		return ""
	}
	return s.turnStampText(s.apiResponseTurn[response])
}

// restampTurnBubbles re-pushes every already-drawn response bubble of a turn
// with the turn's current total, so a bubble drawn BEFORE a later API response
// arrived grows to show the whole turn's tokens — every bubble of a turn shows
// the same turn total. A thinking bubble carries no stamp and is skipped; the
// prompt row and other kinds carry no response and are skipped too. upsert
// itself drops an unchanged row, so a bubble already at the current total is
// not re-published.
func (r *resolver) restampTurnBubbles(s *wsState, turn string) {
	stamp := s.turnStampText(turn)
	if stamp == "" {
		return
	}
	for key, f := range s.feeds {
		for _, id := range f.order {
			row := f.rows[id]
			resp := row.GetActivity().GetResponse()
			if resp == nil || resp.GetThinking() {
				continue
			}
			if row.GetTurn().GetValue() != turn {
				continue
			}
			if resp.GetUsage().GetText() == stamp {
				continue
			}
			restamped, ok := proto.Clone(row).(*frontendv1.FeedRow)
			if !ok {
				r.logger(s.id).Error("daemon.feed.row_not_clonable",
					"a response bubble could not be snapshotted to re-stamp its turn total",
					dlog.Context{"feed": f.key, "row": id})
				continue
			}
			bubble := restamped.GetActivity().GetResponse()
			// Keep the settled instant; only the figure grows.
			bubble.Usage = &frontendv1.FeedResponseUsageStamp{
				Text: stamp,
				AtMs: resp.GetUsage().GetAtMs(),
			}
			r.upsert(s, placement{feed: s.feedAddrs[key]}, restamped, true)
		}
	}
}
