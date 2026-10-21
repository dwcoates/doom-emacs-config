package feed

import (
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
)

// A VENDOR THAT SERVES AGAIN ANSWERS ITS OUTAGE. While the vendor is down
// (overloaded, its own internal error, a rate limit, a connection that never
// got an answer) every turn ends on a vendor-fault card, and agent-repl's
// retry makes a turn every 30 seconds (the shim's vendor-error retry), so the
// cards pile up. The moment a LATER turn shows the vendor serving again, those
// cards have been answered: the reader is told the outage is over by the
// cards leaving the feed, and a card still on screen would report a fault
// that no longer stands.
//
// ONLY A FAULT WAITING CURES IS ANSWERED (waitCuredEnding). A refusal of the
// request itself (a 400, an auth, permission or billing refusal, a model that
// does not exist, an output ceiling) stays: the vendor serving the next turn
// says nothing about it. An account cause has its own answer, a sign-in
// (signin.go).
//
// THE EVIDENCE IS IN THE FEED ITSELF (vendorServedRow): a row the model
// produced (a response, a tool call, a skill, a subagent, a plan, findings,
// an artifact) or a turn that concluded. A vendor notice, a broken response
// bubble, a hook and a merge bubble are not: the vendor's tooling wrote the
// first, the second is the failure itself, and agent-repl makes the last two.
//
// "LATER" IS FEED ORDER, NEVER ARRIVAL ORDER. A fault card is answered by
// evidence of another turn whose order key sorts after the card's own, which
// holds whichever arrives first: live, the evidence comes after; on a history
// replay both are redrawn and the rule runs again; on a page loaded on
// scroll-back the card arrives after the newer page's evidence, and the
// feed's newest evidence (feedState.served) refuses it. No record is kept on
// disk because none is needed: the rows that retire a card are the rows of
// the feed. A PINNED row is no evidence until it settles (order.go): its key
// sorts after every placed row however late that row arrives, so it would
// answer an outage that began after it.

const opVendorRecovered = "daemon.feed.vendor_recovered"

// servedEvidence is the newest row of a feed that showed the vendor serving:
// its placed order key and its turn.
type servedEvidence struct {
	key  string
	turn string
}

// waitCuredEnding reports whether ROW is a turn's vendor-fault ending whose
// cause waiting can cure: the outage a later turn's service answers.
func waitCuredEnding(row *frontendv1.FeedRow) bool {
	errored := row.GetTurnEnded().GetErrored()
	if errored.GetMarker().GetVendorFault() == nil {
		return false
	}
	switch errored.GetError().(type) {
	case *frontendv1.FeedTurnEndedErrored_RateLimited,
		*frontendv1.FeedTurnEndedErrored_Overloaded,
		*frontendv1.FeedTurnEndedErrored_Internal,
		*frontendv1.FeedTurnEndedErrored_VendorUnmodeled:
		return true
	default:
		return false
	}
}

// vendorServedRow reports whether ROW shows the vendor serving its turn.
func vendorServedRow(row *frontendv1.FeedRow) bool {
	if row.GetTurnEnded().GetConcluded() != nil {
		return true
	}
	unit := row.GetActivity()
	switch {
	case unit.GetResponse() != nil:
		response := unit.GetResponse()
		return response.GetNotice() == nil && response.GetError() == nil
	case unit.GetSimpleToolCall() != nil, unit.GetSkill() != nil, unit.GetSubagent() != nil,
		unit.GetPlan() != nil, unit.GetFindings() != nil, unit.GetArtifact() != nil:
		return true
	default:
		return false
	}
}

// settleVendorRecovery runs after row ID of feed ADDR is stored: evidence of
// the vendor serving retires every earlier outage card of another turn, and
// an outage card that sorts before the feed's newest evidence of another
// turn is retired as soon as it is drawn.
func (r *resolver) settleVendorRecovery(s *wsState, addr feedid.Feed, id string) {
	f := r.feed(s, addr)
	row, stored := f.rows[id]
	if !stored || row.GetTurn().GetValue() == "" {
		return
	}
	key := f.rank[id].key
	if isPinKey(key) {
		return
	}
	turn := row.GetTurn().GetValue()
	switch {
	case vendorServedRow(row):
		if key > f.served.key {
			f.served = servedEvidence{key: key, turn: turn}
		}
		var answered []string
		for _, other := range f.order {
			ending := f.rows[other]
			if !waitCuredEnding(ending) || ending.GetTurn().GetValue() == turn || f.rank[other].key >= key {
				continue
			}
			answered = append(answered, other)
		}
		for _, other := range answered {
			r.retire(s, addr, other)
		}
		if len(answered) > 0 {
			r.logger(s.id).Info(opVendorRecovered,
				"the vendor served a later turn; the outage's vendor-error cards before it were retired",
				dlog.Context{"feed": f.key, "evidence_row": id, "turn": turn, "retired": len(answered), "first": answered[0]})
		}
	case waitCuredEnding(row):
		if f.served.key <= key || f.served.turn == turn {
			return
		}
		r.retire(s, addr, id)
		r.logger(s.id).Info(opVendorRecovered,
			"a vendor-error card was drawn before the feed's newest service of a later turn; it was retired",
			dlog.Context{"feed": f.key, "row": id, "turn": turn, "evidence_key": f.served.key, "evidence_turn": f.served.turn})
	}
}
