package feed

import (
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
)

// A CARD WHOSE CAUSE IS ANSWERED LEAVES THE FEED. The vendor reports trouble
// two ways: a turn's vendor-fault ending, and a vendor notice drawn on its own
// (the prose the vendor's tooling writes into the conversation, notice.go).
// While the vendor is down every turn ends on a card, and agent-repl's retry
// makes a turn every 30 seconds (the shim's vendor-error retry), so the cards
// pile up. The moment the cause is answered, the reader is told so by the
// cards leaving the feed: a card still on screen would report a fault that no
// longer stands.
//
// ONE RULE SAYS WHAT ANSWERS WHICH CARD (cureOf), keyed on the notice's
// subject as its producer classified it, which the row itself carries
// (frontend.v1.FeedResponseNotice.synthesized), and on the ending's error arm:
//
//   - trouble waiting cures (an overload, the API's internal error, a rate
//     limit, a connection that never got an answer), as an ending or a
//     notice: any later service by the vendor;
//   - an exhausted allowance: any later service by the vendor;
//   - the account refused: a completed sign-in (signin.go), or any later
//     service by the vendor, which covers a refresh the vendor made itself;
//   - a turn's ending that offers the sign-in action: a completed sign-in;
//   - a refused request and an unclassified notice: nothing, so they stay.
//
// THE EVIDENCE OF SERVICE IS IN THE FEED ITSELF (vendorServedRow): a row the
// model produced (a response, a tool call, a skill, a subagent, a plan,
// findings, an artifact) or a turn that concluded. A vendor notice, a broken
// response bubble, a hook and a merge bubble are not: the vendor's tooling
// wrote the first, the second is the failure itself, and agent-repl makes the
// last two. Another turn's evidence answers a card, and so does a row the
// model produced later in the card's OWN turn (the vendor recovered inside the
// turn); the card's own turn concluding is no evidence, since a turn the
// vendor failed can still conclude on the vendor's words alone.
//
// "LATER" IS FEED ORDER, NEVER ARRIVAL ORDER. A card is answered by evidence
// whose order key sorts after the card's own, which holds whichever arrives
// first: live, the evidence comes after; on a history replay both are redrawn
// and the rule runs again; on a page loaded on scroll-back the card arrives
// after the newer page's evidence, and the feed's newest evidence
// (feedState.served) refuses it. No record is kept on disk because none is
// needed: the rows that retire a card are the rows of the feed. A PINNED row
// is no evidence until it settles (order.go): its key sorts after every placed
// row however late that row arrives, so it would answer an outage that began
// after it. A sign-in's answer is the one recorded durably (signin.go).

const (
	opVendorRecovered = "daemon.feed.vendor_recovered"
	opCardAnswered    = "daemon.feed.card_answered"
)

// servedEvidence is a row of a feed that showed the vendor serving: its row,
// its placed order key and its turn.
type servedEvidence struct {
	row  string
	key  string
	turn string
}

// cardCure is what answers a card: the subject its log records name, and
// whether a later service by the vendor, a completed sign-in, or both answer
// it.
type cardCure struct {
	subject string
	service bool
	signIn  bool
}

// cureOf is THE retirement rule: what answers ROW, and whether ROW is a card
// anything answers at all.
func cureOf(row *frontendv1.FeedRow) (cardCure, bool) {
	if notice := row.GetActivity().GetResponse().GetNotice(); notice != nil {
		subject := notice.GetSynthesized()
		switch subject.GetSubject().(type) {
		case *conversationv1.AgentResponseSynthesizedNotice_VendorTransient,
			*conversationv1.AgentResponseSynthesizedNotice_UsageLimit:
			return cardCure{subject: noticeSubject(subject), service: true}, true
		case *conversationv1.AgentResponseSynthesizedNotice_Account:
			return cardCure{subject: noticeSubject(subject), service: true, signIn: true}, true
		}
		return cardCure{}, false
	}
	if kind, ok := waitCuredEnding(row); ok {
		return cardCure{subject: "ending_" + kind, service: true}, true
	}
	if offersSignIn(row) {
		return cardCure{subject: "ending_sign_in", signIn: true}, true
	}
	return cardCure{}, false
}

// waitCuredEnding reports whether ROW is a turn's vendor-fault ending whose
// cause waiting can cure, and names the cause.
func waitCuredEnding(row *frontendv1.FeedRow) (string, bool) {
	errored := row.GetTurnEnded().GetErrored()
	if errored.GetMarker().GetVendorFault() == nil {
		return "", false
	}
	switch errored.GetError().(type) {
	case *frontendv1.FeedTurnEndedErrored_RateLimited:
		return "rate_limited", true
	case *frontendv1.FeedTurnEndedErrored_Overloaded:
		return "overloaded", true
	case *frontendv1.FeedTurnEndedErrored_Internal:
		return "internal", true
	case *frontendv1.FeedTurnEndedErrored_Unreachable:
		return "unreachable", true
	case *frontendv1.FeedTurnEndedErrored_VendorUnmodeled:
		return "vendor_unmodeled", true
	default:
		return "", false
	}
}

// vendorServedRow reports whether ROW shows the vendor serving its turn.
func vendorServedRow(row *frontendv1.FeedRow) bool {
	return row.GetTurnEnded().GetConcluded() != nil || modelProducedRow(row)
}

// modelProducedRow reports whether ROW is something the model produced: the
// evidence that answers a card of the row's own turn.
func modelProducedRow(row *frontendv1.FeedRow) bool {
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

// answersCard reports whether EVIDENCE, a row showing the vendor serving that
// sorts after the card, answers a card of CARDTURN: another turn's service
// does, and so does the model producing a row in the card's own turn.
func answersCard(evidence *frontendv1.FeedRow, cardTurn string) bool {
	return evidence.GetTurn().GetValue() != cardTurn || modelProducedRow(evidence)
}

// retireAnswered retires card ID of feed ADDR, whose cause EVIDENCE answered,
// and logs the retirement at INFO: the card's subject, the card, its turn, and
// what answered it. It reports whether a row left the feed.
func (r *resolver) retireAnswered(s *wsState, addr feedid.Feed, id string, card *frontendv1.FeedRow, cure cardCure, evidence dlog.Context) bool {
	if !r.retire(s, addr, id) {
		return false
	}
	r.logAnswered(s, addr, id, card, cure, evidence)
	return true
}

// logAnswered is the one INFO record of a card retired because its cause was
// answered: the card's subject, the card, its turn, and what answered it (a
// service's row and turn, or a sign-in).
func (r *resolver) logAnswered(s *wsState, addr feedid.Feed, id string, card *frontendv1.FeedRow, cure cardCure, evidence dlog.Context) {
	fields := dlog.Context{"feed": r.feed(s, addr).key, "subject": cure.subject, "row": id, "turn": card.GetTurn().GetValue()}
	for k, v := range evidence {
		fields[k] = v
	}
	r.logger(s.id).Info(opCardAnswered, "a card whose cause was answered was retired from the feed", fields)
}

// serviceEvidence is the log context naming a row of service as the answer.
func serviceEvidence(e servedEvidence) dlog.Context {
	return dlog.Context{"answered_by": "service", "evidence_row": e.row, "evidence_turn": e.turn}
}

// settleVendorRecovery runs after row ID of feed ADDR is stored: evidence of
// the vendor serving retires every earlier card service answers, and a card
// service answers that sorts before evidence the feed already holds is
// retired as soon as it is drawn.
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
	if vendorServedRow(row) {
		evidence := servedEvidence{row: id, key: key, turn: turn}
		if key > f.served.key {
			f.served = evidence
		}
		if modelProducedRow(row) && key > f.servedByModel.key {
			f.servedByModel = evidence
		}
		var answered []string
		for _, other := range f.order {
			card := f.rows[other]
			cure, ok := cureOf(card)
			if !ok || !cure.service || f.rank[other].key >= key || !answersCard(row, card.GetTurn().GetValue()) {
				continue
			}
			answered = append(answered, other)
		}
		retired := 0
		for _, other := range answered {
			card := f.rows[other]
			cure, _ := cureOf(card)
			if r.retireAnswered(s, addr, other, card, cure, serviceEvidence(evidence)) {
				retired++
			}
		}
		if retired > 0 {
			r.logger(s.id).Info(opVendorRecovered,
				"the vendor served again; the cards its service answers before it were retired",
				dlog.Context{"feed": f.key, "evidence_row": id, "turn": turn, "retired": retired, "first": answered[0]})
		}
		return
	}
	cure, ok := cureOf(row)
	if !ok || !cure.service {
		return
	}
	evidence, found := f.answeringEvidence(key, turn)
	if !found {
		return
	}
	if r.retireAnswered(s, addr, id, row, cure, serviceEvidence(evidence)) {
		r.logger(s.id).Info(opVendorRecovered,
			"a card was drawn before service the feed already holds; it was retired",
			dlog.Context{"feed": f.key, "row": id, "turn": turn, "evidence_key": evidence.key, "evidence_turn": evidence.turn})
	}
}

// answeringEvidence is the evidence F holds that answers a card of TURN at
// KEY: the newest service of another turn, or the newest row the model
// produced, when it sorts after the card.
func (f *feedState) answeringEvidence(key, turn string) (servedEvidence, bool) {
	if f.served.key > key && f.served.turn != turn {
		return f.served, true
	}
	if f.servedByModel.key > key {
		return f.servedByModel, true
	}
	return servedEvidence{}, false
}
