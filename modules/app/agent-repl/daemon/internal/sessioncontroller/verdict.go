package sessioncontroller

import (
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/errclass"
	"claude-repld/internal/ssm"
)

// Verdict is the queue classifier's answer as the DAEMON holds it, in the
// daemon's own memory.
//
// The wire carries the same four answers as oneof arms on QueueEntry, each
// carrying its own facts (a rationale on interject and hold, `accepted` on hold,
// a detail on error). A queue entry in memory is mutated through its lifetime —
// pending, then classified, then re-stamped pending when a hold releases — and
// a plain value is what that mutation wants; the arm is minted once, at the
// moment the entry becomes a view.
type Verdict int

const (
	// VerdictPending — the classifier has not answered yet.
	VerdictPending Verdict = iota
	// VerdictInterject — deliver now: interrupt the running turn.
	VerdictInterject
	// VerdictHold — deliver when the turn ends on its own.
	VerdictHold
	// VerdictError — the classifier could not be believed. It is its own
	// answer, never folded into HOLD: a frontend must be able to see that
	// nothing decided this. Delivery still falls back to the turn-end drain, so
	// the prompt is delayed rather than dropped.
	VerdictError
)

func (v Verdict) String() string {
	switch v {
	case VerdictPending:
		return "pending"
	case VerdictInterject:
		return "interject"
	case VerdictHold:
		return "hold"
	case VerdictError:
		return "error"
	default:
		return "unknown"
	}
}

// setClassification stamps an entry's verdict arm, carrying the facts that
// verdict owns and no others. accepted rides HOLD alone because confirming a
// hold is the only thing QueueAcceptCmd does, and detail rides ERROR alone
// because it is the account of a failure no other verdict had.
//
// An unknown verdict stamps ERROR carrying its own name, rather than leaving
// the entry arm-less: an entry with no classification arm is a malformed frame
// the client rejects whole, which would cost the user the entire queue view
// over one unrecognized value.
func setClassification(entry *frontendv1.QueueEntry, v Verdict, rationale, detail string, accepted bool) {
	switch v {
	case VerdictPending:
		entry.Classification = &frontendv1.QueueEntry_Pending{
			Pending: &frontendv1.QueueClassificationPending{},
		}
	case VerdictInterject:
		entry.Classification = &frontendv1.QueueEntry_Interject{
			Interject: &frontendv1.QueueClassificationInterject{Rationale: rationale},
		}
	case VerdictHold:
		entry.Classification = &frontendv1.QueueEntry_HoldForTurnEnd{
			HoldForTurnEnd: &frontendv1.QueueClassificationHold{
				Rationale: rationale,
				Accepted:  accepted,
			},
		}
	case VerdictError:
		entry.Classification = &frontendv1.QueueEntry_Error{
			Error: &frontendv1.QueueClassificationError{Detail: detail},
		}
	default:
		entry.Classification = &frontendv1.QueueEntry_Error{
			Error: &frontendv1.QueueClassificationError{
				Detail: "the daemon held an unrecognized queue verdict " + v.String(),
			},
		}
	}
}

// fence is this consumer's STALENESS TOKEN, stamped on every per-workspace push
// it produces. It is minted from the same two identities the SSM projects onto
// WorkspaceState, through the same function, so a push and the state a client
// compares it against can never disagree about what "current" means.
func (c *consumer) fence() string {
	return ssm.Fence(c.sessionID, c.generationID)
}

// failureType names a card's classified failure for a log line. A card whose
// kind the daemon cannot name reads as "unclassified_kind" rather than as
// nothing — an unnamed failure in a record is exactly the case worth seeing.
func failureType(card *frontendv1.FailureCardView) string {
	t, ok := errclass.TypeOf(card.GetKind())
	if !ok {
		return "unclassified_kind"
	}
	return string(t)
}

// hasTurnResult reports whether a delta carries the turn's terminal result
// emission — the item a turn's accounting record belongs to.
func hasTurnResult(cd *frontendv1.ConversationDelta) bool {
	for _, item := range cd.GetItems() {
		if item.GetAgent().GetTurnResult() != nil {
			return true
		}
	}
	return false
}
