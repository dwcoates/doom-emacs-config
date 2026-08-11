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
	// VerdictUninterruptibleTurn — no classifier ran and none will: the turn in
	// front of the entry is a CONTEXT CUT (`/compact` or `/clear`), which is
	// never interrupted for a queued prompt (uninterruptibleturn.go).
	//
	// It is its own answer for the reason ERROR is: HOLD would claim a model
	// weighed this prompt against the running turn and decided to wait, and
	// nothing of the kind happened. Delivery is the ordinary turn-end drain's,
	// so the verdict states WHO decided rather than WHEN the prompt runs.
	VerdictUninterruptibleTurn
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
	case VerdictUninterruptibleTurn:
		return "uninterruptible-turn"
	default:
		return "unknown"
	}
}

// setClassification stamps view's verdict arm off the in-memory entry e,
// carrying the facts that verdict owns and no others. accepted rides HOLD alone
// because confirming a hold is the only thing QueueAcceptCmd does, and the
// entry's one free-text account rides the field its verdict owns — a rationale
// on interject and hold, a detail on error.
//
// IT READS THE ENTRY RATHER THAN A PARAMETER LIST because a verdict's facts are
// the entry's: the caller used to spread them across five arguments and pass
// the same string twice, which is how a fact could be handed to the wrong arm.
//
// An unknown verdict stamps ERROR carrying its own name, rather than leaving
// the entry arm-less: an entry with no classification arm is a malformed frame
// the client rejects whole, which would cost the user the entire queue view
// over one unrecognized value.
func setClassification(entry *frontendv1.QueueEntry, e *queueEntry) {
	v, rationale, detail, accepted := e.classification, e.rationale, e.rationale, e.accepted
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
	case VerdictUninterruptibleTurn:
		// The COMMAND is the whole account this arm carries: the entry's
		// free-text rationale says the same thing in prose for the log, and a
		// card that read it from there would be parsing a sentence for a fact
		// the arm already states.
		entry.Classification = &frontendv1.QueueEntry_UninterruptibleTurn{
			UninterruptibleTurn: &frontendv1.QueueClassificationUninterruptibleTurn{
				Command: e.uninterruptibleCommand,
			},
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
//
// A HISTORY CONSUMER CARRIES THE PUBLISHED TOKEN INSTEAD OF COMPOSING ONE, and
// the distinction is the consumer's KIND rather than whether the field happens
// to be set — an absent published fence is an answer ("" is what an unwired
// workspace publishes), not a reason to fall back to composing.
//
// A consumer serving a history read runs under no controller of its own, so it
// has no generation to compose from and used to compose `Fence(session, "")` —
// the token "s_…|", which no WorkspaceState ever published and which therefore
// no client could ever match. Its every push was discarded whole by the fence
// gate, and the page, still missing what it had discarded, resynced again at
// the next heartbeat: the ~2Hz churn this field ends. The admission ladder had
// already resolved the workspace's AUTHORITATIVE fence to admit the request;
// carrying that same token verbatim is what makes producer and consumer agree
// by construction rather than by two compositions happening to coincide.
func (c *consumer) fence() string {
	if c.servesHistory {
		return c.publishedFence
	}
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
