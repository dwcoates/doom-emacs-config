package footer

import (
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/ladder"
	"agent-repld/internal/resolve/turnfault"
	"agent-repld/internal/wsm"
)

// THE TURN FAULT (owner ruling, 2026-10-06): a turn that ended abnormally puts
// its fault on the workspace status, and the activity cell carries the
// daemon's per-cause sentence — the same sentence the feed's turn-end row
// carries as its headline (resolve/turnfault). Every cause the vendor ended or
// refused is `vendor_fault · vendor_error` (or the standing account block's
// own step); the query or the agent process dying under the turn is
// `agent_repl_fault · turn_died`; an interrupt and an expected stop raise
// nothing. It stands until the next turn starts.
//
// THE FAULT IS RAISED FROM THE TURN'S CLOSE, NOT ITS TERMINAL. The roster
// raises its arm from the same close and the same class (SetTurnEnded,
// ladder.ResolveTurnFault), so the strip and the dot cannot disagree about a
// failed turn: the terminal and a query death only RECORD how the turn failed
// (pendingEnding), and the close the prompt queue's door reports is what turns
// that record into a fault.

// pendingEnding is how the turn in flight failed, recorded when its terminal
// (or the query's death) arrives, for the close to raise from.
type pendingEnding struct {
	// class is the failure's class (ladder.ClassifyFailure).
	class ladder.FailureClass
	// words is the per-cause account.
	words turnfault.Words
	// retryAt is when the vendor's stated wait ends, nil when it stated none.
	retryAt *time.Time
	// signInCures reports an account cause signing in again cures
	// (ladder.FailureCuredBySignIn).
	signInCures bool
}

// turnFaultState is the standing turn fault.
type turnFaultState struct {
	// fault is the domain whose machinery ended the turn. Never NoTurnFault.
	fault ladder.TurnFault
	// words is the per-cause account.
	words turnfault.Words
	// retryAt is when the vendor's stated wait ends, nil when it stated none.
	retryAt *time.Time
	// at is when the fault was raised.
	at time.Time
	// signInCures reports a fault a completed sign-in lifts (OnSignedIn).
	signInCures bool
}

// recordFailedEnding records how the turn in flight failed, from its own
// terminal. A failure with no fault of its own (an expected stop) records its
// class all the same, so the close reads it as one.
func (r *resolver) recordFailedEnding(s *wsState, failure *conversationv1.AgentFailure, refused bool) {
	ending := &pendingEnding{
		class:       ladder.ClassifyFailure(failure),
		words:       turnfault.OfAgentFailure(failure, refused),
		signInCures: ladder.FailureCuredBySignIn(failure),
	}
	if wait, ok := turnfault.RetryAfter(failure); ok {
		at := r.opts.clock.Now().Add(wait)
		ending.retryAt = &at
	}
	s.pendingEnding = ending
}

// recordQueryDeath records the query dying under the turn in flight.
func (s *wsState) recordQueryDeath(died *conversationv1.SessionQueryDied) {
	s.pendingEnding = &pendingEnding{class: ladder.AgentReplFailed, words: turnfault.OfQueryDeath(died)}
}

// SetTurnEnded takes the close the prompt queue's door recorded for the last
// turn — the very call the roster takes beside it — and raises the turn fault
// that close and the recorded failure resolve to (ladder.ResolveTurnFault).
func (r *resolver) SetTurnEnded(ws ids.WorkspaceID, how wsm.TurnClose) {
	r.mutate(ws, "daemon.footer.set_turn_ended", "the footer took the turn's close",
		dlog.Context{"close": how.String()}, func(s *wsState) {
			log := r.logOf(ws, s)
			ending := s.pendingEnding
			s.pendingEnding = nil
			// THE CLOSE ENDS THE TURN, as the roster's SetTurnEnded does: a
			// close no terminal preceded (the agent process dying, an orphan)
			// leaves no other edge that would.
			s.endTurn(r.opts.clock.Now())
			s.retrying = nil
			// A COMPACTION ENDS WITH ITS TURN, as the roster's does: the
			// vendor's own `compacting` clear may never follow a turn the
			// prompt queue closed.
			s.compacting = false
			class := ladder.NoFailure
			if ending != nil {
				class = ending.class
			}
			fault, known := ladder.ResolveTurnFault(how, class)
			if !known {
				log.Error("daemon.footer.set_turn_ended",
					"the footer took a turn close it has no turn fault for; no fault is raised",
					dlog.Context{
						"close":               how.String(),
						"class":               class.String(),
						"invariant_violation": "every turn close resolves to a turn fault",
						"remediation":         "add the close to ladder.ResolveTurnFault",
					})
				return
			}
			if fault == ladder.NoTurnFault {
				log.Debug("daemon.footer.turn_fault", "the turn's end raised no fault",
					dlog.Context{"close": how.String(), "class": class.String()})
				return
			}
			raised := &turnFaultState{fault: fault, at: r.opts.clock.Now()}
			switch {
			case ending != nil && class != ladder.NoFailure && how != wsm.CloseAgentDied:
				raised.words = ending.words
				raised.retryAt = ending.retryAt
				raised.signInCures = ending.signInCures
			default:
				// NO TERMINAL EXPLAINED THE CLOSE, or the agent process died
				// under the turn: the close is all there is to tell it by.
				words, failed := turnfault.OfClose(how)
				if !failed {
					log.Error("daemon.footer.set_turn_ended",
						"a turn fault was resolved for a close turnfault cannot word; no fault is raised",
						dlog.Context{
							"close":               how.String(),
							"fault":               fault.String(),
							"invariant_violation": "every close that raises a fault has words",
							"remediation":         "word the close in turnfault.OfClose",
						})
					return
				}
				raised.words = words
			}
			s.turnFault = raised
			log.Info("daemon.footer.turn_fault",
				"the turn's end raised a fault on the workspace status; it stands until the next turn starts",
				dlog.Context{
					"close": how.String(), "class": class.String(), "fault": fault.String(),
					"cause": raised.words.Cause, "line": raised.words.Sentence,
					"retry_at": raised.retryAt != nil,
				})
		})
}

// vendorTurnFault reports whether a vendor turn fault stands.
func (s *wsState) vendorTurnFault() bool {
	return s.turnFault != nil && s.turnFault.fault == ladder.VendorTurnFault
}

// agentReplTurnFault reports whether an agent-repl turn fault stands.
func (s *wsState) agentReplTurnFault() bool {
	return s.turnFault != nil && s.turnFault.fault == ladder.AgentReplTurnFault
}

// turnEndedLine is the standing turn fault's activity line.
func (s *wsState) turnEndedLine() *frontendv1.FooterStatusActivityTurnEnded {
	line := &frontendv1.FooterStatusActivityTurnEnded{Text: s.turnFault.words.Sentence}
	if s.turnFault.retryAt != nil {
		line.RetryAt = stamp(*s.turnFault.retryAt)
	}
	return line
}

// OnSignedIn takes a completed sign-in for the workspace's account root: the
// account failure it answers stops standing on the strip. A turn fault whose
// cause a sign-in cures, and a standing `auth` block, are lifted, so the
// status returns to whatever the rest of the workspace resolves to — `idle`
// for a session that is simply waiting. Every other fault stands.
func (r *resolver) OnSignedIn(ws ids.WorkspaceID) {
	r.mutate(ws, "daemon.footer.on_signed_in", "the footer took a completed sign-in",
		nil, func(s *wsState) {
			log := r.logOf(ws, s)
			fields := dlog.Context{
				"turn_fault_lifted": s.turnFault != nil && s.turnFault.signInCures,
				"auth_block_lifted": s.blocked != nil && s.blocked.kind == blockedAuth,
			}
			if s.turnFault != nil && s.turnFault.signInCures {
				s.turnFault = nil
			}
			if s.blocked != nil && s.blocked.kind == blockedAuth {
				s.blocked = nil
			}
			log.Info("daemon.footer.signed_in", "a completed sign-in lifted the account failure it answers", fields)
		})
}
