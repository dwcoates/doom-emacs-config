package feed

import (
	"context"
	"sort"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/health"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// FINAL ANSWER: LANDED, NOT LANDED, NOT TIMELY.
//
// A turn's terminal NAMES the response that answered it, and the feed draws
// that row with the green final-answer border. Three outcomes, and only the
// first is silent:
//
//  1. LANDED. The terminal names an answer activity id AND this resolver
//     resolves it to a drawn, non-thinking response row. That row is published
//     final_answer=true, live and on replay (recordFinalAnswer /
//     restampFinalAnswer).
//  2. NOT LANDED. The terminal arrives and either names NO answer while this
//     turn drew response prose, or names an answer no drawn row resolves. The
//     daemon records it at ERROR and raises the `final_answer_unresolved`
//     fault, which stands on the footer until the NEXT TURN STARTS.
//  3. NOT TIMELY. An open response fold has had no frame and no terminal for
//     the stall window. Same fault kind, `why` = "stalled", cleared the instant
//     a frame or the turn's terminal arrives.
//
// THE FAULT IS THE ONLY SURFACE. The bubble is never marked: the prose on
// screen is exactly what the agent said, and a turn whose answer the workspace
// cannot POINT AT is a fact about the workspace, not about the prose. It reaches
// the footer as the standing fault's activity line, through the same chip every
// other fault kind uses.
//
// IT IS NON-ESCALATING, and that is a decision made once in health/footer.go:
// the session is serving, and `disconnected` would close the composer over a
// session that is perfectly healthy.

// The `why` vocabulary — SessionFaultFinalAnswerUnresolved.why, spelled here
// once and carried as the fault record's own evidence.
const (
	// whyNoAnswerNamed is a terminal that concluded naming no answer at all
	// while the turn had drawn response prose.
	whyNoAnswerNamed = "no_answer_named"
	// whyAnswerRowUnresolved is a terminal whose named answer resolves to no
	// drawn response row.
	whyAnswerRowUnresolved = "answer_row_unresolved"
	// whyStalled is an open response fold that went silent.
	whyStalled = "stalled"
)

// DefaultAnswerStall is how long an OPEN response fold may go with neither a
// frame nor a terminal before the turn is called not timely. The owner's
// window; it is injectable only so a test can name its own.
const DefaultAnswerStall = 90 * time.Second

// Timer is one scheduled stall window. Stop cancels it, reporting whether it
// had not yet fired.
type Timer interface {
	Stop() bool
}

// FaultRecorder is the daemon's fault record, as this resolver needs it. It is
// the SAME state client every other raise site shares — the one decorated by
// health.ObserveFaults — so a fault opened here reaches the footer by the one
// path every fault reaches it by, rather than by plumbing beside this raise.
type FaultRecorder interface {
	// OpenFault records a fault that now stands.
	OpenFault(ctx context.Context, f wsm.Fault) (ids.FaultID, error)
	// CloseFault retracts one by id.
	CloseFault(ctx context.Context, id ids.FaultID, at time.Time) error
	// OpenFaults lists the standing faults in scope, which is what the
	// turn-started recovery edge reads to close every fault that ends there.
	OpenFaults(ctx context.Context, scope wsm.FaultScope) ([]wsm.Fault, error)
}

// answerFaultState is the ONE final-answer fault standing for a workspace.
// One, because the reader is told a fact about the workspace's latest turn and
// a queue of them would say nothing more.
type answerFaultState struct {
	// id is the record's id, which is what closes it again.
	id ids.FaultID
	// why is the vocabulary value the standing fault carries.
	why string
	// unit is the activity id it was raised about.
	unit string
	// openedAt is when it was raised, which is what a close measures how
	// long it stood against.
	openedAt time.Time
}

// stallState is one armed stall window. The sequence is what makes a timer
// that has already fired — and is blocked on the resolver's mutex while a
// frame disarms it — recognise that it is no longer the armed one.
type stallState struct {
	timer Timer
	seq   uint64
}

// answerFaultLine is the TERSE line the footer's fault chip draws beside the
// kind, one per `why`. It is short because the chip is one elastic cell on a
// one-line strip; the explanatory sentence is the ERROR record's, not the
// reader's.
func (r *resolver) answerFaultLine(why string) string {
	switch why {
	case whyNoAnswerNamed:
		return "the turn named no answering response"
	case whyAnswerRowUnresolved:
		return "the named answer has no drawn row"
	case whyStalled:
		return "no response frame for " + r.deps.AnswerStall.String()
	}
	return why
}

// raiseAnswerFault records a turn whose answer did not land, at ERROR, and
// opens the standing fault the footer draws. A fault already standing for the
// same unit and the same reason is left exactly as it is, so a terminal that
// replays across store planes neither doubles the record nor moves the line's
// age.
//
// `message` is the ERROR record's explanatory sentence; the footer's line is
// composed from `why` by answerFaultLine, so the strip stays terse and the log
// stays readable without either wording the other.
func (r *resolver) raiseAnswerFault(s *wsState, turn, unit, why, message string) {
	log := r.logger(s.id)
	log.Error("daemon.feed.final_answer_unresolved", message,
		dlog.Context{"turn": turn, "unit": unit, "why": why})
	if s.answerFault != nil && s.answerFault.why == why && s.answerFault.unit == unit {
		log.Debug("daemon.feed.final_answer_fault_already_standing",
			"the same final-answer fault already stands; the record and its age are left alone",
			dlog.Context{"turn": turn, "unit": unit, "why": why})
		return
	}
	// A DIFFERENT final-answer fault stands: it is superseded, not accumulated.
	r.closeAnswerFault(s, health.EdgeSuperseded, "a later final-answer fault superseded it")
	if r.deps.Faults == nil {
		log.Debug("daemon.feed.final_answer_fault_unrecorded",
			"no fault recorder is wired into the feed resolver; the fault reaches no footer",
			dlog.Context{"turn": turn, "unit": unit, "why": why})
		return
	}
	ws := s.id
	line := r.answerFaultLine(why)
	openedAt := r.deps.Now()
	id, err := r.deps.Faults.OpenFault(context.Background(), wsm.Fault{
		Workspace: &ws,
		Kind:      health.KindFinalAnswerUnresolved,
		Detail:    message,
		Evidence:  map[string]string{"turn": turn, "unit": unit, "why": why, "detail": line},
		OpenedAt:  openedAt,
	})
	if err != nil {
		log.Error("daemon.feed.final_answer_fault_unopenable",
			"the final-answer fault could not be recorded; the footer carries no line for it",
			dlog.Context{"turn": turn, "unit": unit, "why": why, "cause": err.Error()})
		return
	}
	s.answerFault = &answerFaultState{id: id, why: why, unit: unit, openedAt: openedAt}
}

// closeAnswerFault retracts the standing final-answer fault, if one stands,
// on the recovery edge that ended it. The close goes through the health
// package's one door (health.CloseFaultOn), which refuses an edge the kind
// does not declare and records the close at INFO; a close that fails is ERROR
// there, and the footer may keep drawing the fault.
func (r *resolver) closeAnswerFault(s *wsState, edge health.Edge, because string) {
	fault := s.answerFault
	if fault == nil {
		return
	}
	s.answerFault = nil
	if r.deps.Faults == nil {
		return
	}
	ws := s.id
	health.CloseFaultOn(context.Background(), r.deps.Faults,
		r.logger(s.id).With(dlog.Context{"why": fault.why, "because": because}), edge, wsm.Fault{
			ID:        fault.id,
			Workspace: &ws,
			Kind:      health.KindFinalAnswerUnresolved,
			OpenedAt:  fault.openedAt,
		}, r.deps.Now())
}

// clearStalledAnswerFault retracts a standing fault ONLY when it is the stall
// raised about THIS fold: a stall is answered by the frame that finally arrived
// on the very fold that went silent, and a sibling fold moving says nothing
// about it. The two NOT-LANDED faults are never cleared here — they are raised
// at the terminal and stand until the next turn starts.
func (r *resolver) clearStalledAnswerFault(s *wsState, unit, because string) {
	if s.answerFault == nil || s.answerFault.why != whyStalled || s.answerFault.unit != unit {
		return
	}
	r.closeAnswerFault(s, health.EdgeAnswerArrived, because)
}

// clearTurnStalledAnswerFault is the terminal's clearing: the turn ended, so
// every fold of it is answered, including the one the stall was raised about.
func (r *resolver) clearTurnStalledAnswerFault(s *wsState, turn, because string) {
	if s.answerFault == nil || s.answerFault.why != whyStalled {
		return
	}
	fold, ok := s.responses[s.answerFault.unit]
	if !ok || fold.turn != turn {
		return
	}
	r.closeAnswerFault(s, health.EdgeAnswerArrived, because)
}

// turnStarted is what every turn-start site calls: the standing final-answer
// fault is a statement about the turn that just ended, and the next turn
// beginning is what retires it.
func (r *resolver) turnStarted(s *wsState, turn ids.TurnID) {
	r.closeAnswerFault(s, health.EdgeTurnStarted, "the next turn started: "+string(turn))
}

// liveTurnStarted is the turn-started recovery edge for a turn the daemon
// OPENED, never one replayed from history: every standing fault of the
// workspace whose lifetime ends at the next turn (health/lifetime.go) is
// closed, the conversation-level ones (an abandoned conversation, a failed
// classifier run) with the final-answer one. A replayed turn retires only
// the final-answer fault this resolver tracks, because replaying a
// conversation's history proves nothing about it now.
func (r *resolver) liveTurnStarted(s *wsState, turn ids.TurnID) {
	r.turnStarted(s, turn)
	if r.deps.Faults == nil {
		return
	}
	ws := s.id
	health.CloseOnEdge(context.Background(), r.deps.Faults,
		r.logger(s.id).With(dlog.Context{"turn": string(turn)}), health.EdgeTurnStarted,
		health.EdgeScope{Workspace: &ws}, r.deps.Now())
}

// armAnswerStall starts (or restarts) one response fold's stall window. Every
// frame for the fold rearms it, so the window is always measured from the LAST
// thing that happened rather than from the fold opening.
func (r *resolver) armAnswerStall(s *wsState, unit, turn string) {
	r.disarmAnswerStall(s, unit)
	if r.deps.AfterFunc == nil {
		return
	}
	s.stallSeq++
	seq := s.stallSeq
	ws := s.id
	timer := r.deps.AfterFunc(r.deps.AnswerStall, func() {
		r.answerStallFired(ws, unit, turn, seq)
	})
	s.stalls[unit] = &stallState{timer: timer, seq: seq}
}

// disarmAnswerStall cancels one fold's stall window.
func (r *resolver) disarmAnswerStall(s *wsState, unit string) {
	armed, ok := s.stalls[unit]
	if !ok {
		return
	}
	armed.timer.Stop()
	delete(s.stalls, unit)
}

// disarmTurnStalls cancels every stall window a turn had open. The terminal
// answers all of them at once: no fold of a turn that ended is still expecting
// a frame.
func (r *resolver) disarmTurnStalls(s *wsState, turn string) {
	for unit, fold := range s.responses {
		if fold.turn == turn {
			r.disarmAnswerStall(s, unit)
		}
	}
}

// answerStallFired is the stall window elapsing. It runs on the clock's own
// goroutine, so it re-reads the state under the resolver's mutex and raises
// only when the window it was armed for is STILL the armed one — a frame that
// disarmed it while this call waited on the mutex is the answer to the stall,
// and firing anyway would report a fold that is moving as silent.
func (r *resolver) answerStallFired(ws ids.WorkspaceID, unit, turn string, seq uint64) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s, ok := r.workspaces[ws]
	if !ok {
		return
	}
	armed, ok := s.stalls[unit]
	if !ok || armed.seq != seq {
		return
	}
	delete(s.stalls, unit)
	r.raiseAnswerFault(s, turn, unit, whyStalled,
		"an open response has had no frame and no terminal for the stall window")
}

// turnDrewProse answers whether this turn drew any response prose at all, and
// the unit of the fold that did. A DIRECTIVE's response draws no bubble and
// folds no markdown, so a /clear or /compact turn concluding with no answer is
// not a turn that lost one.
//
// The lowest unit id among the candidates is answered rather than whichever the
// map yields first, so the fault's evidence is the same on two runs of the same
// conversation.
func (r *resolver) turnDrewProse(s *wsState, turn string) (string, bool) {
	var candidates []string
	for unit, fold := range s.responses {
		if fold.turn != turn || fold.markdown == "" || s.directiveUnits[unit] {
			continue
		}
		if fold.notice {
			// A VENDOR NOTICE IS NOT AN ANSWER. The vendor wrote it in the
			// shape of prose (a turn cut off by an unreachable API ends on
			// "API Error: …" and nothing else), the feed already draws it as a
			// notice, and a turn left with nothing else never had an answer to
			// lose.
			r.logger(s.id).Debug("daemon.feed.final_answer_notice_not_prose",
				"a vendor-synthesized notice drawn in this turn is not answer prose; it owes no final answer",
				dlog.Context{"turn": turn, "unit": unit})
			continue
		}
		candidates = append(candidates, unit)
	}
	if len(candidates) == 0 {
		return "", false
	}
	sort.Strings(candidates)
	return candidates[0], true
}
