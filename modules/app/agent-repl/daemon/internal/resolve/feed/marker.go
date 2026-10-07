package feed

import (
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/resolve/ladder"
	"claude-repld/internal/resolve/turnfault"
	"claude-repld/internal/wsm"
)

// THE OUTCOME MARKER (owner ruling, 2026-10-06; feed.proto FeedOutcomeMarker):
// the ONE drawing for an event that is not a message — how a turn ended, a
// permission the user denied, a plan episode that broke. The daemon composes
// every word of it and every line its fault arms expand to; the client draws
// it verbatim. Its FAMILY follows ladder.ResolveTurnFault, the table the
// footer and the roster raise their fault from, so the marker's color is the
// workspace status's color for the same end.

// The marker labels. One spelling each, read by every builder below.
const (
	markerLabelVendor      = "vendor error"
	markerLabelAgentRepl   = "agent-repl"
	markerLabelInterrupted = "interrupted"
	markerLabelPlanFailed  = "plan failed"
	markerLabelDeniedByYou = "permission denied by you"
	// markerLabelCompactionFailed is a failed compaction's label.
	markerLabelCompactionFailed = "compaction failed"
	// compactionFailedType is a failed compaction's error type: the
	// producer's own word for the cut (conversation.v1 ContextCut).
	compactionFailedType = "compaction_failed"
)

// neutralMarker is a marker of the user's own act or a hook's. It carries no
// expansion: the neutral family has none to carry.
func neutralMarker(label, detail string) *frontendv1.FeedOutcomeMarker {
	m := &frontendv1.FeedOutcomeMarker{
		Label:  &frontendv1.FeedOutcomeMarkerLabel{Text: label},
		Family: &frontendv1.FeedOutcomeMarker_Neutral{Neutral: &frontendv1.FeedOutcomeMarkerNeutral{}},
	}
	if detail != "" {
		m.Detail = &frontendv1.FeedOutcomeMarkerDetail{Text: detail}
	}
	return m
}

// interruptedMarker is the stop's neutral marker.
func interruptedMarker() *frontendv1.FeedOutcomeMarker {
	return neutralMarker(markerLabelInterrupted, "")
}

// planFailedMarker is the broken plan episode's neutral marker, its composed
// reason the detail.
func planFailedMarker(reason string) *frontendv1.FeedOutcomeMarker {
	return neutralMarker(markerLabelPlanFailed, reason)
}

// deniedByYouMarker is the user's denial's neutral marker.
func deniedByYouMarker() *frontendv1.FeedOutcomeMarker {
	return neutralMarker(markerLabelDeniedByYou, "")
}

// ending is everything a turn's ending marker is composed from.
type ending struct {
	// turn is the turn that ended.
	turn ids.TurnID
	// fault is the domain whose machinery ended it (ladder.ResolveTurnFault).
	fault ladder.TurnFault
	// words is how the end is told (turnfault).
	words turnfault.Words
	// endedAtMs is when it ended: the expansion's time line.
	endedAtMs int64
	// vendorMessage is the vendor's own message, "" when it recorded none.
	vendorMessage string
	// retryAfter is the vendor's stated wait, nil when it stated none.
	retryAfter *time.Duration
	// signIn reports an account cause the existing login flow can cure.
	signIn bool
	// died is what died, for an agent-repl fault: nil otherwise.
	died *frontendv1.FeedOutcomeWhatDied
}

// endingOfFailure gathers a terminal failure's ending. A failure ENDS its turn
// on a failed close, which is the close ladder.ResolveTurnFault reads a
// terminal's class on — the very resolution the footer makes.
func endingOfFailure(turn ids.TurnID, endedAtMs int64, failure *conversationv1.AgentFailure, words turnfault.Words, vendorMessage string) ending {
	fault, _ := ladder.ResolveTurnFault(wsm.CloseFailed, ladder.ClassifyFailure(failure))
	e := ending{turn: turn, fault: fault, words: words, endedAtMs: endedAtMs, vendorMessage: vendorMessage, signIn: curedBySignIn(failure)}
	if wait, ok := turnfault.RetryAfter(failure); ok {
		e.retryAfter = &wait
	}
	if died, ok := failure.GetFailure().(*conversationv1.AgentFailure_QueryDied); ok {
		e.died = queryDiedWhat(died.QueryDied)
	}
	return e
}

// endingOfQueryDeath gathers a query death's ending.
func endingOfQueryDeath(turn ids.TurnID, endedAtMs int64, died *conversationv1.SessionQueryDied) ending {
	return ending{
		turn: turn, fault: ladder.AgentReplTurnFault, words: turnfault.OfQueryDeath(died),
		endedAtMs: endedAtMs, died: queryDiedWhat(died),
	}
}

// queryDiedWhat is the expansion's what-died line for a query death.
func queryDiedWhat(died *conversationv1.SessionQueryDied) *frontendv1.FeedOutcomeWhatDied {
	query := &frontendv1.FeedOutcomeQueryDied{
		Line: &frontendv1.FeedOutcomeDeathLine{Text: turnfault.OfQueryDeath(died).Sentence},
	}
	if thrown := turnfault.QueryDeathThrown(died); thrown != "" {
		query.Thrown = &frontendv1.FeedOutcomeThrown{Text: thrown}
	}
	return &frontendv1.FeedOutcomeWhatDied{What: &frontendv1.FeedOutcomeWhatDied_Query{Query: query}}
}

// processDiedWhat is the expansion's what-died line for an agent process death.
func processDiedWhat(words turnfault.Words) *frontendv1.FeedOutcomeWhatDied {
	return &frontendv1.FeedOutcomeWhatDied{What: &frontendv1.FeedOutcomeWhatDied_Process{
		Process: &frontendv1.FeedOutcomeProcessDied{Line: &frontendv1.FeedOutcomeDeathLine{Text: words.Sentence}},
	}}
}

// curedBySignIn reports an account cause signing in again can cure: the
// credential rejected, or the organization not allowed — the causes the
// footer's block reads as `auth`.
func curedBySignIn(failure *conversationv1.AgentFailure) bool {
	api, ok := failure.GetFailure().(*conversationv1.AgentFailure_ApiRequestFailed)
	if !ok {
		return false
	}
	switch api.ApiRequestFailed.GetKind().(type) {
	case *conversationv1.ApiRequestFailed_AuthenticationFailed,
		*conversationv1.ApiRequestFailed_OauthOrgNotAllowed:
		return true
	}
	return false
}

// endingMarker composes a turn ending's marker: neutral for an end that raises
// no fault, a fault family with its expansion otherwise. Only a field whose
// source exists is set.
func (r *resolver) endingMarker(s *wsState, e ending) *frontendv1.FeedOutcomeMarker {
	log := r.logger(s.id)
	switch e.fault {
	case ladder.NoTurnFault:
		log.Debug("daemon.feed.outcome_marker", "a turn's ending drew a neutral marker",
			dlog.Context{"turn": string(e.turn), "cause": e.words.Cause})
		return neutralMarker(e.words.Detail, "")
	case ladder.VendorTurnFault:
		expansion := &frontendv1.FeedOutcomeVendorFaultExpansion{
			Time:      &frontendv1.FeedOutcomeTime{AtMs: e.endedAtMs},
			ErrorType: &frontendv1.FeedOutcomeVendorErrorType{Text: e.words.Cause},
		}
		if e.vendorMessage != "" {
			expansion.Message = &frontendv1.FeedOutcomeVendorMessage{Text: e.vendorMessage}
		}
		if n := retriesMade(s.turnEvidence[string(e.turn)]); n > 0 {
			expansion.Retries = &frontendv1.FeedOutcomeRetriesMade{Count: n}
		}
		if e.retryAfter != nil {
			expansion.RetryAt = &frontendv1.FeedOutcomeRetryAt{AtMs: e.endedAtMs + e.retryAfter.Milliseconds()}
		}
		if live := s.plane == planeLive; live && s.model != "" {
			expansion.Model = &frontendv1.FeedOutcomeModel{Name: s.model}
		}
		if live := s.plane == planeLive; live && s.accountKnown {
			expansion.Account = &frontendv1.FeedOutcomeAccount{Email: s.account}
		}
		if e.signIn {
			expansion.SignIn = &frontendv1.FeedOutcomeSignIn{}
		}
		expansion.Resend = r.resendFor(s, e.turn)
		log.Debug("daemon.feed.outcome_marker", "a turn's ending drew a vendor-fault marker",
			dlog.Context{
				"turn": string(e.turn), "cause": e.words.Cause,
				"message": expansion.Message != nil, "retries": expansion.Retries.GetCount(),
				"retry_at": expansion.RetryAt != nil, "model": expansion.Model != nil,
				"account": expansion.Account != nil, "sign_in": e.signIn, "resend": expansion.Resend != nil,
			})
		return &frontendv1.FeedOutcomeMarker{
			Label:  &frontendv1.FeedOutcomeMarkerLabel{Text: markerLabelVendor},
			Detail: &frontendv1.FeedOutcomeMarkerDetail{Text: e.words.Detail},
			Family: &frontendv1.FeedOutcomeMarker_VendorFault{VendorFault: &frontendv1.FeedOutcomeMarkerVendorFault{Expansion: expansion}},
		}
	default:
		died := e.died
		if died == nil {
			died = processDiedWhat(e.words)
		}
		expansion := &frontendv1.FeedOutcomeAgentReplFaultExpansion{
			Time:     &frontendv1.FeedOutcomeTime{AtMs: e.endedAtMs},
			WhatDied: died,
			Resend:   r.resendFor(s, e.turn),
		}
		log.Debug("daemon.feed.outcome_marker", "a turn's ending drew an agent-repl-fault marker",
			dlog.Context{"turn": string(e.turn), "cause": e.words.Cause, "resend": expansion.Resend != nil})
		return &frontendv1.FeedOutcomeMarker{
			Label:  &frontendv1.FeedOutcomeMarkerLabel{Text: markerLabelAgentRepl},
			Detail: &frontendv1.FeedOutcomeMarkerDetail{Text: e.words.Detail},
			Family: &frontendv1.FeedOutcomeMarker_AgentReplFault{AgentReplFault: &frontendv1.FeedOutcomeMarkerAgentReplFault{Expansion: expansion}},
		}
	}
}

// retriesMade is how many retries the vendor announced for a turn's failed
// calls: the highest attempt its retry schedules named (conversationv1.ApiRetry
// counts the retry it makes next, so the highest one announced is how many it
// made). Zero when none was announced.
func retriesMade(lines []turnEvidenceLine) uint32 {
	var most uint32
	for _, line := range lines {
		if line.apiFailure && line.retryAttempt > most {
			most = line.retryAttempt
		}
	}
	return most
}

// resendFor is the resend action, when the turn produced nothing and its
// prompt as said is known; nil otherwise.
func (r *resolver) resendFor(s *wsState, turn ids.TurnID) *frontendv1.FeedOutcomeResend {
	said := s.promptSaid[turn]
	if said == nil || s.turnProduced[turn] {
		return nil
	}
	return &frontendv1.FeedOutcomeResend{Said: said}
}

// noteProduced records that a row other than the turn's prompt and its ending
// was drawn for the turn: the turn produced something, so its ending offers
// no resend.
func (s *wsState) noteProduced(row *frontendv1.FeedRow) {
	turn := row.GetTurn().GetValue()
	if turn == "" {
		return
	}
	switch row.GetRow().(type) {
	case *frontendv1.FeedRow_UserPrompt, *frontendv1.FeedRow_TurnEnded:
		return
	}
	s.turnProduced[ids.TurnID(turn)] = true
}

// awaitRestart records an agent-repl-fault ending drawn LIVE, so the session's
// next start can say on it that the restart that followed worked.
func (s *wsState) awaitRestart(at placement, row *frontendv1.FeedRow) {
	if s.plane != planeLive {
		return
	}
	if row.GetTurnEnded().GetErrored().GetMarker().GetAgentReplFault() == nil {
		return
	}
	s.awaitingRestart = append(s.awaitingRestart, restartWatch{at: at, row: row})
}

// restartWatch is one agent-repl-fault ending waiting on the session's next
// start.
type restartWatch struct {
	at  placement
	row *frontendv1.FeedRow
}

// OnSessionStarted takes the session's start: the model it runs, and the edge
// on which every agent-repl-fault ending drawn before it learns the restart
// that followed it worked.
func (r *resolver) OnSessionStarted(ws ids.WorkspaceID, started *conversationv1.SessionStarted) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	if model := started.GetEffectiveModel().GetName(); model != "" {
		s.model = model
	}
	// THE CONTRACT DECIDES WHAT A LIVE SET MEANS. Under the live-work level a
	// bubble the set does not name is not running; before it, the set is only
	// the announcements' account. A new SessionStarted is judged afresh: no
	// set has been published against it yet.
	s.levelMode = started.GetContract() >= conversationv1.SessionContract_SESSION_CONTRACT_LIVE_WORK_LEVEL
	s.levelKnown = false
	watches := s.awaitingRestart
	s.awaitingRestart = nil
	now := r.deps.Now().UnixMilli()
	for _, watch := range watches {
		expansion := watch.row.GetTurnEnded().GetErrored().GetMarker().GetAgentReplFault().GetExpansion()
		expansion.Restarted = &frontendv1.FeedOutcomeRestarted{AtMs: now}
		r.logger(ws).Info("daemon.feed.outcome_restarted",
			"the session started again after an agent-repl fault ended a turn; its marker says the restart worked",
			dlog.Context{"turn": watch.row.GetTurn().GetValue(), "vendor_session_id": started.GetVendorSessionId()})
		r.upsert(s, watch.at, watch.row, true)
	}
	r.logger(ws).Debug("daemon.feed.session_started", "the feed took a session start",
		dlog.Context{"model": s.model, "restarts_told": len(watches)})
}

// SetAccount installs the account the workspace's session spends as: the
// logged-in email, "" when the root is logged out.
func (r *resolver) SetAccount(ws ids.WorkspaceID, email string) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	s.account = email
	s.accountKnown = email != ""
	r.logger(ws).Debug("daemon.feed.set_account", "the feed took the session's account",
		dlog.Context{"logged_in": s.accountKnown})
}

// modelOf takes up a session update's model change.
func (s *wsState) modelOf(update *conversationv1.SessionUpdate) {
	if model := update.GetModelChanged().GetEffectiveModel().GetName(); model != "" {
		s.model = model
	}
}

// compactionFailedMarker is a failed compaction's marker: a vendor fault,
// expanding to the producer's account. The cut states no instant, so the time
// line is set only for one drawn live, where receipt is when it happened; a
// replayed one carries none rather than its replay's.
func (r *resolver) compactionFailedMarker(s *wsState, reason string) *frontendv1.FeedOutcomeMarker {
	expansion := &frontendv1.FeedOutcomeVendorFaultExpansion{
		ErrorType: &frontendv1.FeedOutcomeVendorErrorType{Text: compactionFailedType},
	}
	if s.plane == planeLive {
		expansion.Time = &frontendv1.FeedOutcomeTime{AtMs: r.deps.Now().UnixMilli()}
		if s.model != "" {
			expansion.Model = &frontendv1.FeedOutcomeModel{Name: s.model}
		}
		if s.accountKnown {
			expansion.Account = &frontendv1.FeedOutcomeAccount{Email: s.account}
		}
	}
	if reason != "" {
		expansion.Message = &frontendv1.FeedOutcomeVendorMessage{Text: reason}
	}
	return &frontendv1.FeedOutcomeMarker{
		Label:  &frontendv1.FeedOutcomeMarkerLabel{Text: markerLabelCompactionFailed},
		Family: &frontendv1.FeedOutcomeMarker_VendorFault{VendorFault: &frontendv1.FeedOutcomeMarkerVendorFault{Expansion: expansion}},
	}
}
