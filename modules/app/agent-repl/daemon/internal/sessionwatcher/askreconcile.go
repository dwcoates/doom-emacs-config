package sessionwatcher

import (
	"agentrepl/logging/logsite"
	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

// askreconcile.go — retiring an ask the views stand that the session no longer
// holds.
//
// AN ASK LIVES ONLY IN THE PERMISSION GATE OF THE SHIM THAT ASKED IT. When that
// shim dies without a teardown, nothing ever settles the ask, and every view
// that stood it (the feed's card, the footer, the sidebar, the merge sink, the
// roster's attention marker) goes on standing it: every answer is refused
// `no_open_ask`, and a typed prompt is routed to the dead ask, so the
// workspace can never be prompted again (2026-11-11,
// explanation-engine-remote-control).
//
// Two moments find the contradiction, and both settle the ask through
// settleAbandonedAskLocked, which routes an abandoned settle down exactly the
// path a live settle takes:
//
//   - AT ATTACH (retireAsksAbsentFromFactsLocked): the session's facts name
//     every ask the shim's gate holds (SessionStarted.open_asks), so an ask
//     the views stood before this watcher started that the facts do not name
//     is one no gate holds.
//   - ON A REFUSED ANSWER (AbandonRefusedAsk): the shim's `no_open_ask` for an
//     ask the feed still stands is the same contradiction, found late.

// AskKind names which identity space an ask's id lives in.
type AskKind string

const (
	// AskKindQuestion is a question batch (conversation.v1.AgentQuestionId).
	AskKindQuestion AskKind = "question"
	// AskKindPermission is a consent ask (conversation.v1.AgentPermissionId).
	AskKindPermission AskKind = "permission"
)

// openAskKey is the unseen-set key (askKey) of one ask as a SessionOpenAsk
// carries it, "" for one that is neither kind.
func openAskKey(ask *conversationv1.SessionOpenAsk) string {
	switch {
	case ask.GetQuestion() != nil:
		return askKey(string(AskKindQuestion), ask.GetQuestion().GetId().GetValue())
	case ask.GetPermission() != nil:
		return askKey(string(AskKindPermission), ask.GetPermission().GetId().GetValue())
	default:
		return ""
	}
}

// retireAsksAbsentFromFactsLocked settles, as abandoned, every ask the feed
// stood when this watcher started that the session's facts do not name open
// and the feed still stands. It runs ONCE, on the first facts, beside
// reconcileOpenAtAttachLocked, for the same reason: the facts describe the
// moment of attaching.
//
// ONLY THE ASKS STANDING BEFORE THIS WATCHER STARTED ARE JUDGED. The facts are
// a snapshot the shim took after this watcher began, so every ask in it began
// later than any ask in the snapshot taken at start; an ask the views stood
// after start may have opened after the facts' snapshot, and judging it here
// would abandon a live ask. Those reach the views through this watcher's own
// streams, and their own frames settle them.
func (w *watcher) retireAsksAbsentFromFactsLocked(open []*conversationv1.SessionOpenAsk) {
	before := w.asksBeforeAttach
	w.asksBeforeAttach = nil
	if len(before) == 0 {
		return
	}
	held := map[string]struct{}{}
	for _, ask := range open {
		held[openAskKey(ask)] = struct{}{}
	}
	standing := map[string]struct{}{}
	for _, ask := range w.sinks.Feed.AsksStanding(w.ws) {
		standing[openAskKey(ask)] = struct{}{}
	}
	for _, ask := range before {
		key := openAskKey(ask)
		if _, ok := held[key]; ok {
			continue
		}
		if _, ok := standing[key]; !ok {
			w.log.Debug("daemon.sessionwatcher.ask_settled_before_facts", "an ask standing when this watcher started settled before the facts arrived; nothing is retired", dlog.Context{
				"ask": key,
			})
			continue
		}
		w.settleAbandonedAskLocked(ask, "daemon.sessionwatcher.ask_abandoned_at_attach",
			logsite.Msg("an ask the views stand is not open in the session's facts: the session that asked it is gone, so it is settled abandoned"))
	}
}

// AbandonRefusedAsk settles, as abandoned, the ask of KIND under ID when the
// feed still stands it: the shim refused an answer to it `no_open_ask`, so no
// gate holds it, and the views are told the same thing the shim already knows.
// It reports whether a standing ask was settled; false means the feed no
// longer stands it (its own settle won the race), and nothing is touched.
func (w *watcher) AbandonRefusedAsk(kind AskKind, id string) bool {
	w.mu.Lock()
	defer w.mu.Unlock()
	key := askKey(string(kind), id)
	for _, ask := range w.sinks.Feed.AsksStanding(w.ws) {
		if openAskKey(ask) != key {
			continue
		}
		w.settleAbandonedAskLocked(ask, "daemon.sessionwatcher.ask_abandoned_on_refusal",
			logsite.Msg("the shim refused an answer to an ask the views stand, as no longer open: the session that asked it is gone, so it is settled abandoned"))
		return true
	}
	w.log.Debug("daemon.sessionwatcher.ask_abandoned_on_refusal", "the shim refused an answer to an ask the views no longer stand; nothing is retired", dlog.Context{
		"ask": key,
	})
	return false
}

// settleAbandonedAskLocked routes an abandoned settle of ASK (a start, as the
// feed stood it) down the path a live settle of it takes, recording the
// retirement at INFO under OPERATION.
func (w *watcher) settleAbandonedAskLocked(ask *conversationv1.SessionOpenAsk, operation string, message logsite.Message) {
	agent := ask.GetAgent()
	ctx := dlog.Context{"agent_id": agent.GetValue(), "ask": openAskKey(ask), "turn_in_flight": ids.Optional(w.turn)}
	switch {
	case ask.GetQuestion() != nil:
		start := ask.GetQuestion()
		ctx["question_id"] = start.GetId().GetValue()
		w.log.Log(dlog.LevelInfo, operation, message, ctx)
		w.routeQuestionLocked(agent, abandonedQuestion(start), nil, nil)
	case ask.GetPermission() != nil:
		start := ask.GetPermission()
		ctx["permission_id"] = start.GetId().GetValue()
		w.log.Log(dlog.LevelInfo, operation, message, ctx)
		w.routePermissionLocked(agent, abandonedPermission(start), nil, nil)
	default:
		panic("sessionwatcher: an ask the feed stands is neither a question nor a consent ask")
	}
}

// abandonedQuestion is the abandoned settle of the question START names,
// restating its batch, as the shim writes one.
func abandonedQuestion(start *conversationv1.AgentQuestion) *conversationv1.AgentQuestion {
	return &conversationv1.AgentQuestion{
		Id: start.GetId(),
		Result: &conversationv1.AgentQuestion_Success{Success: &conversationv1.AgentQuestionSuccess{
			Batch:   start.GetStart().GetBatch(),
			Outcome: &conversationv1.AgentQuestionSuccess_Abandoned{Abandoned: &conversationv1.AgentQuestionAbandoned{}},
		}},
	}
}

// abandonedPermission is the abandoned settle of the consent ask START names,
// as the shim writes one.
func abandonedPermission(start *conversationv1.AgentPermission) *conversationv1.AgentPermission {
	return &conversationv1.AgentPermission{
		Id:        start.GetId(),
		GatedCall: start.GetGatedCall(),
		Result: &conversationv1.AgentPermission_Success{Success: &conversationv1.AgentPermissionSuccess{
			Decision: &conversationv1.AgentPermissionSuccess_Abandoned{Abandoned: &conversationv1.AgentPermissionAbandoned{}},
		}},
	}
}
