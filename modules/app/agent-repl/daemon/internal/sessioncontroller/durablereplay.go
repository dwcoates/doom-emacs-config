package sessioncontroller

import (
	"context"
	"fmt"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/errclass"
	"claude-repld/internal/ssm"
	"claude-repld/internal/statedb"
	"claude-repld/internal/storehistory"

	"google.golang.org/protobuf/proto"
)

// This file serves a frontend resync for a workspace that has NO live session
// controller.
//
// THE GAP IT CLOSES. Manager.Resync's live path replays the retained ring and
// closes the remainder through the shim (repull.go). Both halves need a session
// controller, so a workspace without one used to have its conversation replay
// SKIPPED — quietly, one layer up in the frontend command handler. The observed
// symptom is a reloaded webview with a correct footer and an EMPTY feed: the
// daemon had bounced, nothing had brought the session back up, and the whole
// conversation was still sitting in the store.
//
// WHY IT DOES NOT BRING A SESSION UP. A frontend mounting is not a reason to
// spawn a vendor process. Bring-up is what the user's first prompt and an
// explicit workspace open are for; a read must cost a read.
//
// WHY IT IS NOT A FALLBACK. See storehistory's package header: an unwired
// workspace is not a shim outage but the resting state of every workspace after
// a daemon bounce, and for it the store is not a SECOND route to the history —
// it is the ONLY one. Nothing is being masked, because there is no live route
// whose failure could be hidden.
//
// WHAT IT REPRODUCES EXACTLY. The events are pushed through the SAME
// consumer.pushConversation the live ring replay and the shim re-pull use, so
// the translation, the curation passes, the ConversationDelta.through_seq
// stamping, and the provenance verdict are one implementation rather than two
// that must be kept in agreement. Provenance in particular is read from the
// merge lease's DURABLE ledger by each item's own timestamp
// (StateApplier.ConversationSourceAt), so a merge-window item replays as
// CONVERSATION_SOURCE_MERGE long after the lease was released.

// DurableHistorySource replays a session's PERSISTED conversation events with
// no shim in the loop. Satisfied by *storehistory.Reader.
//
// fromSeq is EXCLUSIVE (Subscribe.from_seq); toSeq is an EXCLUSIVE upper bound
// with 0 meaning "until the history drains"; maxEvents caps one replay, and a
// tripped cap comes back as a TRUNCATED result rather than a quiet short answer.
type DurableHistorySource interface {
	ReplayHistory(ctx context.Context, workspace, sessionID string, fromSeq, toSeq uint64, maxEvents uint32, onEvent func(*corev1.Event)) (storehistory.Result, error)
}

// resyncFromDurableHistory serves a resync for a workspace with no live session
// controller, straight from the durable store.
//
// SEQ AND FLOOR SEMANTICS ARE THE LIVE PATH'S, UNCHANGED. The replay starts at
// max(fromSeq, newestClearOrCompactSeq) INCLUSIVE — the same replayFloor rule,
// applied here against the DURABLE last_seen_seq alone, because an unwired
// workspace has no retained ring to raise that ceiling. The inclusive floor is
// then converted to the store's exclusive from_seq by exclusiveLowerBound, so a
// replay floored at a clear or a compaction still replays that event itself.
// There is no upper bound: with no ring there is no live window for the replay
// to stop short of, so the whole conversation above the floor is served.
//
// EVERY FAILURE IS LOUD. A missing session record, an unwired durable source,
// an unreadable store, and a truncated replay all return an error, so the
// resync's CommandAck reports the failure. Silence here would be
// indistinguishable from an empty conversation, which is the bug.
// promptReceiptStaleAfterMs is the age past which a served prompt receipt is
// reported as STALE.
//
// Twenty-four hours, and the number is a diagnostic threshold rather than an
// expiry: nothing is withheld for being old, because a receipt is the only
// evidence its prompt was ever sent and discarding evidence for being
// inconvenient is the failure this mechanism exists to end. What the bound
// buys is a name for an anomaly. A receipt survives exactly as long as the
// conversation takes to carry its prompt, which is normally seconds and at
// worst the length of one turn; a day means something never retired it — a
// transcript that never arrived, a session abandoned mid-submit, a retirement
// that has been failing — and that deserves to be findable in the log rather
// than replayed in silence forever.
const promptReceiptStaleAfterMs int64 = 24 * 60 * 60 * 1000

func (m *Manager) resyncFromDurableHistory(workspace string, fromSeq uint64) error {
	logf := dlog.Tag(dlog.Logf(m.logf), "ws", workspace, "from_seq", fromSeq, "source", "shim-store")
	logf("session-controller: resync for an UNWIRED workspace — serving the conversation replay from DURABLE history (no session controller is live, and none is brought up to answer a read)")
	if m.cfg.DurableHistory == nil {
		logf("session-controller: durable resync REFUSED — no DurableHistorySource is wired, so the workspace's stored conversation cannot be read at all")
		return fmt.Errorf("session-controller: resync for unwired ws %q cannot be served: no durable history source is wired", workspace)
	}
	sessionID, ok := m.cfg.Locator.Locate(workspace)
	if !ok {
		logf("session-controller: durable resync REFUSED — the workspace resolves to no session record, so there is no conversation to locate in the store")
		return fmt.Errorf("session-controller: resync for unwired ws %q cannot be served: %w", workspace, errclass.ErrNoLiveSessionController)
	}
	lastSeen := m.cfg.SeqStore.LastSeq(sessionID)
	replayFrom := m.replayFloorAt(workspace, sessionID, lastSeen, fromSeq)
	cons := m.durableConsumer(workspace, sessionID)
	if err := m.hydratePersistedAccounting(cons, sessionID); err != nil {
		return err
	}
	// What the store's OWN events serve, watched as they are pushed, so a
	// receipt for a prompt the conversation already carries is suppressed
	// rather than drawn beside it (serveDurableReceipts).
	served := newServedPrompts()
	cons.onPushedConversation = served.observe
	logf("session-controller: durable resync replaying ws=%q session=%s replay_from=%d (inclusive) last_seen_seq=%d max_events=%d",
		workspace, sessionID, replayFrom, lastSeen, repullMaxEvents)
	res, err := m.cfg.DurableHistory.ReplayHistory(m.rootCtx, workspace, sessionID,
		exclusiveLowerBound(replayFrom), 0, repullMaxEvents,
		func(ev *corev1.Event) { cons.pushConversation(ev, false) })
	if err != nil {
		logf("session-controller: durable resync FAILED ws=%q session=%s replay_from=%d delivered=%d: %v",
			workspace, sessionID, replayFrom, res.Delivered, err)
		return fmt.Errorf("session-controller: durable resync for ws %q (replay_from=%d) failed: %w", workspace, replayFrom, err)
	}
	if res.Truncated {
		logf("session-controller: durable resync TRUNCATED ws=%q session=%s replay_from=%d delivered=%d first_seq=%d last_seq=%d reason=%q",
			workspace, sessionID, replayFrom, res.Delivered, res.FirstSeq, res.LastSeq, res.Reason)
		return fmt.Errorf("%w: ws=%q replay_from=%d delivered %d event(s) from durable history: %s",
			ErrRepullTruncated, workspace, replayFrom, res.Delivered, res.Reason)
	}
	logf("session-controller: durable resync COMPLETE ws=%q session=%s replay_from=%d events_served=%d first_seq=%d last_seq=%d",
		workspace, sessionID, replayFrom, res.Delivered, res.FirstSeq, res.LastSeq)
	// AFTER the store's own events, never before: a receipt belongs at the
	// bottom of the conversation, because the prompt it stands for is the most
	// recent thing that happened to this workspace.
	if err := m.serveDurableReceipts(workspace, sessionID, cons, served); err != nil {
		return err
	}
	// LAST, and the unwired stamp with it: a terminal refusal is the most
	// recent thing that happened to a fenced workspace, and it is the reason
	// nothing is driving the conversation the replay just served.
	fenced, err := m.serveStandingTerminalCard(workspace, sessionID, cons)
	if err != nil {
		return err
	}
	return m.stampDurableReplayUnwired(workspace, sessionID, fenced)
}

// serveStandingTerminalCard pushes the session's STANDING terminal failure card
// onto a durable replay, and reports whether one stood.
//
// THIS IS THE HALF THAT WAS MISSING. The fence publishes its card live, and a
// live push reaches only the clients connected at that instant; every later
// webview redials and is served from here. Without this the replay handed the
// user the whole conversation with NO account of why it stopped — indis-
// tinguishable from a session that is merely quiet.
//
// The card is pushed under the SAME uuid the live push used, so a client that
// saw both updates one item rather than drawing two accounts of one failure.
func (m *Manager) serveStandingTerminalCard(workspace, sessionID string, cons *consumer) (bool, error) {
	logf := dlog.Tag(dlog.Logf(m.logf), "ws", workspace, "session", sessionID, "source", "terminal-card")
	if m.cfg.TerminalFailureCards == nil {
		logf("session-controller: standing terminal failure card NOT served — no TerminalFailureCardStore is wired, so a terminally fenced session's account of itself cannot be recovered")
		return false, nil
	}
	rec, standing, err := m.cfg.TerminalFailureCards.Standing(sessionID)
	if err != nil {
		logf("session-controller: standing terminal failure card UNREADABLE session=%s: %v — the resync is failed rather than served without the one item that explains it", sessionID, err)
		return false, fmt.Errorf("session-controller: reading the standing terminal failure card for session %q failed: %w", sessionID, err)
	}
	if !standing {
		return false, nil
	}
	card := &frontendv1.FailureCardView{}
	if err := proto.Unmarshal(rec.Card, card); err != nil {
		logf("session-controller: standing terminal failure card UNPARSEABLE session=%s: %v — the resync is failed rather than served without it", sessionID, err)
		return false, fmt.Errorf("session-controller: parsing the standing terminal failure card for session %q failed: %w", sessionID, err)
	}
	item := &frontendv1.ConversationItem{
		Uuid: rec.UUID,
		TsMs: rec.AtMs,
		Item: &frontendv1.ConversationItem_FailureCard{FailureCard: card},
	}
	if !cons.pushReplayedItem(item) {
		logf("session-controller: standing terminal failure card NOT PUSHED session=%s uuid=%s — its provenance could not be resolved (see the refusal above)", sessionID, rec.UUID)
		return true, fmt.Errorf("session-controller: the standing terminal failure card for session %q could not be published on a durable resync", sessionID)
	}
	logf("session-controller: standing terminal failure card SERVED session=%s uuid=%s at_ms=%d — this session's bring-up is terminally fenced, and this is the only account of it a client connecting now would get",
		sessionID, rec.UUID, rec.AtMs)
	return true, nil
}

// stampDurableReplayUnwired records, on the state the frontend renders
// connectivity from, that NOTHING IS DRIVING the conversation just served.
//
// THE DISHONESTY IT ENDS. A durable replay hands a client a full conversation
// with no controller behind it. Nothing in the replay itself said so, so the
// webapp rendered it exactly as a live session — banner cleared, feed
// populated, nothing driving it and no signal that anything was wrong.
//
// IT INVENTS NOTHING. The wired axis already has both closed halves and this
// path knows which one applies, because it is reached only when no controller
// is live:
//
//   - SEVERED when a terminal failure card stands: the session's bring-up is
//     fenced on a verdict that cannot heal on its own, which is precisely
//     "not wired, and there is EVIDENCE of breakage".
//   - HIBERNATED otherwise: an unwired workspace after a daemon bounce is the
//     resting state of every workspace, and painting it as broken is what
//     spent blue's meaning in the first place.
//
// A no-op when the axis already reads that way, so an ordinary reload of an
// ordinary hibernated workspace writes nothing.
func (m *Manager) stampDurableReplayUnwired(workspace, sessionID string, fenced bool) error {
	logf := dlog.Tag(dlog.Logf(m.logf), "ws", workspace, "session", sessionID, "source", "unwired-stamp")
	applier, ok := m.cfg.SSM.(WiringApplier)
	if !ok {
		logf("session-controller: durable resync CANNOT stamp the unwired truth — the state applier does not apply the wired axis, so this replay is served without saying that nothing drives it")
		return nil
	}
	wiring, reason := ssm.WiringHibernated, "durable_replay_unwired"
	if fenced {
		wiring, reason = ssm.WiringSevered, "durable_replay_terminally_fenced"
	}
	if err := applier.ApplyWired(workspace, wiring, reason); err != nil {
		logf("session-controller: durable resync FAILED to stamp the unwired truth wiring=%s reason=%s: %v", wiring, reason, err)
		return fmt.Errorf("session-controller: stamping ws %q as %s on a durable resync failed: %w", workspace, wiring, err)
	}
	logf("session-controller: durable resync STAMPED the unwired truth wiring=%s reason=%s — the replay carries a conversation nothing is driving, and now says so", wiring, reason)
	return nil
}

// hydratePersistedAccounting loads the completed terminal records that replay
// translation attaches to historical assistant and result items.
func (m *Manager) hydratePersistedAccounting(cons *consumer, sessionID string) error {
	accountings, err := m.cfg.TurnAccountings.List(sessionID)
	if err != nil {
		m.logf("session-controller: persisted accounting hydrate FAILED session=%s: %v", sessionID, err)
		return fmt.Errorf("session-controller: list durable turn accounting for %q: %w", sessionID, err)
	}
	for _, accounting := range accountings {
		if accounting == nil || accounting.GetTurnId() == "" {
			m.logf("session-controller: persisted accounting hydrate REFUSED session=%s: accounting record has no turn id", sessionID)
			return fmt.Errorf("session-controller: durable turn accounting for %q contains a record without turn id", sessionID)
		}
		cons.replayedAccounting[accounting.GetTurnId()] = accounting
		for _, response := range accounting.GetResponses() {
			messageID := response.GetApiMessageId()
			if messageID == "" {
				m.logf("session-controller: persisted accounting hydrate REFUSED session=%s turn_id=%s: response has no API message id", sessionID, accounting.GetTurnId())
				return fmt.Errorf("session-controller: durable turn accounting for %q turn %q contains a response without API message id", sessionID, accounting.GetTurnId())
			}
			if prior := cons.replayedResponses[messageID]; prior != nil && !proto.Equal(prior, response) {
				m.logf("session-controller: persisted accounting hydrate REFUSED session=%s turn_id=%s api_message_id=%s: conflicting response identity", sessionID, accounting.GetTurnId(), messageID)
				return fmt.Errorf("session-controller: durable turn accounting for %q contains conflicting responses for API message %q", sessionID, messageID)
			}
			cons.replayedResponses[messageID] = response
		}
	}
	m.logf("session-controller: persisted accounting hydrated session=%s records=%d", sessionID, len(accountings))
	return nil
}

// serveDurableReceipts pushes every un-retired prompt receipt the store's own
// events did not already account for.
//
// WHY THE DURABLE REPLAY NEEDS THIS AT ALL. The store serves the conversation,
// and a prompt is in the conversation only once the vendor's transcript has
// carried it. A prompt ACCEPTED and not yet durable when the daemon died is in
// neither place: the store never received the turn, and the in-memory receipt
// died with the process. Without this the user's own prompt simply disappeared
// on reconnect, which is indistinguishable from never having sent it.
//
// WHY THE LIVE RESYNC PATH NEEDS NOTHING. A workspace WITH a live session
// controller replays its retained receipts already (consumer.resync pushes
// snapshotEchoes beside the permission and failure cards), and those receipts
// are the very ones this daemon is still holding in memory. The durable record
// and the retained one describe the same submits there, so serving both would
// draw each bubble twice.
//
// SUPPRESSION IS BY REQUEST ID FIRST AND BY TEXT SECOND. A durable line that
// NAMES its request settles the question outright. A transcript UserLine
// carries no request id of its own, though, so after a bounce the store's copy
// of the prompt and the receipt for it share nothing but their text — and
// drawing both would put the user's prompt on screen twice, every time, for
// every turn that outlived the daemon that started it. Matching on the text of
// a prompt served at or after the accept instant closes that, and the failure
// mode of matching too eagerly is benign in a way the alternative is not: a
// wrongly suppressed receipt loses nothing, because what suppressed it is the
// conversation's own copy of the same prompt.
func (m *Manager) serveDurableReceipts(workspace, sessionID string, cons *consumer, served *servedPrompts) error {
	logf := dlog.Tag(dlog.Logf(m.logf), "ws", workspace, "session", sessionID, "source", "prompt-receipts")
	if m.cfg.PromptReceipts == nil {
		logf("session-controller: durable prompt receipts NOT served — no PromptReceiptStore is wired, so a prompt accepted but never made durable cannot be recovered")
		return nil
	}
	receipts, err := m.cfg.PromptReceipts.Outstanding(workspace)
	if err != nil {
		logf("session-controller: durable prompt receipts UNREADABLE ws=%q: %v — the resync is failed rather than served without the prompts it may be missing", workspace, err)
		return fmt.Errorf("session-controller: reading the durable prompt receipts for ws %q failed: %w", workspace, err)
	}
	now := m.now()
	for _, r := range receipts {
		age := now - r.AcceptedAtMs
		if reason, ok := served.claim(r); ok {
			logf("session-controller: durable prompt receipt SUPPRESSED ws=%q request_id=%s age_ms=%d match=%s — the store's own events already carried this prompt, and the record is retired",
				workspace, r.RequestID, age, reason)
			cons.retireDurableReceipt(r.RequestID, "store_events_carried_the_prompt:"+reason)
			continue
		}
		if age > promptReceiptStaleAfterMs {
			logf("session-controller: durable prompt receipt STALE ws=%q request_id=%s age_ms=%d stale_after_ms=%d — served anyway, because it is the only evidence this prompt was ever sent",
				workspace, r.RequestID, age, promptReceiptStaleAfterMs)
		}
		item := promptReceiptItem(r.RequestID, r.Text, r.AcceptedAtMs)
		if !cons.pushReplayedItem(item) {
			logf("session-controller: durable prompt receipt NOT PUSHED ws=%q request_id=%s age_ms=%d — its provenance could not be resolved (see the refusal above)",
				workspace, r.RequestID, age)
			continue
		}
		logf("session-controller: durable prompt receipt SERVED ws=%q request_id=%s age_ms=%d accepted_at_ms=%d len=%d — the prompt never became durable, so this is the only record of it",
			workspace, r.RequestID, age, r.AcceptedAtMs, len(r.Text))
	}
	return nil
}

// servedPrompts is what a durable replay's OWN store events served, as far as
// user prompts go: the request ids they named, and the prompt texts they drew.
type servedPrompts struct {
	ids map[string]struct{}
	// texts are consumed one-for-one by claim, so two receipts carrying the
	// same text are suppressed only by two served copies of it. A user who
	// submits "continue" twice and has one of the two become durable keeps the
	// receipt for the one that did not.
	texts []servedPrompt
}

type servedPrompt struct {
	text    string
	tsMs    int64
	claimed bool
}

func newServedPrompts() *servedPrompts {
	return &servedPrompts{ids: map[string]struct{}{}}
}

// observe records the user prompts one pushed delta carried.
func (s *servedPrompts) observe(cd *frontendv1.ConversationDelta) {
	for _, it := range cd.GetItems() {
		um := it.GetUserMessage()
		if um == nil {
			continue
		}
		text := userMessageText(um)
		if text == "" {
			// A pure tool-result feedback message rides the user_message arm
			// too, and it is not a prompt anyone submitted.
			continue
		}
		if id := it.GetRequestId(); id != "" {
			s.ids[id] = struct{}{}
		}
		s.texts = append(s.texts, servedPrompt{text: text, tsMs: it.GetTsMs()})
	}
}

// claim reports whether the served events already account for this receipt's
// prompt, naming which rule decided it.
func (s *servedPrompts) claim(r statedb.PromptReceipt) (string, bool) {
	if _, ok := s.ids[r.RequestID]; ok {
		return "request_id", true
	}
	for i := range s.texts {
		// AT OR AFTER THE ACCEPT INSTANT: an identical prompt from earlier in
		// the conversation is a different submit, and letting it suppress this
		// receipt would discard the evidence of the one still outstanding.
		if s.texts[i].claimed || s.texts[i].text != r.Text || s.texts[i].tsMs < r.AcceptedAtMs {
			continue
		}
		s.texts[i].claimed = true
		return "text", true
	}
	return "", false
}

// durableConsumer builds the throwaway translation consumer one durable replay
// runs through.
//
// It is deliberately NOT a session controller and holds no shim, no ring, and
// no lifecycle hooks: replayed history must reach conversation translation and
// nothing else, exactly as repullConversation guarantees for the shim-served
// re-pull. The curation state (the skill correlator in particular) starts empty
// per replay, which is what a replay read in store order from the floor wants.
func (m *Manager) durableConsumer(workspace, sessionID string) *consumer {
	cons := m.historyConsumer(workspace, sessionID, m.cfg.Push)
	// The one durable store it DOES hold, and it holds it to RETIRE rows: a
	// replay that finds a receipt's prompt already in the store has established
	// the fact the live retirement point would have established, for a daemon
	// that was not alive to establish it.
	//
	// It is bound HERE rather than in historyConsumer because retiring a row is
	// a WRITE, and the other history reader built on that helper — the backwards
	// page reader — is a read that must leave the ledger exactly as it found it
	// (conversationpage.go). A shared builder that bound it would make every
	// future history reader a receipt-retiring one by default.
	cons.receipts = m.cfg.PromptReceipts
	return cons
}

// historyConsumer builds the throwaway translation consumer EVERY history read
// runs through: the durable resync above, and the backwards page reader in
// conversationpage.go.
//
// It exists because those two want the same consumer for the same reason. Both
// translate persisted events that every other plane has already consumed, so
// both need conversation translation with the lifecycle hooks, the progress
// resolver and the ring left unwired — and both need the two bindings below,
// whose absence is silent rather than loud. Two hand-built consumers would
// agree today and drift the first time a third binding is added to one of them.
//
// push is the caller's: a durable resync pushes to clients, while a page
// assembly captures into a buffer and pushes nothing.
func (m *Manager) historyConsumer(workspace, sessionID string, push Pusher) *consumer {
	cons := newConsumer(workspace, sessionID, push, m.cfg.SSM, nil, m.cfg.ClearCompactStore, m.cfg.TurnAccountings, m.logf, nil, nil, nil, nil, nil)
	// A replay's degradations are as user-visible as a live one's — the same
	// conversation reaches the same frontend — so it takes the same WARN
	// channel.
	cons.warnf = m.warnf
	// THE KEEP-ALIVE EXCLUSION'S EVIDENCE, bound here for the same reason the
	// live consumer binds it (sessioncontroller.go): the exclusion is a property
	// of the CONVERSATION CHOKEPOINT, not of the consumer that happens to be
	// driving it. A replay consumer without the ledger runs the whole curation
	// block with withholdKeepAlive silently a no-op, so every ping the live push
	// withheld comes back rendered as though the user had typed it — precisely
	// the divergence between a live push and a replay three days later that a
	// durable ledger exists to prevent.
	cons.keepAliveWindows = m.cfg.KeepAliveWindows
	return cons
}
