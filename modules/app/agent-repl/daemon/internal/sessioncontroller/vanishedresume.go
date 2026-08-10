// vanishedresume.go is the FENCE for a bring-up whose outcome is already known.
//
// The bring-up ladder (bringupescape.go) bounds the failures it can SEE: a
// controller that never wired is counted, carded and eventually parked. A
// spawn refused BEFORE a controller exists — the spawner's resume-viability
// gate is the one that matters here — reaches none of that machinery. It is
// returned bare from bringUpTracked, so nothing counts it, nothing cards it,
// and every ensure/open/sweep re-runs it. For a resume target whose transcript
// has been deleted with no sibling to fall back to (server/resumevanished.go),
// that is an unbounded loop over a fact that cannot change on its own.
//
// THE FENCE IS ONE FACT PER SESSION, PER BOOT: the first terminal refusal
// publishes ONE failure card and records the session; every later bring-up is
// short-circuited to that same standing card without spawning anything, and
// says so at debug rather than at error — the error was already reported, once,
// by the attempt that established the fence.
//
// IT IS DELIBERATELY NOT A COOLDOWN. The park in bringupescape.go expires
// because the failures it bounds are the kind that heal on their own (load, a
// vendor blip). A vanished transcript does not come back by waiting, so a
// cooldown would only re-derive the same verdict forever at a slower rate. The
// two ways out are both EXPLICIT USER ACTIONS: deleting the session, or a hard
// restart — which clears the fence, re-checks the disk exactly once through the
// ordinary gate, and re-fences if the transcript is still gone.
//
// THE FENCE IS PER BOOT, held in memory. A successor daemon re-checks the disk
// on its first bring-up of the session and re-fences from what it finds, which
// costs one refused spawn per boot and keeps the fence from outliving a
// transcript that was restored while the daemon was down.
package sessioncontroller

import (
	"errors"
	"fmt"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/errclass"
	"claude-repld/internal/frontend"
	"claude-repld/internal/ssm"
	"claude-repld/internal/statedb"

	"google.golang.org/protobuf/proto"
)

// vanishedResumeFence is one session's standing terminal refusal.
type vanishedResumeFence struct {
	// workspace is the workspace the card was published onto.
	workspace string
	// cause is the terminal error, returned verbatim to every short-circuited
	// caller so no caller can mistake a fence for a different failure.
	cause error
	// shortCircuits counts the bring-ups the fence has refused, so the debug
	// record shows the loop it is standing in for.
	shortCircuits int
}

// startFailedCardUUID is the stable card identity for a session's start-failed
// report, shared by the consumer and by the fence.
//
// ONE DEFINITION, because the fence publishes onto the SAME card the bring-up
// ladder does: a session that fences after a carded failure must update that
// card rather than stack a second account of one broken session.
func startFailedCardUUID(sessionID string) string { return "start_failed:" + sessionID }

// vanishedResumeRefusal reports the standing terminal refusal for sessionID, if
// the session is fenced. The bool is the caller's instruction to spawn nothing.
func (m *Manager) vanishedResumeRefusal(workspace, sessionID string) (error, bool) {
	m.mu.Lock()
	f := m.vanishedResume[sessionID]
	if f == nil {
		m.mu.Unlock()
		return nil, false
	}
	f.shortCircuits++
	count := f.shortCircuits
	cause := f.cause
	m.mu.Unlock()
	dlog.Tag(dlog.Logf(m.logf), "level", string(dlog.LevelDebug))(
		"session-controller: bring-up SHORT-CIRCUITED by the vanished-resume fence ws=%q session=%s short_circuits=%d — the standing failure card already names this; nothing is spawned and nothing is retried until the session is deleted or hard-restarted",
		workspace, sessionID, count)
	return cause, true
}

// vanishedResumeFenced reports whether sessionID is fenced, under the lock that
// owns the map.
func (m *Manager) vanishedResumeFenced(sessionID string) bool {
	m.mu.Lock()
	defer m.mu.Unlock()
	_, ok := m.vanishedResume[sessionID]
	return ok
}

// fenceVanishedResume records the terminal refusal and publishes its one card.
// It reports whether THIS call established the fence, so a caller cannot
// mistake a re-observation for a first one.
func (m *Manager) fenceVanishedResume(workspace, sessionID string, cause error) bool {
	m.mu.Lock()
	if m.vanishedResume == nil {
		m.vanishedResume = make(map[string]*vanishedResumeFence)
	}
	if _, already := m.vanishedResume[sessionID]; already {
		m.mu.Unlock()
		return false
	}
	m.vanishedResume[sessionID] = &vanishedResumeFence{workspace: workspace, cause: cause}
	m.mu.Unlock()
	// LOUD, ONCE. This is the single report of a session that cannot come back
	// on its own, and it names both the way in and the ways out.
	m.warnf("session-controller: bring-up TERMINALLY FAILED ws=%q session=%s reason=resume_target_vanished retry=never — the recorded conversation's transcript is gone and the workspace has no other transcript to resume; the session is fenced on a standing failure card and will not be respawned until it is deleted or hard-restarted: %v",
		workspace, sessionID, cause)
	m.publishTerminalStartFailure(workspace, sessionID, cause)
	return true
}

// clearVanishedResumeFence drops a session's fence so the NEXT bring-up
// re-derives the verdict from the disk. It is the way out, and only an explicit
// user action reaches it.
func (m *Manager) clearVanishedResumeFence(workspace, sessionID string) {
	m.mu.Lock()
	f := m.vanishedResume[sessionID]
	delete(m.vanishedResume, sessionID)
	m.mu.Unlock()
	// UNCONDITIONALLY, and before the in-memory check. The fence map is per
	// boot and the card's row is not, so a successor daemon that has not yet
	// re-derived the fence still holds the previous daemon's standing card —
	// and a hard restart must withdraw that claim whether or not this process
	// happens to be the one that made it.
	m.withdrawTerminalStartFailure(workspace, sessionID)
	if f == nil {
		return
	}
	m.logf("session-controller: vanished-resume fence CLEARED ws=%q session=%s short_circuits=%d — the next bring-up re-checks the transcript on disk and re-fences if it is still gone",
		workspace, sessionID, f.shortCircuits)
}

// terminalStartFailureCard renders the fence's card.
//
// The body comes from openFailureCard, so a terminal resume failure carries the
// SAME typed continuity evidence — which conversation, which config root, which
// paths were searched — that every other classified resume failure does.
//
// THE CARD STATES THE FENCE'S OWN DISPOSITION. Every other bring-up failure is
// `open' — it invites waiting, and the ensure/reattach ladders are right to
// keep asking. This one cannot heal on its own, which is exactly what the
// terminal arm means, and stamping it is what lets a client stop its automatic
// re-open loop from the wire instead of from prose. The card is not hidden or
// downgraded by this: it renders in the feed as it always did, and only its
// invitation to wait is withdrawn.
//
// IT IS ONE RENDERING, shared by the live push and the durable record, so the
// card a late reader is served cannot drift from the one that was pushed.
func (m *Manager) terminalStartFailureCard(cause error) *frontendv1.FailureCardView {
	card := openFailureCard(m.logf, cause)
	errclass.Terminal(card)
	return card
}

// persistTerminalStartFailure writes the fence's card to the durable store,
// which is what makes it survive the instant it was pushed at.
//
// THE PUSH IS FOR THE CLIENT CONNECTED NOW; THIS IS FOR EVERY LATER ONE. A
// resync for a workspace with no live controller is served from durable history
// (durablereplay.go), and durable history is the vendor conversation — a
// session that was refused before it ever spawned wrote nothing into it. So
// without this row the terminal refusal is invisible to every client that
// connects after it, forever, which is the failure this exists to end.
//
// THE ROW REPLACES rather than accumulates: the fence is cleared by a hard
// restart and re-established when the re-check finds the transcript still gone,
// and that flow must restate one standing card, not stack a second.
func (m *Manager) persistTerminalStartFailure(workspace, sessionID string, item *frontendv1.ConversationItem, cause error) {
	if m.cfg.TerminalFailureCards == nil {
		m.warnf("session-controller: terminal bring-up failure ws=%q session=%s is NOT persisted — no TerminalFailureCardStore is wired, so only a client connected at this instant will ever see its card: %v",
			workspace, sessionID, cause)
		return
	}
	blob, err := proto.Marshal(item.GetFailureCard())
	if err != nil {
		m.errorf("session-controller: terminal bring-up failure card for ws=%q session=%s could not be marshaled and is NOT persisted; a client connecting later will see no account of it: %v",
			workspace, sessionID, err)
		return
	}
	if err := m.cfg.TerminalFailureCards.Record(statedb.TerminalFailureCard{
		SessionID: sessionID,
		Workspace: workspace,
		UUID:      item.GetUuid(),
		Card:      blob,
		AtMs:      item.GetTsMs(),
	}); err != nil {
		m.errorf("session-controller: terminal bring-up failure card for ws=%q session=%s could not be persisted; a client connecting later will see no account of it: %v",
			workspace, sessionID, err)
		return
	}
	m.logf("session-controller: terminal bring-up failure card PERSISTED ws=%q session=%s uuid=%s — every later durable-history resync serves it, under the same identity the live push used",
		workspace, sessionID, item.GetUuid())
}

// withdrawTerminalStartFailure deletes the session's standing card, because the
// claim it makes has been withdrawn: clearing the fence is an explicit user
// action, and the next bring-up either succeeds — in which case the card would
// be a lie about a live session — or re-fences and rewrites the row.
func (m *Manager) withdrawTerminalStartFailure(workspace, sessionID string) {
	if m.cfg.TerminalFailureCards == nil {
		return
	}
	stood, err := m.cfg.TerminalFailureCards.Withdraw(sessionID)
	if err != nil {
		m.errorf("session-controller: the standing terminal failure card for ws=%q session=%s could not be withdrawn; a durable-history resync will keep serving it after the fence was cleared: %v",
			workspace, sessionID, err)
		return
	}
	if stood {
		m.logf("session-controller: standing terminal failure card WITHDRAWN ws=%q session=%s — the fence was cleared, so its account of a session that cannot come up no longer stands",
			workspace, sessionID)
	}
}

// publishTerminalStartFailure publishes the fence's card WITHOUT a controller,
// which is the whole reason it exists: the refusal happens before any
// controller is constructed, so the consumer's push path is out of reach and a
// failure with nowhere to land is a failure the user never sees.
//
// IT PUBLISHES TWICE, TO TWO AUDIENCES. The durable record is written FIRST and
// is the source of truth for every later reader; the live push then reaches the
// clients connected at this instant, which the store cannot. Neither replaces
// the other, and both carry one rendering under one identity.
func (m *Manager) publishTerminalStartFailure(workspace, sessionID string, cause error) {
	card := m.terminalStartFailureCard(cause)
	item := &frontendv1.ConversationItem{
		Uuid: startFailedCardUUID(sessionID),
		TsMs: m.now(),
		Item: &frontendv1.ConversationItem_FailureCard{FailureCard: card},
	}
	// PERSISTED BEFORE PUSHED, and before the Push nil-check refuses: a daemon
	// with no frontend attached still owes the record, and a card the store
	// holds is a card every later client is served whether or not one was
	// listening now.
	m.persistTerminalStartFailure(workspace, sessionID, item, cause)
	if m.cfg.Push == nil {
		m.errorf("session-controller: terminal bring-up failure ws=%q session=%s has no frontend to publish its failure card onto live; the cause is recorded here and in the durable card: %v",
			workspace, sessionID, cause)
		return
	}
	// Provenance is stamped exactly as the consumer's local-item push stamps
	// it, and a refusal to stamp is a refusal to push: an item with an unset
	// source is rejected downstream, so sending one would trade a missing card
	// for a rejected frame.
	source := frontendv1.ConversationSource_CONVERSATION_SOURCE_USER
	if m.cfg.SSM != nil && m.cfg.SSM.MergeLeaseHeld(workspace) {
		source = frontendv1.ConversationSource_CONVERSATION_SOURCE_MERGE
	}
	if err := frontend.StampItemConversationSource(item, source); err != nil {
		m.errorf("session-controller: terminal bring-up failure card for ws=%q session=%s could not be stamped and is NOT published: %v",
			workspace, sessionID, err)
		return
	}
	m.cfg.Push.PushConversationDelta(&frontendv1.ConversationDelta{
		Workspace: workspace,
		// The generation half of the fence is empty because no controller
		// generation was ever minted: nothing was spawned. The session half is
		// what addresses the card.
		Fence: ssm.Fence(sessionID, ""),
		Items: []*frontendv1.ConversationItem{item},
	})
}

// classifyTerminalBringUp is the single decision point for a spawn refusal that
// arrived before any controller existed: fence it when it is terminal, and
// return the error either way.
//
// EVERY OTHER SPAWN FAILURE IS UNTOUCHED. A refusal that is not classified
// terminal keeps the retry behavior it has always had, because a fence applied
// to a transient failure would turn a recoverable session into a dead one.
func (m *Manager) classifyTerminalBringUp(workspace, sessionID string, err error) error {
	if !errors.Is(err, errclass.ErrResumeTargetVanished) {
		return err
	}
	m.fenceVanishedResume(workspace, sessionID, err)
	return fmt.Errorf("session-controller: session %s (ws %q) cannot be brought up: %w", sessionID, workspace, err)
}
