package feed

import (
	"sort"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
	"claude-repld/internal/sessionwatcher"
)

// RETIRED ENTRIES. The store retires a page line when the sidecar's conversion
// changed and the vendor record behind it no longer converts to it; the shim
// relays it on WatchAgent's `retired` arm carrying the entry as last served.
// The feed removes whatever it drew for that entry through retire, so an open
// tail drops the row live and a tail that connects later replays the removal.
//
// THE ROWS ARE FOUND BY IDENTITY, NOT BY PLACEMENT. Where a prompt was placed
// depended on the output address and the sub-feed map as they stood when it
// was drawn, and either may have moved since. A row's id is minted from its
// feed and the entry's own key, so every feed is asked for the ids that entry
// could have drawn there, and exactly the rows that exist are retired.

// OnPromptRetired removes every row OnPrompt drew for a retired prompt: the
// user-prompt row (`prompt:<turn>`), or an agent prompt's outgoing and
// delivered ends. A directive's prompt drew no row, so its retirement finds
// none.
//
// THE TURN BOOKKEEPING STAYS. The prompt made its turn known, and possibly the
// turn in flight and stamp; later entries stamped with that turn, or a terminal
// ending it, are judged against those facts, and withdrawing them would report
// a healthy session's later rows as naming a turn nobody opened.
func (r *resolver) OnPromptRetired(ws ids.WorkspaceID, prompt *conversationv1.AgentPrompt, addr sessionwatcher.OutputAddress) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	turn := prompt.GetId().GetValue()
	r.retireEntryRows(s, "user_prompt", turn,
		feedid.RowKey{Kind: feedid.KindPrompt, ID: turn},
		feedid.RowKey{Kind: feedid.KindPrompt, ID: turn, Sub: "out"},
		feedid.RowKey{Kind: feedid.KindPrompt, ID: turn, Sub: "in"},
	)
}

// OnPeerMessageRetired removes the row OnPeerMessage drew for a retired peer
// message: the peer bubble or the hand-back badge, which share one id.
func (r *resolver) OnPeerMessageRetired(ws ids.WorkspaceID, peer *conversationv1.PeerMessage, addr sessionwatcher.OutputAddress) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	r.retireEntryRows(s, "peer_message", peer.GetId(),
		feedid.RowKey{Kind: feedid.KindPeer, ID: peer.GetId()},
	)
}

// retireEntryRows retires, on every feed, each row one of KEYS names there, and
// records a retirement that found nothing drawn at DEBUG: an entry the feed
// never drew (a directive's suppressed prompt, one placed nowhere, one on a
// feed this resolver never opened) is an ordinary outcome, not a fault.
func (r *resolver) retireEntryRows(s *wsState, kind, key string, keys ...feedid.RowKey) {
	feeds := make([]string, 0, len(s.feeds))
	for feed := range s.feeds {
		feeds = append(feeds, feed)
	}
	// Sorted, so the removals publish in the same order on every run.
	sort.Strings(feeds)
	removed := 0
	for _, feed := range feeds {
		addr := s.feedAddrs[feed]
		for _, rowKey := range keys {
			id := r.rowID(s.id, addr, rowKey).GetValue()
			if _, drawn := s.feeds[feed].rows[id]; !drawn {
				continue
			}
			if r.retire(s, addr, id) {
				removed++
			}
		}
	}
	if removed == 0 {
		r.logger(s.id).Debug("daemon.feed.retired_entry_undrawn",
			"a retired entry had drawn no row on any feed; nothing was removed",
			dlog.Context{"kind": kind, "key": key})
	}
}

// OnApiErrorRetired withdraws the evidence line OnApiError recorded for a
// retired api_error page line. An api error draws no row, so the one thing to
// undo is that line on its turn's pending evidence, which a terminal not yet
// drawn would otherwise put in its headline.
//
// ONLY A STAMPED ENTRY IS WITHDRAWN. The line was charged to the entry's stamp
// or, unstamped, to whatever turn was in flight when it arrived; that turn is
// not recoverable now, and withdrawing from the turn in flight TODAY could
// remove a different turn's genuine failure that shares the message.
//
// A TERMINAL ALREADY DRAWN IS LEFT AS IT IS. The turn's evidence is dropped
// when the turn ends, so a retirement arriving after the terminal finds no
// line, and the drawn headline is not re-composed.
func (r *resolver) OnApiErrorRetired(ws ids.WorkspaceID, agent *conversationv1.AgentId, failed *conversationv1.ApiRequestFailed, turn *conversationv1.TurnId, addr sessionwatcher.OutputAddress) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	log := r.logger(ws)
	stamp := turn.GetValue()
	if stamp == "" {
		log.Debug("daemon.feed.retired_api_error_unstamped",
			"a retired api error carried no turn stamp, so no evidence line is attributable to it; nothing was withdrawn",
			dlog.Context{"agent": agent.GetValue()})
		return
	}
	lines := s.turnEvidence[stamp]
	for i, line := range lines {
		if !line.apiFailure || line.apiMessage != failed.GetMessage() {
			continue
		}
		s.turnEvidence[stamp] = append(lines[:i:i], lines[i+1:]...)
		log.Info("daemon.feed.api_error_evidence_withdrawn",
			"a retired api error's evidence line was withdrawn from its turn",
			dlog.Context{"agent": agent.GetValue(), "turn": stamp, "message": failed.GetMessage()})
		return
	}
	log.Debug("daemon.feed.retired_entry_undrawn",
		"a retired entry had drawn no row on any feed; nothing was removed",
		dlog.Context{"kind": "api_error", "key": stamp})
}
