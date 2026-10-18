package feed

import (
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// A MIRRORED TURN IS DRAWN TWICE, by ONE author. An async merge's workspace
// draws its merge turns in its own feed as ordinary turns (that workspace has
// no merge bubble), and the requester's bubble streams the same turns in its
// agentic tab (owner ruling, 2026-10-11). The prompt queue records the mirror
// on such a turn (wsm.Turn.Mirror) exactly as it records an address, and hands
// it here as it records the turn (MirrorTurn); a replay reads it back
// (Deps.TurnMirrors) into the same table.
//
// Every publication of a row of a mirrored turn in its own workspace is
// followed by the same row, RE-IDENTIFIED into the mirror's workspace and feed,
// in that workspace's feed: same row key, the mirror's feed, its top-level
// rows nested under the mirror's parent row and its nested rows under their
// parents re-identified alike. The copy carries no turn of its own (the turn
// is the other workspace's) and is DURABLE in the mirror's workspace, so a
// restart draws it without replaying the merge workspace. It is a projection
// re-derived on every publication of its source, never a second author.

// SetOutputMirror installs the mirror a lease holder wants the turns it starts
// drawn at too; nil withdraws it. Like SetOutputAddress it moves no row: only
// a turn the prompt queue records while it stands takes it.
func (r *resolver) SetOutputMirror(ws ids.WorkspaceID, m *wsm.MirrorAddress) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	s.mirror = copyMirror(m)
	target := "none"
	if m != nil {
		target = string(m.Workspace) + "|" + r.feedKey(m.Workspace, m.Feed)
	}
	r.logger(ws).Debug("daemon.feed.output_mirror", "the feed's output mirror changed", dlog.Context{"mirror": target, "cleared": m == nil})
}

// OutputMirror answers a copy of the mirror standing for one workspace, nil
// when none stands.
func (r *resolver) OutputMirror(ws ids.WorkspaceID) *wsm.MirrorAddress {
	r.mu.Lock()
	defer r.mu.Unlock()
	s, ok := r.workspaces[ws]
	if !ok {
		return nil
	}
	return copyMirror(s.mirror)
}

// MirrorTurn records the mirror one turn is also drawn at, nil for none.
func (r *resolver) MirrorTurn(ws ids.WorkspaceID, turn ids.TurnID, m *wsm.MirrorAddress) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.state(ws).mirrorTurn(turn, m)
	if m != nil {
		r.logger(ws).Debug("daemon.feed.turn_mirror", "the feed took the mirror a turn is also drawn at", dlog.Context{
			"turn": string(turn), "mirror_workspace": string(m.Workspace), "feed": r.feedKey(m.Workspace, m.Feed)})
	}
}

// mirrorTurn stores a copy of TURN's mirror, or forgets it when it has none.
func (s *wsState) mirrorTurn(turn ids.TurnID, m *wsm.MirrorAddress) {
	if turn == "" {
		return
	}
	if m == nil {
		delete(s.turnMirrors, turn)
		return
	}
	s.turnMirrors[turn] = copyMirror(m)
}

// copyMirror copies a mirror address, nil for nil.
func copyMirror(m *wsm.MirrorAddress) *wsm.MirrorAddress {
	if m == nil {
		return nil
	}
	copied := *m
	return &copied
}

// recordedMirrors reads the recorded mirror of every turn a history page names,
// BEFORE the resolver's lock is taken. A FAILED READ IS RECORDED, NEVER
// SWALLOWED, and the page still draws: the turns' own rows are drawn, and only
// their mirrored copies wait for the next publication.
func (r *resolver) recordedMirrors(ws ids.WorkspaceID, turns []ids.TurnID) map[ids.TurnID]*wsm.MirrorAddress {
	return readRecorded(r, ws, turns, r.deps.TurnMirrors, "daemon.feed.turn_mirrors_unreadable",
		"the replayed turns' recorded mirrors could not be read; their rows are drawn in this workspace only")
}

// mirrorRow draws ROW, just published in S's feed, again at its turn's mirror,
// when its turn has one. Called with r.mu held.
func (r *resolver) mirrorRow(s *wsState, row *frontendv1.FeedRow) {
	turn := ids.TurnID(row.GetTurn().GetValue())
	if turn == "" {
		return
	}
	m, mirrored := s.turnMirrors[turn]
	if !mirrored {
		return
	}
	if row.GetEarlierSteps() != nil {
		// A FOLD OVER THIS BOOK'S UNLOADED PAGES is about this feed's own
		// walk: the mirror pages nothing of this book, so it never stands there.
		r.logger(s.id).Debug("daemon.feed.mirror_skipped_earlier_steps",
			"a mirrored turn's earlier-steps row was not mirrored; it is about this feed's own walk",
			dlog.Context{"turn": string(turn), "row": row.GetId().GetValue()})
		return
	}
	fields := dlog.Context{"turn": string(turn), "row": row.GetId().GetValue(), "mirror_workspace": string(m.Workspace)}
	ref, err := r.deps.Decode(row.GetId())
	if err != nil {
		r.logger(s.id).Error("daemon.feed.mirror_undecodable", "a mirrored turn's row carries an identity that will not decode; it is not mirrored",
			dlog.WithCause(fields, err))
		return
	}
	parent := r.deps.Encode(m.Parent)
	if row.GetParent() != nil {
		nested, err := r.deps.Decode(row.GetParent().GetRow())
		if err != nil {
			r.logger(s.id).Error("daemon.feed.mirror_undecodable", "a mirrored turn's row names a parent that will not decode; it is not mirrored",
				dlog.WithCause(fields, err))
			return
		}
		parent = r.deps.Encode(feedid.Ref{WS: m.Workspace, Feed: m.Feed, Row: nested.Row})
	}
	copied, ok := r.snapshotRow(s, row, unclonable{
		operation: "daemon.feed.mirror_unclonable", message: "a mirrored turn's row could not be cloned; it is not mirrored", context: fields,
	})
	if !ok {
		return
	}
	copied.Id = r.deps.Encode(feedid.Ref{WS: m.Workspace, Feed: m.Feed, Row: ref.Row})
	copied.Parent = &frontendv1.FeedRowParent{Row: parent}
	copied.Turn = nil
	copied.Order = nil
	r.upsertDurable(r.state(m.Workspace), placement{feed: m.Feed}, copied)
}
