package sidebar

import (
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// rowContext is everything a row needs that is not the workspace record
// itself, assembled once per render rather than looked up per row.
type rowContext struct {
	// sessions are the durable session records, by workspace.
	sessions map[ids.WorkspaceID]*wsm.Session
	// selected is the current selection, nil when none is.
	selected *ids.WorkspaceID
	// tree is the section's nesting, empty for a flat section.
	tree forest
}

// row composes one workspace's row, and its family below it.
func (r *resolver) row(rec wsm.Workspace, rc rowContext, log dlog.Logger) *frontendv1.RosterRow {
	s := r.state.workspace(rec.ID)
	session := rc.sessions[rec.ID]
	rowLog := log.With(dlog.Context{"workspace_id": string(rec.ID)})

	if s.restoreResult(rec) {
		rowLog.Info("daemon.sidebar.result_restored",
			"the roster drew the last turn result from the durable record, as it stood before this daemon", dlog.Context{
				"end": s.restoredEnd, "result": s.result.String(),
			})
	}
	armName := statusArm(s, rec, session, rowLog)
	r.noteResult(rec.ID, s, rowLog)
	// THE MARKER IS DERIVED FROM THE READ FACT, in the same breath as the
	// status, so the row's display mode cannot lag its status by a push and a
	// read result is never drawn as unread (`wsState.viewedOn`).
	wasViewed := s.lastArmSeen && s.viewedOn(s.lastArm)
	armChanged := s.noteArm(armName)
	viewed := s.viewedOn(armName)
	current := rc.selected != nil && *rc.selected == rec.ID
	closed := recedes(rec, session)

	rowLog.Debug("daemon.sidebar.row", "the roster resolved a row", dlog.Context{
		"status":   armName,
		"current":  current,
		"closed":   closed,
		"viewed":   viewed,
		"result":   s.result.String(),
		"reviving": s.reviving,
	})
	switch {
	case wasViewed && !viewed:
		rowLog.Debug("daemon.sidebar.row_viewed_cleared",
			"the row left its read turn-end state, so it is drawn FULL", dlog.Context{
				"status": armName,
				"result": s.result.String(),
			})
	case armChanged && !wasViewed && viewed:
		rowLog.Debug("daemon.sidebar.row_viewed_restored",
			"the row returned to its turn-end arm with the result already read, so it is drawn PARTIAL", dlog.Context{
				"status": armName,
			})
	}
	r.assertArm(armName, rowLog)

	out := &frontendv1.RosterRow{
		Workspace: &frontendv1.RosterRowWorkspace{
			Workspace: &workspacev1.WorkspaceRef{Id: string(rec.ID), Dir: rec.Dir}},
		Name:    &frontendv1.RosterRowName{Text: rec.Name},
		Current: &frontendv1.RosterRowCurrent{Current: current},
		When:    when(rec),
		Detail:  detail(rec, s.summary),
		Closed:  &frontendv1.RosterRowClosed{Closed: closed},
	}
	setStatus(out, armName, rowLog)
	if badge := priorityBadge(rec.Priority); badge != nil {
		out.Priority = badge
	}
	if attention(rec, current) {
		out.Attention = &frontendv1.RosterRowAttention{}
	}
	// PRESENCE IS THE MODE: the marker is set for PARTIAL and omitted for
	// FULL, exactly as `frontend.v1.RosterRowViewed` states it.
	if viewed {
		out.Viewed = &frontendv1.RosterRowViewed{}
	}
	// PRESENCE IS THE FACT, as `frontend.v1.RosterRowReviving` states it: set
	// while the revival is in flight, omitted otherwise.
	if s.reviving {
		out.Reviving = &frontendv1.RosterRowReviving{}
	}
	for _, child := range rc.tree.children[rec.ID] {
		out.Children = append(out.Children, r.row(child, rc, rowLog))
	}
	return out
}

// attention reports whether the row draws the attention marker.
//
// The marker is RAISED by a host notification (WSM's own flag, which the
// notification path sets) and CLEARED by selecting the workspace, or by the
// last ask that raised it settling (the workspace verbs' AsksSettled, which
// clears the same flag). Selection is
// what clears it, so the workspace being looked at never wears one: the daemon
// stamps the selection the moment SelectWorkspace arrives, and the marker goes
// with it rather than waiting for WSM to echo the clear back.
func attention(rec wsm.Workspace, current bool) bool {
	return rec.Attention && !current
}

// recedes reports whether the row draws GREYED. Three settled ends recede a
// row and they are deliberately one field rather than three: the workspace was
// MERGED, its editor state was CLOSED, or its session was KILLED. Receding is
// orthogonal to the lifecycle — a receded workspace still has one — which is
// why it is not a status arm.
func recedes(rec wsm.Workspace, session *wsm.Session) bool {
	if rec.Closed || rec.MergedAt != nil {
		return true
	}
	return session != nil && session.Terminal != nil && session.Terminal.Kind == "killed"
}

// when resolves the when-column's ONE value.
//
// THE COLUMN IS LAST-ACTIVITY, NOT LAST-VIEWED — and that distinction is the
// whole point of this function. It once showed LastSelectedAt, which the daemon
// stamps on EVERY SelectWorkspace, so the column was really a viewing-recency
// timer that reset whenever the user switched to the workspace: the age beside
// a row jumped on mere navigation and read as broken. The column must reflect
// when the workspace last DID REAL WORK, which is stable across selection, so
// this reads LastActivityAt (stamped at the turn edge in wsm) and NEVER
// LastSelectedAt. LastSelectedAt lives on for ordering and attention-clear; it
// just must not drive this column again — reintroducing it here is the exact
// regression the sidebar tests lock.
//
// Precedence: MERGED WINS (that the workspace is done is the more interesting
// fact), else the last activity, else the creation time. The last arm never
// leaves the oneof unset for a registered workspace — every one has a creation
// time — so the column falls back to "created" rather than drawing empty.
func when(rec wsm.Workspace) *frontendv1.RosterRowWhen {
	out := &frontendv1.RosterRowWhen{}
	switch {
	case rec.MergedAt != nil:
		out.Shown = &frontendv1.RosterRowWhen_Merged{
			Merged: &frontendv1.RosterRowWhenMerged{AtMs: rec.MergedAt.UnixMilli()}}
	case rec.LastActivityAt != nil:
		out.Shown = &frontendv1.RosterRowWhen_Active{
			Active: &frontendv1.RosterRowWhenActive{AtMs: rec.LastActivityAt.UnixMilli()}}
	case !rec.CreatedAt.IsZero():
		out.Shown = &frontendv1.RosterRowWhen_Created{
			Created: &frontendv1.RosterRowWhenCreated{AtMs: rec.CreatedAt.UnixMilli()}}
	}
	return out
}

// detail composes the expanded panel. Each of the three lines is OMITTED when
// there is nothing to say rather than drawn blank, which is why each is its own
// message rather than a string on the panel.
func detail(rec wsm.Workspace, summary string) *frontendv1.RosterRowDetail {
	out := &frontendv1.RosterRowDetail{}
	if rec.Branch != "" {
		out.Branch = &frontendv1.RosterRowDetailBranch{Name: rec.Branch}
	}
	if rec.ParentBranch != "" {
		out.ParentBranch = &frontendv1.RosterRowDetailParentBranch{Name: rec.ParentBranch}
	}
	if summary != "" {
		out.Summary = &frontendv1.RosterRowDetailSummary{Text: summary}
	}
	return out
}

// firstLine is the summary's whole rule: the last prompt's FIRST LINE. A
// prompt is often a paragraph and the row has one line to give it.
func firstLine(text string) string {
	for i, c := range text {
		if c == '\n' {
			return text[:i]
		}
	}
	return text
}

// noteResult queues a report of the workspace's last turn result when it
// differs from what the durable record holds, for mutate to hand the result
// sink once the lock is released.
func (r *resolver) noteResult(ws ids.WorkspaceID, s *wsState, log dlog.Logger) {
	snapshot := s.resultSnapshot()
	if sameResult(snapshot, s.persisted) {
		return
	}
	s.persisted = snapshot
	if r.results == nil {
		return
	}
	log.Debug("daemon.sidebar.result_changed", "the last turn result changed; the durable record is told", dlog.Context{
		"result": s.result.String(), "end": s.turnEndArm(),
	})
	r.pendingResults = append(r.pendingResults, resultChange{ws: ws, result: snapshot})
}
