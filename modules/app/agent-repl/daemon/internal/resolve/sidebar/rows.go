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

	armName := statusArm(s, rec, session, rowLog)
	current := rc.selected != nil && *rc.selected == rec.ID
	closed := recedes(rec, session)

	rowLog.Debug("daemon.sidebar.row", "the roster resolved a row", dlog.Context{
		"status":  armName,
		"current": current,
		"closed":  closed,
	})
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
	for _, child := range rc.tree.children[rec.ID] {
		out.Children = append(out.Children, r.row(child, rc, rowLog))
	}
	return out
}

// attention reports whether the row draws the attention marker.
//
// The marker is RAISED by a host notification (WSM's own flag, which the
// notification path sets) and CLEARED by selecting the workspace. Selection is
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

// when resolves the when-column's ONE value. MERGED WINS over last selected
// when both exist: that the workspace is done is the more interesting fact,
// and the client applies no precedence of its own. Neither leaves the oneof
// unset, which draws an empty column rather than "0ms ago".
func when(rec wsm.Workspace) *frontendv1.RosterRowWhen {
	out := &frontendv1.RosterRowWhen{}
	switch {
	case rec.MergedAt != nil:
		out.Shown = &frontendv1.RosterRowWhen_Merged{
			Merged: &frontendv1.RosterRowWhenMerged{AtMs: rec.MergedAt.UnixMilli()}}
	case rec.LastSelectedAt != nil:
		out.Shown = &frontendv1.RosterRowWhen_LastSelected{
			LastSelected: &frontendv1.RosterRowWhenLastSelected{AtMs: rec.LastSelectedAt.UnixMilli()}}
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
