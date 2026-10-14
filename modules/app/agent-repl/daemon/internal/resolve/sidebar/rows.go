package sidebar

import (
	"fmt"

	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/shimclient"
	"agent-repld/internal/wsm"
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
	// cause is the operation whose fact is being rendered.
	cause string
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
	armName := statusArm(s, rec, session, r.footer.Status(rec.ID), r.now(), rowLog)
	r.noteResult(rec.ID, s, rowLog)
	// THE MARKER IS DERIVED FROM THE READ FACT, in the same breath as the
	// status, so the row's display mode cannot lag its status by a push and a
	// read result is never drawn as unread (`wsState.viewedOn`).
	wasViewed := s.lastArmSeen && s.viewedOn(s.lastArm)
	previousArm, armChanged := s.noteArm(armName)
	if armChanged {
		r.logArmChange(rec.ID, armName, previousArm, rc.cause)
	}
	viewed := s.viewedOn(armName)
	current := rc.selected != nil && *rc.selected == rec.ID
	closed := recedes(rec, session)
	detachedLive := s.asyncLive()

	rowLog.Debug("daemon.sidebar.row", "the roster resolved a row", dlog.Context{
		"status":   armName,
		"current":  current,
		"closed":   closed,
		"viewed":   viewed,
		"result":   s.result.String(),
		"reviving": s.reviving,
		// The editor chooses its dwell threshold from this fact.
		"detached_live": detachedLive,
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
		When:    when(rec, r.footer.MergeStartedAt(rec.ID), r.footer.TurnClock(rec.ID)),
		Detail:  detail(rec, s.summary),
		Closed:  &frontendv1.RosterRowClosed{Closed: closed},
		// ALWAYS SET: an editor opens the workspace only once this leaves
		// `pending`, so a row without it would never be opened.
		Availability: availability(s),
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
	// PRESENCE IS THE FACT, as `frontend.v1.RosterRowDetachedLive` states it:
	// set exactly while detached work counts as live (asyncLive), on any arm.
	// Every arm above the idle rung (thinking, a permission ask) can stand
	// beside it, so the status alone cannot say it.
	if detachedLive {
		out.DetachedLive = &frontendv1.RosterRowDetachedLive{}
	}
	// PRESENCE IS THE FACT, as `frontend.v1.RosterRowOneShot` states it: set
	// for a workspace created as a one-shot, for its whole life.
	out.OneShot = oneShot(rec)
	// PRESENCE IS THE FACT, as `frontend.v1.RosterRowOpening` states it: set
	// while an open of the workspace is in flight, when the status is
	// `inactive` and the row takes no click.
	if s.opening {
		out.Opening = &frontendv1.RosterRowOpening{}
	}
	// PRESENCE IS THE FACT, as `frontend.v1.RosterRowAsyncMerging` states it:
	// set while the workspace's own branch merges async, beside any arm.
	out.AsyncMerging = asyncMergingMarker(r.state.asyncMerging(rec.ID), armName, rowLog)
	// THE DURABLE SELECTION INSTANT, for ordering only: clients order
	// most-recently-selected first by it (the landing after a close, the
	// editor's jump to the most recent workspace), and it survives a restart
	// of every client because it is WSM's. Unset when never selected.
	out.LastSelected = lastSelected(rec)
	// THE DURABLE TURN-END INSTANT, for ordering only: the editor's jump to
	// the workspace that most recently finished ranks by it. Unset when no
	// turn of the workspace has closed.
	out.LastTurnEnded = lastTurnEnded(rec)
	// THE DURABLE CLOSE RECORD, for filtering and ordering only: the editor's
	// switcher offers the workspaces the user most recently closed. Unset
	// while open, and on a close that predates the record.
	out.Closure = closure(rec, rowLog)
	for _, child := range rc.tree.children[rec.ID] {
		out.Children = append(out.Children, r.row(child, rc, rowLog))
	}
	return out
}

// availability resolves whether an editor may open the workspace yet
// (frontend.v1.RosterRowAvailability): AVAILABLE once a link has connected
// under this daemon, which never lapses; otherwise PENDING while a bring-up is
// under way; otherwise UNAVAILABLE when the link is dead (it died before it
// ever connected, the condition `start_failed` draws); otherwise AVAILABLE,
// since a workspace with nothing under way and nothing failed has nothing to
// wait for.
func availability(s *wsState) *frontendv1.RosterRowAvailability {
	switch {
	case s.everConnected:
		return &frontendv1.RosterRowAvailability{Availability: &frontendv1.RosterRowAvailability_Available{
			Available: &frontendv1.RosterRowAvailabilityAvailable{}}}
	case s.bringingUp:
		return &frontendv1.RosterRowAvailability{Availability: &frontendv1.RosterRowAvailability_Pending{
			Pending: &frontendv1.RosterRowAvailabilityPending{}}}
	case s.linkSeen && s.link == shimclient.LinkDead:
		return &frontendv1.RosterRowAvailability{Availability: &frontendv1.RosterRowAvailability_Unavailable{
			Unavailable: &frontendv1.RosterRowAvailabilityUnavailable{}}}
	default:
		return &frontendv1.RosterRowAvailability{Availability: &frontendv1.RosterRowAvailability_Available{
			Available: &frontendv1.RosterRowAvailabilityAvailable{}}}
	}
}

// lastSelected resolves the row's durable selection instant, or nil for a
// workspace the user never selected. It is NOT the when-column (see when):
// the column shows activity, and this field is never drawn.
func lastSelected(rec wsm.Workspace) *frontendv1.RosterRowLastSelected {
	if rec.LastSelectedAt == nil {
		return nil
	}
	return &frontendv1.RosterRowLastSelected{AtMs: rec.LastSelectedAt.UnixMilli()}
}

// lastTurnEnded resolves the row's durable turn-end instant, or nil for a
// workspace none of whose turns has closed. Like lastSelected it is never
// drawn: the when-column shows the running turn, not this.
func lastTurnEnded(rec wsm.Workspace) *frontendv1.RosterRowLastTurnEnded {
	if rec.LastTurnEndedAt == nil {
		return nil
	}
	return &frontendv1.RosterRowLastTurnEnded{AtMs: rec.LastTurnEndedAt.UnixMilli()}
}

// closure resolves the row's durable close record, or nil for an open
// workspace and for a closed one whose close predates the record. A cause
// outside wsm's declared set is impossible (the record refuses it at the write
// and at the read), so reaching one here fails hard.
func closure(rec wsm.Workspace, log dlog.Logger) *frontendv1.RosterRowClosure {
	if rec.Closure == nil {
		return nil
	}
	out := &frontendv1.RosterRowClosure{AtMs: rec.Closure.At.UnixMilli()}
	switch rec.Closure.By {
	case wsm.CloseByUser:
		out.By = &frontendv1.RosterRowClosure_User{User: &frontendv1.RosterRowClosureByUser{}}
	case wsm.CloseByAgent:
		out.By = &frontendv1.RosterRowClosure_Agent{Agent: &frontendv1.RosterRowClosureByAgent{}}
	case wsm.CloseByMerge:
		out.By = &frontendv1.RosterRowClosure_Merge{Merge: &frontendv1.RosterRowClosureByMerge{}}
	case wsm.CloseByDaemon:
		out.By = &frontendv1.RosterRowClosure_Daemon{Daemon: &frontendv1.RosterRowClosureByDaemon{}}
	default:
		log.Error("daemon.sidebar.closure", "the close record names an undeclared cause", dlog.Context{"closed_by": string(rec.Closure.By)})
		panic(fmt.Sprintf("sidebar: workspace %s: undeclared close cause %q", rec.ID, rec.Closure.By))
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
// Precedence: MERGED WINS (that the workspace is done is the more interesting
// fact), else HIDDEN (a hidden row counts from its hide, owner request
// 2026-10-13), else a SYNC MERGE the workspace's footer strip draws (owner
// request, 2026-10-14), counted from when it joined the queue, else the TURN
// CLOCK the strip shows (owner request, 2026-10-13): a running turn counts
// from the clock's own start instant, so the row and the strip read the same
// duration, and a workspace with no turn running draws "0s".
//
// THE MERGE OUTRANKS THE TURN: the turns a merge runs (its prompts, conflict
// resolution, fixing) are the merge's work, so the row counts the whole merge
// rather than restarting at each of them.
//
// Both instants are the footer's PUBLISHED ones (footer.Resolver's
// MergeStartedAt and TurnClock), never state this resolver keeps for itself,
// so the row and the strip cannot disagree; a clock with a start and an end
// is the last turn's, frozen, and no turn is running. An async merge's
// requester is never told its merge, so its row keeps its turn clock.
func when(rec wsm.Workspace, mergeStartedAt *int64, clock *frontendv1.FooterClock) *frontendv1.RosterRowWhen {
	out := &frontendv1.RosterRowWhen{}
	switch {
	case rec.MergedAt != nil:
		out.Shown = &frontendv1.RosterRowWhen_Merged{
			Merged: &frontendv1.RosterRowWhenMerged{AtMs: rec.MergedAt.UnixMilli()}}
	case rec.HiddenAt != nil:
		hidden := &frontendv1.RosterRowWhenHidden{AtMs: rec.HiddenAt.UnixMilli()}
		// THE COLUMN COUNTS DOWN TO THE LAPSE once the daemon knows it
		// (cachelapse.Keeper), and up from the hide until then.
		if rec.CacheLapse != nil {
			hidden.Cache = &frontendv1.RosterRowWhenHiddenCache{LapsesAtMs: rec.CacheLapse.LapsesAt.UnixMilli()}
		}
		out.Shown = &frontendv1.RosterRowWhen_Hidden{Hidden: hidden}
	case mergeStartedAt != nil:
		out.Shown = &frontendv1.RosterRowWhen_SyncMerging{
			SyncMerging: &frontendv1.RosterRowWhenSyncMerging{StartedAtMs: *mergeStartedAt}}
	case clock.TurnStartedAtMs != nil && clock.TurnEndedAtMs == nil:
		out.Shown = &frontendv1.RosterRowWhen_TurnRunning{
			TurnRunning: &frontendv1.RosterRowWhenTurnRunning{StartedAtMs: *clock.TurnStartedAtMs}}
	default:
		out.Shown = &frontendv1.RosterRowWhen_NoTurnRunning{NoTurnRunning: &frontendv1.RosterRowWhenNoTurnRunning{}}
	}
	return out
}

// detail composes the expanded panel. Each line is OMITTED when there is
// nothing to say rather than drawn blank, which is why each is its own message
// rather than a string on the panel. The commit count is the record's as the
// last turn close (or, merged, the landing) left it; a workspace never counted
// carries no count line.
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
	if rec.CommitCount != nil {
		out.Commits = &frontendv1.RosterRowDetailCommits{Count: *rec.CommitCount}
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

// logArmChange records a row's PUBLISHED status arm whenever it changes, and
// only then, on that workspace's own log sink. It is the roster's half of the
// footer's `daemon.footer.status_arm_changed`: with both, a disagreement
// between the strip and the sidebar or tab bar is diagnosable from the log
// alone, and a row that never leaves a fault shows which fact last moved it.
// The caller holds r.mu (logFor reads the registry).
func (r *resolver) logArmChange(ws ids.WorkspaceID, arm, previous, cause string) {
	r.logFor(ws).Info("daemon.sidebar.status_arm_changed", "the roster published a new status arm for the row",
		dlog.Context{"arm": arm, "previous_arm": previous, "cause": cause})
}
