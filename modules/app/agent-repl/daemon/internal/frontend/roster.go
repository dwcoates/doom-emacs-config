package frontend

import (
	"fmt"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// ---------------------------------------------------------------------------
// The editor-global workspace roster
// ---------------------------------------------------------------------------
//
// Emacs is the roster's single author. The daemon is its RETAINER and FAN-OUT:
// it holds the newest published roster, hands it to every client on connect,
// and rebroadcasts each accepted publication to every connected frontend.
//
// The roster is EDITOR-GLOBAL, not per-workspace. There is exactly one of them
// for the whole daemon, it carries no session or workspace routing key, and it
// is therefore never scope-filtered (see scopeFrame) — a session-scoped webview
// renders the same sidebar as the Emacs host.
//
// Retention is IN-MEMORY ONLY and deliberately so. The roster's revision is
// monotonic per-Emacs-BOOT, so a roster that outlived the Emacs that authored
// it would carry a revision a restarted publisher could no longer beat, and
// every publication until the counter caught up would be refused. A restarted
// daemon therefore has no roster until Emacs republishes on reconnect.

// RosterRetainer accepts a roster publication, retains it if it supersedes what
// is already held, and fans it out. It is the narrow contract the command
// dispatcher needs; *Server is the implementation.
//
// A refusal is an ERROR, never a silent drop: a publisher must always be able
// to tell an accepted roster from a rejected one.
type RosterRetainer interface {
	PublishWorkspaceRoster(roster *frontendv1.WorkspaceRoster) error
}

// validateRoster rejects a roster no client could render.
//
// Three things make a roster unrenderable, and all three are refused loudly
// rather than normalized into something plausible:
//
//   - A non-positive revision. Revisions are monotonic and start at 1, so 0 is
//     the zero value of an unset field, not a legitimate first publication.
//   - An unset `view` oneof. The SET ARM IS THE GROUPING; with none set there
//     is no grouping and no rows, which is not the same thing as an empty
//     roster (an empty repository view is a perfectly good empty roster).
//   - A row whose `status` oneof is unset. The SET ARM IS THE STATUS, so an
//     unset one is a row with no lifecycle at all. Children are rows too and
//     are checked to the same standard, recursively.
func validateRoster(roster *frontendv1.WorkspaceRoster) error {
	if roster == nil {
		return fmt.Errorf("frontend: workspace roster is nil")
	}
	if roster.GetRevision() <= 0 {
		return fmt.Errorf("frontend: workspace roster revision must be positive, got %d", roster.GetRevision())
	}
	if roster.GetView() == nil {
		return fmt.Errorf("frontend: workspace roster revision=%d has no view set; exactly one of repository/task is required", roster.GetRevision())
	}
	for _, section := range roster.GetRepository().GetSections() {
		if err := validateRosterRows(roster.GetRevision(), fmt.Sprintf("repository section %q", section.GetRepoKey()), section.GetRows()); err != nil {
			return err
		}
	}
	for _, section := range roster.GetTask().GetSections() {
		if err := validateRosterRows(roster.GetRevision(), fmt.Sprintf("task section %q", section.GetTaskId()), section.GetRows()); err != nil {
			return err
		}
	}
	if err := validateRosterRows(roster.GetRevision(), "recently_merged", roster.GetRecentlyMerged().GetRows()); err != nil {
		return err
	}
	return nil
}

// validateRosterRows checks every row in one section, and every descendant of
// those rows, for a set status oneof. where names the section for the error.
func validateRosterRows(revision int64, where string, rows []*frontendv1.RosterRow) error {
	for _, row := range rows {
		if row == nil {
			return fmt.Errorf("frontend: workspace roster revision=%d %s carries a nil row", revision, where)
		}
		if row.GetStatus() == nil {
			return fmt.Errorf("frontend: workspace roster revision=%d %s row dir=%q has no status set; exactly one status arm is required",
				revision, where, row.GetDir())
		}
		if err := validateRosterRows(revision, fmt.Sprintf("%s child of dir=%q", where, row.GetDir()), row.GetChildren()); err != nil {
			return err
		}
	}
	return nil
}

// PublishWorkspaceRoster validates roster, retains it if it supersedes the one
// already held, and delivers it to every connected client.
//
// Retention and fan-out happen as ONE operation under the delivery lock, the
// same lock a connect registers under. That is what makes the two orderings
// that matter unbreakable rather than merely likely:
//
//   - A client connecting concurrently with a publication either registers
//     first (and receives the new roster as a broadcast) or registers second
//     (and receives it in its connect snapshot). It can never miss it, and it
//     can never be handed the older roster after the newer one.
//   - Two concurrent publications are serialized, so the roster that survives
//     retention is also the roster that was broadcast last.
//
// A revision that does not advance is REFUSED, naming both revisions. Delivery
// is not idempotent-by-revision downstream, so silently dropping a stale
// publication would leave the publisher believing a roster it authored is on
// screen when a newer one is.
func (s *Server) PublishWorkspaceRoster(roster *frontendv1.WorkspaceRoster) error {
	if err := validateRoster(roster); err != nil {
		s.logf("frontend: workspace roster rejected: %v", err)
		return err
	}
	s.mu.Lock()
	if held := s.roster; held != nil && roster.GetRevision() <= held.GetRevision() {
		heldRevision := held.GetRevision()
		s.mu.Unlock()
		err := fmt.Errorf("frontend: workspace roster revision=%d does not supersede the retained revision=%d; refusing",
			roster.GetRevision(), heldRevision)
		s.logf("frontend: workspace roster rejected: %v", err)
		return err
	}
	s.roster = roster
	slow := s.deliverLocked(WorkspaceRosterFrame(roster), func(*client) bool { return true })
	clients := len(s.clients)
	s.mu.Unlock()
	s.disconnectAll(slow)
	s.logVerbosef("frontend: workspace roster retained revision=%d current_dir=%q nav_dir=%q clients=%d",
		roster.GetRevision(), roster.GetCurrentDir(), roster.GetNavDir(), clients)
	return nil
}

// retainedRosterLocked returns the roster to include in a connect snapshot, or
// nil when nothing has been published yet. Caller holds mu.
//
// Nil is a REAL answer, not a missing one: before Emacs's first publication —
// and after a daemon restart, until Emacs republishes — there is no roster, and
// the connect sequence omits the frame rather than sending an empty roster no
// client could distinguish from a genuinely empty one.
func (s *Server) retainedRosterLocked() *frontendv1.WorkspaceRoster { return s.roster }
