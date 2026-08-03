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
// Retention is IN-MEMORY ONLY. A restarted daemon has no roster until Emacs
// republishes on reconnect.
//
// The monotonicity floor is keyed by PUBLISHER EPOCH — the roster's boot_id,
// minted once per Emacs boot — not held globally. Revisions are monotonic only
// WITHIN one boot_id; a publish carrying a different boot_id opens a new epoch
// and is accepted whatever its revision, resetting the floor to it. That is
// what lets a fresh Emacs publish successfully against a surviving daemon still
// retaining a high revision from the previous boot, and it is why no resync
// path exists: there is no retained state a new publisher must first learn.

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
// Four things make a roster unrenderable, and all four are refused loudly
// rather than normalized into something plausible:
//
//   - A non-positive revision. Revisions are monotonic and start at 1, so 0 is
//     the zero value of an unset field, not a legitimate first publication.
//   - An empty boot_id. boot_id is the epoch key the revision floor is keyed
//     by, so a publisher that will not identify its epoch cannot be told apart
//     from one whose epoch ended. Refused rather than silently folded into
//     whatever epoch happens to be retained.
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
	if roster.GetBootId() == "" {
		return fmt.Errorf("frontend: workspace roster revision=%d has an empty boot_id; a publisher must identify its epoch", roster.GetRevision())
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
// The monotonicity check is SCOPED TO THE PUBLISHER EPOCH. Within one boot_id a
// revision that does not advance is REFUSED, naming both revisions: delivery is
// not idempotent-by-revision downstream, so silently dropping a stale
// publication would leave the publisher believing a roster it authored is on
// screen when a newer one is.
//
// A publish whose boot_id DIFFERS from the retained roster's is a different
// publisher epoch, and its revision is not comparable to the retained one at
// all — the two counters were minted by different Emacs boots. It is accepted
// unconditionally, resetting the floor, and the epoch change is logged. Without
// this a fresh Emacs booting against a surviving daemon would be refused
// forever, with no honest resync available to it (a nack carries no retained
// revision to resync against).
func (s *Server) PublishWorkspaceRoster(roster *frontendv1.WorkspaceRoster) error {
	if err := validateRoster(roster); err != nil {
		s.logf("frontend: workspace roster rejected: %v", err)
		return err
	}
	s.mu.Lock()
	held := s.roster
	epochChanged := held != nil && held.GetBootId() != roster.GetBootId()
	if held != nil && !epochChanged && roster.GetRevision() <= held.GetRevision() {
		heldRevision := held.GetRevision()
		s.mu.Unlock()
		err := fmt.Errorf("frontend: workspace roster revision=%d does not supersede the retained revision=%d for boot_id=%q; refusing",
			roster.GetRevision(), heldRevision, roster.GetBootId())
		s.logf("frontend: workspace roster rejected: %v", err)
		return err
	}
	var heldBootID string
	var heldRevision int64
	if epochChanged {
		heldBootID, heldRevision = held.GetBootId(), held.GetRevision()
	}
	s.roster = roster
	slow := s.deliverLocked(WorkspaceRosterFrame(roster), func(*client) bool { return true })
	clients := len(s.clients)
	s.mu.Unlock()
	s.disconnectAll(slow)
	if epochChanged {
		s.logf("frontend: workspace roster publisher epoch changed: boot_id=%q revision=%d -> boot_id=%q revision=%d; monotonicity floor reset",
			heldBootID, heldRevision, roster.GetBootId(), roster.GetRevision())
	}
	s.logVerbosef("frontend: workspace roster retained boot_id=%q revision=%d current_dir=%q nav_dir=%q clients=%d",
		roster.GetBootId(), roster.GetRevision(), roster.GetCurrentDir(), roster.GetNavDir(), clients)
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
