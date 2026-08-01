package sessioncontroller

import (
	"fmt"

	"claude-repld/internal/ssm"
)

// TeardownMerged stands a merged workspace's session down. It is the second
// half of ssm.SessionAuthority (InterruptForMerge, in mergelease.go, is the
// first), and the SSM drives it off the `merged` transition itself.
//
// IT IS HIBERNATION, NOT DELETION, and the choice is deliberate. A merged
// workspace's commits are on the target and nothing is left for its shim to do,
// so the ~500MB it holds is owed back — but its conversation is still the
// account of how the work got written, and the user still reads it. Hibernation
// is the daemon's existing session-scoped teardown that reclaims the process
// and leaves the registry record revivable; deleting the session would throw
// the account away to save nothing extra.
//
// NOTHING TO STAND DOWN IS NOT A FAILURE. A workspace merged after the idle
// sweeper already reaped it, or merged from a sibling worktree without ever
// having been opened here, has no live session controller — the post-merge
// condition already holds, reached without this call's help. It is reported
// rather than passed over in silence, exactly as the merge interrupt reports
// the same discovery.
func (m *Manager) TeardownMerged(workspace string) error {
	if workspace == "" {
		return fmt.Errorf("session-controller: merged teardown got an empty workspace")
	}
	d, err := m.existing(workspace)
	if err != nil {
		m.logf("session-controller: merged teardown ws=%q decision=none (%v) — the merge landed over a workspace with no live session controller, so there is nothing to stand down",
			workspace, err)
		return nil
	}
	m.logf("session-controller: merged teardown ws=%q session=%s decision=stand_down — the merge landed, so the daemon hibernates the session and SIGTERMs its shim",
		workspace, d.sessionID)
	if err := m.Hibernate(workspace); err != nil {
		m.logf("session-controller: merged teardown FAILED ws=%q session=%s: %v — the merge STILL LANDED and the workspace stays merged; its shim is running and holding its memory",
			workspace, d.sessionID, err)
		return fmt.Errorf("session-controller: merged teardown for workspace %q: %w", workspace, err)
	}
	m.logf("session-controller: merged teardown ws=%q session=%s decision=complete — the session is hibernated", workspace, d.sessionID)
	return nil
}

// The SSM's port is the compile-time obligation, not a comment. Both halves
// live on this type, which is what makes "the fleet that ran the merge is the
// fleet that stands it down" a fact the compiler keeps.
var _ ssm.SessionAuthority = (*Manager)(nil)
