// LEASE RELEASE ON EVERY TERMINAL PHASE, end to end: merged, merge_failed, and
// an abandoned merge_conflict each hand the workspace's shim back.
//
// WHY ALL THREE ROWS AND NOT JUST THE HAPPY ONE. A lease released only on
// success is a lease that leaks on every failure, and the leak is invisible
// until a user tries to prompt the workspace and is told the merge owns it —
// forever, by a merge that finished long ago. merge.Lease.Acquire states the
// contract explicitly: Release "is called on EVERY terminal phase — merged,
// merge_failed, and an abandoned merge_conflict alike — so a lease never
// outlives the merge that took it". Each row here is one of those three, and
// each is provoked by a genuinely different mechanism rather than by asking the
// daemon to pretend.
//
// THE THREE PROVOCATIONS.
//   - merged: a sibling worktree whose commit touches a file nothing else does.
//   - merge_failed: a target carrying an UNTRACKED file the pick would
//     overwrite, so git refuses outright and leaves nothing to resolve.
//   - abandoned merge_conflict: a real conflict, then the workspace is CLOSED
//     without resolving it. Abandonment has no command of its own on the wire;
//     closing the workspace the merge was for is the user action that means it.
//
// Reuses mergequeue_e2e_test.go's fixtures / mergeWatch and
// mergelease_e2e_test.go's lease predicates READ-ONLY.
package e2e

import (
	"fmt"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"github.com/gorilla/websocket"
)

// sendCloseWorkspace dispatches the close/discard command for a workspace.
func sendCloseWorkspace(t *testing.T, conn *websocket.Conn, requestID, workspace string) {
	t.Helper()
	writeCmd(t, conn, fmt.Sprintf(`{"requestId":%q,"workspace":%q,"closeWorkspace":{}}`, requestID, workspace))
}

// TestE2EEveryTerminalMergePhaseReleasesTheLease drives each terminal phase and
// asserts the lease that the merge took is published as released by the time
// that phase has been reported.
func TestE2EEveryTerminalMergePhaseReleasesTheLease(t *testing.T) {
	tests := []struct {
		name string
		// worktree builds the sibling whose merge produces the row's phase.
		worktree func(repo *mergeRepo, branch string) string
		// terminal is the phase the merge must end on.
		terminal frontendv1.RenderState
		// abandon, when set, is the user action that gives up on a conflict the
		// merge parked in the target tree.
		abandon bool
	}{
		{
			name:     "a merge that landed releases the lease",
			worktree: func(repo *mergeRepo, branch string) string { return repo.cleanWorktree(branch) },
			terminal: phaseMerged,
		},
		{
			name:     "a merge git refused releases the lease",
			worktree: func(repo *mergeRepo, branch string) string { return repo.blockedWorktree(branch) },
			terminal: phaseMergeFailed,
		},
		{
			name:     "an abandoned conflict releases the lease",
			worktree: func(repo *mergeRepo, branch string) string { return repo.conflictingWorktree(branch) },
			terminal: phaseMergeConflict,
			abandon:  true,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange — a live workspace (the lease is a claim over a shim, so
			// there has to be one) on the row's fixture.
			h := newUDSHarness(t)
			repo := newMergeRepo(t)
			const branch = "feature-release"
			wsDir := tc.worktree(repo, branch)
			_, conn, _, _ := liveSession(t, h, wsDir)
			w := newMergeWatch(t, conn)

			// Act — merge, then (for the abandonment row) give up on it.
			sendMerge(t, conn, "r-merge-release", mergeCmdFor(t, h.geometry, repo, wsDir, branch))
			w.awaitOKAck("r-merge-release")
			w.awaitLeaseHeld(wsDir)
			w.awaitPhase(wsDir, tc.terminal)
			if tc.abandon {
				sendCloseWorkspace(t, conn, "r-abandon-conflict", wsDir)
			}

			// Assert — the shim is handed back, visibly.
			w.awaitLeaseReleasedAtOrAfter(wsDir, tc.terminal)
		})
	}
}
