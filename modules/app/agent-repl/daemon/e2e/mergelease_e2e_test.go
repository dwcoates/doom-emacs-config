// THE MERGE LEASE, end to end over a LIVE session: while merge.Coordinator
// holds the exclusivity claim on a workspace's shim, that workspace's shim
// belongs to the merge — the user cannot prompt it, and everything it produces
// is stamped with the merge's provenance.
//
// WHY THE SESSION IS REAL HERE. The other merge e2e files drive merges with no
// session at all, because a queue and a cherry-pick need none. The lease is the
// opposite: it is a claim OVER a session's shim, so a test of it without a shim
// would be a test of a variable. These tests therefore bring the workspace up
// through liveSession (clearcompact_e2e_test.go) — the real shim, in --fake
// mode — and merge that same workspace, so the lease lands on a session that is
// genuinely there to be taken.
//
// WHY THE MERGE CONFLICTS. The lease exists so merge.Coordinator can borrow the
// workspace's own shim to RESOLVE a conflict (frontend.proto ConversationSource:
// "the session holding the full context of the work is the one best equipped to
// resolve it"). A clean pick has nothing to resolve and so nothing to hold the
// lease long enough to observe, which is why every test here parks on a
// conflict.
//
// Reuses mergequeue_e2e_test.go's fixtures / mergeWatch and
// clearcompact_e2e_test.go's liveSession READ-ONLY. This file is the helper
// home for the two lease predicates, which mergeleaserelease reuses.
package e2e

import (
	"fmt"
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// --- lease observation -------------------------------------------------------

// phaseIndex returns the position in the recorded state stream of the first
// WorkspaceState for ws in phase, or -1. It exists so an assertion can be
// scoped to what happened AT OR AFTER a transition rather than to the whole
// history — the difference between "the lease was released by the terminal
// phase" and "the lease had not been taken yet when we started looking".
func (w *mergeWatch) phaseIndex(ws string, phase frontendv1.RenderState) int {
	for i, state := range w.states {
		if state.GetWorkspace() == ws && stateInPhase(state, phase) {
			return i
		}
	}
	return -1
}

// awaitLeaseHeld blocks until a pushed WorkspaceState reports the lease on ws
// as held, and returns it.
func (w *mergeWatch) awaitLeaseHeld(ws string) *frontendv1.WorkspaceState {
	w.t.Helper()
	var held *frontendv1.WorkspaceState
	w.until("a WorkspaceState reporting merge_lease_held for "+ws, func() bool {
		for _, state := range w.states {
			if state.GetWorkspace() == ws && state.GetMergeLeaseHeld() {
				held = state
				return true
			}
		}
		return false
	})
	return held
}

// awaitLeaseReleasedAtOrAfter blocks until a WorkspaceState pushed AT OR AFTER
// ws's transition into phase reports the lease as released.
//
// At-or-after, not merely "eventually", because the frontend's whole account of
// the lease is the pushed flag: a release the daemon performs but never
// publishes leaves every frontend rendering a workspace the user can no longer
// prompt, forever, with nothing on the wire to say otherwise. So the release is
// required to be VISIBLE, and it is required to be visible by the time the
// terminal phase has been reported.
func (w *mergeWatch) awaitLeaseReleasedAtOrAfter(ws string, phase frontendv1.RenderState) *frontendv1.WorkspaceState {
	w.t.Helper()
	var released *frontendv1.WorkspaceState
	w.until(fmt.Sprintf("a WorkspaceState for %s releasing the merge lease at or after %s", ws, phase), func() bool {
		from := w.phaseIndex(ws, phase)
		if from < 0 {
			return false
		}
		for _, state := range w.states[from:] {
			if state.GetWorkspace() == ws && !state.GetMergeLeaseHeld() {
				released = state
				return true
			}
		}
		return false
	})
	return released
}

// mergeUnderLiveSession brings a workspace up as a real session on a
// conflicting sibling worktree and starts its merge, returning the watch and
// the workspace key once the lease is provably held.
func mergeUnderLiveSession(t *testing.T, branch, requestID string) (*mergeWatch, string) {
	t.Helper()
	h := newUDSHarness(t)
	repo := newMergeRepo(t)
	wsDir := repo.conflictingWorktree(branch)
	_, conn, _, _ := liveSession(t, h, wsDir)
	w := newMergeWatch(t, conn)
	sendMerge(t, conn, requestID, mergeCmdFor(t, h.geometry, repo, wsDir, branch))
	w.awaitOKAck(requestID)
	w.awaitLeaseHeld(wsDir)
	return w, wsDir
}

// --- tests -------------------------------------------------------------------

// TestE2EAMergeInFlightPublishesItsLeaseOnTheWorkspaceState is the precondition
// every other lease assertion rests on: the claim is a published fact, not a
// daemon-internal one. Without it a frontend could not explain to the user why
// their prompt was refused.
func TestE2EAMergeInFlightPublishesItsLeaseOnTheWorkspaceState(t *testing.T) {
	// Arrange + Act — a merge of a live workspace.
	w, wsDir := mergeUnderLiveSession(t, "feature-lease-visible", "r-merge-lease-visible")

	// Assert — the workspace whose shim the merge took is the workspace the
	// flag is set on.
	held := w.awaitLeaseHeld(wsDir)
	if got := held.GetWorkspace(); got != wsDir {
		t.Errorf("merge_lease_held reported for workspace %q, want %q", got, wsDir)
	}
}

// TestE2EAUserPromptUnderTheMergeLeaseIsRefusedLoudly covers the REFUSAL: the
// merge owns the session while it holds the lease, so a user submission is
// nacked with an explanation rather than silently dropped, silently queued, or
// silently interleaved into the merge's own turn.
func TestE2EAUserPromptUnderTheMergeLeaseIsRefusedLoudly(t *testing.T) {
	// Arrange — a live workspace whose merge holds the lease.
	w, wsDir := mergeUnderLiveSession(t, "feature-lease-refuses", "r-merge-lease-refuses")

	// Act — the user tries to prompt the session the merge is holding.
	const promptRequest = "r-prompt-under-lease"
	writeCmd(t, w.conn, fmt.Sprintf(`{"requestId":%q,"submitPrompt":{"text":"can you also do this while you are in there","promptOrigin":"PROMPT_ORIGIN_USER_SENT"}}`, promptRequest))

	// Assert — a loud, explanatory nack.
	ack := w.awaitAck(promptRequest)
	if ack.GetOk() {
		t.Fatalf("a user prompt was ACCEPTED for %s while its merge held the lease", wsDir)
	}
	if !strings.Contains(strings.ToLower(ack.GetError()), "merge") {
		t.Errorf("the refusal for %s reads %q, which never mentions the merge holding the session: the user is told no without being told why",
			wsDir, ack.GetError())
	}
}

// TestE2EConversationItemsCarryWhoDroveTheirTurn covers PROVENANCE: an ordinary
// turn's items are stamped USER, and the turn merge.Coordinator drives under
// its lease is stamped MERGE.
//
// Provenance is a durable fact on every item rather than a rendering hint
// (frontend.proto ConversationSource), so both rows also assert the field is
// never left UNSPECIFIED — a receiver seeing that is "looking at a malformed
// frame and must reject it loudly rather than assume USER", which is only
// enforceable if the daemon never emits one.
func TestE2EConversationItemsCarryWhoDroveTheirTurn(t *testing.T) {
	tests := []struct {
		name string
		// drive produces a turn on the live workspace and returns once the
		// turn's producer has been given the session.
		drive func(t *testing.T, w *mergeWatch, geo mergeGeometryRecorder, repo *mergeRepo, wsDir, branch string)
		// want is the provenance every item that turn produces must carry.
		want frontendv1.ConversationSource
	}{
		{
			name: "an ordinary prompt's turn is the user's",
			drive: func(t *testing.T, w *mergeWatch, _ mergeGeometryRecorder, _ *mergeRepo, _, _ string) {
				t.Helper()
				writeCmd(t, w.conn, `{"requestId":"r-user-turn","submitPrompt":{"text":"an ordinary prompt","promptOrigin":"PROMPT_ORIGIN_USER_SENT"}}`)
				w.awaitOKAck("r-user-turn")
			},
			want: frontendv1.ConversationSource_CONVERSATION_SOURCE_USER,
		},
		{
			name: "a turn driven under the merge lease is the merge's",
			drive: func(t *testing.T, w *mergeWatch, geo mergeGeometryRecorder, repo *mergeRepo, wsDir, branch string) {
				t.Helper()
				sendMerge(t, w.conn, "r-merge-turn", mergeCmdFor(t, geo, repo, wsDir, branch))
				w.awaitOKAck("r-merge-turn")
				w.awaitLeaseHeld(wsDir)
			},
			want: frontendv1.ConversationSource_CONVERSATION_SOURCE_MERGE,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange — the same live workspace either way, on a worktree whose
			// merge will conflict (only the merge row needs the conflict; the
			// user row is indifferent to it, so the fixture stays one shape).
			h := newUDSHarness(t)
			repo := newMergeRepo(t)
			const branch = "feature-provenance"
			wsDir := repo.conflictingWorktree(branch)
			_, conn, _, _ := liveSession(t, h, wsDir)
			w := newMergeWatch(t, conn)

			// Act.
			tc.drive(t, w, h.geometry, repo, wsDir, branch)

			// Assert — the turn's items name their driver, and no item on the
			// workspace leaves the field unset.
			w.awaitItem(wsDir, "a conversation item stamped "+tc.want.String(), func(item *frontendv1.ConversationItem) bool {
				return item.GetSource() == tc.want
			})
			for _, item := range w.items[wsDir] {
				if item.GetSource() == frontendv1.ConversationSource_CONVERSATION_SOURCE_UNSPECIFIED {
					t.Errorf("conversation item uuid=%q reached the frontend with an UNSPECIFIED source", item.GetUuid())
				}
			}
		})
	}
}
