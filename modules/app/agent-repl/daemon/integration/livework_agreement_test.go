//go:build integration

package integration

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/integration/harness"
)

// TestRosterAndFooterAgreeOnBackgroundWork is the executable form of the
// owner's cross-surface ruling: a workspace's status renders on the tab-bar,
// the sidebar and the footer, and none may disagree with either of the others.
// Whether background work exists is part of that status.
//
// Both arms resolve from ONE value — the session watcher's live-work set,
// which the watcher fans out to the roster and to the footer on the same edge.
// The regression it guards is the one the owner photographed: a footer
// reporting a background task for workspace random-puzzle-analysis while the
// sidebar and the tab-bar said ready.
//
// It drives ONE detached item through its whole life and asserts both surfaces
// at each edge of it.
func TestRosterAndFooterAgreeOnBackgroundWork(t *testing.T) {
	t.Parallel()

	// Arrange: one opened workspace, watched on both surfaces, quiet.
	f := newOpened(t, harness.Opts{})
	footer := f.d.WatchFooter(f.ws)
	roster := f.d.WatchRoster()
	rowIs := func(pred func(*frontendv1.RosterRow) bool) func(*frontendv1.WorkspaceRoster) bool {
		return func(r *frontendv1.WorkspaceRoster) bool {
			row := rosterRow(r, f.ws.GetId())
			return row != nil && pred(row)
		}
	}
	awaitRoster(t, f.d, roster, "the roster ready before any work",
		rowIs(func(row *frontendv1.RosterRow) bool { return row.GetReady() != nil }))
	awaitFooter(t, f, footer, "the footer idle before any work", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetIdle() != nil
	})

	// Act: a detached shell is announced with no turn in flight.
	pushDetachedShell(f.shim, "work-agree-1", "sleep 100")

	// Assert: the roster says idle_async and the footer says background.
	awaitRoster(t, f.d, roster, "the roster's idle_async arm",
		rowIs(func(row *frontendv1.RosterRow) bool { return row.GetIdleAsync() != nil }))
	awaitFooter(t, f, footer, "the footer's background arm", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetBackground() != nil
	})

	// Act: the shell ends, which empties the watcher's live-work set.
	f.shim.PushBash("work-agree-1", &conversationv1.AgentBash{Result: &conversationv1.AgentBash_Success{
		Success: &conversationv1.AgentBashSuccess{
			Command: &conversationv1.AgentBashCommand{Line: "sleep 100"},
			Outcome: &conversationv1.AgentBashSuccess_Completed{Completed: &conversationv1.AgentBashCompleted{
				Output: &conversationv1.AgentBashOutput{Form: &conversationv1.AgentBashOutput_Text{
					Text: &conversationv1.AgentBashOutputText{
						Stdout: "done\n",
						Extent: &conversationv1.AgentBashOutputText_Whole{Whole: &conversationv1.AgentBashOutputWhole{}},
					}}},
				Termination: &conversationv1.AgentBashTermination{
					How: &conversationv1.AgentBashTermination_Exited{Exited: &conversationv1.AgentBashExited{Code: 0}}},
			}},
			SettledAt: settledAt(2),
		},
	}})

	// Assert: BOTH surfaces retire the background account, and the footer's
	// live-work chips go with it.
	awaitRoster(t, f.d, roster, "the roster leaving idle_async",
		rowIs(func(row *frontendv1.RosterRow) bool { return row.GetIdleAsync() == nil }))
	awaitFooter(t, f, footer, "the footer leaving background", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetBackground() == nil &&
			v.GetStrip().GetLiveWork().GetShells() == nil
	})
}
