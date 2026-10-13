package sidebar_test

import (
	"testing"

	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/wsm"
)

// oneShotWorkspace is the test workspace, created as a one-shot.
func oneShotWorkspace() wsm.Workspace {
	ws := workspace(string(theWS), "one")
	ws.OneShot = true
	return ws
}

// prompt starts an ordinary prompt's turn.
func prompt(r sidebarResolver) {
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})
}

// endTurn ends the turn in flight as completed.
func endTurn(r sidebarResolver) {
	r.SetTurnEnded(theWS, wsm.CloseCompleted)
}

// mergeFacts builds merge facts in the given state.
func mergeFacts(state string) footer.MergeFacts {
	facts := footer.MergeFacts{State: state}
	if state == "queued" {
		facts.Step = footer.StepEnqueued
	}
	return facts
}

// THE ONE-SHOT MARK IS PRESENCE ALONE (frontend.v1.RosterRowOneShot): it says
// the workspace is a one-shot, for its whole life, whatever it is doing. Which
// arms draw the glyph is the shared vocabulary's, never the daemon's.
func TestOneShotMarkIsPresentExactlyForAOneShot(t *testing.T) {
	cases := []struct {
		name    string
		ws      wsm.Workspace
		arrange func(r sidebarResolver)
		want    bool
	}{
		{
			name:    "a workspace that is not a one-shot carries no mark",
			ws:      workspace(string(theWS), "one"),
			arrange: func(r sidebarResolver) { prompt(r); r.AckTurn(theWS) },
			want:    false,
		},
		{
			name:    "a one-shot that is thinking carries the mark",
			ws:      oneShotWorkspace(),
			arrange: func(r sidebarResolver) { prompt(r); r.AckTurn(theWS) },
			want:    true,
		},
		{
			name:    "a one-shot whose turn ended carries the mark",
			ws:      oneShotWorkspace(),
			arrange: func(r sidebarResolver) { prompt(r); endTurn(r) },
			want:    true,
		},
		{
			name:    "a one-shot whose merge failed carries the mark",
			ws:      oneShotWorkspace(),
			arrange: func(r sidebarResolver) { prompt(r); endTurn(r); r.SetMerge(theWS, mergeFacts("failed")) },
			want:    true,
		},
		{
			name:    "a one-shot whose merge is in line carries the mark",
			ws:      oneShotWorkspace(),
			arrange: func(r sidebarResolver) { prompt(r); endTurn(r); r.SetMerge(theWS, mergeFacts("queued")) },
			want:    true,
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			r := live(t, arrange(t, tc.ws))

			// Act.
			tc.arrange(r)

			// Assert.
			if got := onlyRow(t, r).GetOneShot() != nil; got != tc.want {
				t.Fatalf("one_shot present = %v, want %v", got, tc.want)
			}
		})
	}
}
