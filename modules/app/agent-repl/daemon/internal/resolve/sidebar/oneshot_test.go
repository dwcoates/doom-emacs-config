package sidebar_test

import (
	"testing"
	"time"

	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/resolve/footer"
	"claude-repld/internal/wsm"
)

// oneShotWorkspace is the test workspace, created as a one-shot.
func oneShotWorkspace() wsm.Workspace {
	ws := workspace(string(theWS), "one")
	ws.OneShot = true
	return ws
}

// outcomeName names a row's one-shot mark: "absent" when the row carries
// none, "unset" for a mark with no outcome (a contract breach).
func outcomeName(row *frontendv1.RosterRow) string {
	mark := row.GetOneShot()
	if mark == nil {
		return "absent"
	}
	switch mark.GetOutcome().(type) {
	case *frontendv1.RosterRowOneShot_Armed:
		return "armed"
	case *frontendv1.RosterRowOneShot_Failed:
		return "failed"
	default:
		return "unset"
	}
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

func TestOneShotOutcome(t *testing.T) {
	cases := []struct {
		name    string
		ws      wsm.Workspace
		arrange func(r sidebarResolver)
		want    string
	}{
		{
			name:    "a workspace that is not a one-shot carries no mark",
			ws:      workspace(string(theWS), "one"),
			arrange: func(r sidebarResolver) { prompt(r); endTurn(r) },
			want:    "absent",
		},
		{
			name:    "a one-shot whose first turn has not ended is armed",
			ws:      oneShotWorkspace(),
			arrange: func(r sidebarResolver) { prompt(r); r.AckTurn(theWS) },
			want:    "armed",
		},
		{
			name:    "a turn ending with no merge asked for fails it",
			ws:      oneShotWorkspace(),
			arrange: func(r sidebarResolver) { prompt(r); endTurn(r) },
			want:    "failed",
		},
		{
			name: "a turn ending after its merge was requested keeps it armed",
			ws:   oneShotWorkspace(),
			arrange: func(r sidebarResolver) {
				prompt(r)
				r.SetMergeRequested(theWS, true)
				endTurn(r)
			},
			want: "armed",
		},
		{
			name: "a request withdrawn before the turn ends fails it",
			ws:   oneShotWorkspace(),
			arrange: func(r sidebarResolver) {
				prompt(r)
				r.SetMergeRequested(theWS, true)
				r.SetMergeRequested(theWS, false)
				endTurn(r)
			},
			want: "failed",
		},
		{
			name: "a turn ending while the merge runs keeps it armed",
			ws:   oneShotWorkspace(),
			arrange: func(r sidebarResolver) {
				r.SetMerge(theWS, mergeFacts("merging"))
				prompt(r)
				endTurn(r)
			},
			want: "armed",
		},
		{
			name: "a merge in line keeps it armed",
			ws:   oneShotWorkspace(),
			arrange: func(r sidebarResolver) {
				prompt(r)
				r.SetMergeRequested(theWS, true)
				endTurn(r)
				r.SetMerge(theWS, mergeFacts("queued"))
			},
			want: "armed",
		},
		{
			name: "a merge failing fails it",
			ws:   oneShotWorkspace(),
			arrange: func(r sidebarResolver) {
				prompt(r)
				r.SetMergeRequested(theWS, true)
				endTurn(r)
				r.SetMerge(theWS, mergeFacts("queued"))
				r.SetMerge(theWS, mergeFacts("failed"))
			},
			want: "failed",
		},
		{
			name: "a merge landing keeps it armed",
			ws:   oneShotWorkspace(),
			arrange: func(r sidebarResolver) {
				prompt(r)
				r.SetMergeRequested(theWS, true)
				endTurn(r)
				r.SetMerge(theWS, mergeFacts("merged"))
			},
			want: "armed",
		},
		{
			name: "a submitted turn that has not reached thinking leaves it failed",
			ws:   oneShotWorkspace(),
			arrange: func(r sidebarResolver) {
				prompt(r)
				endTurn(r)
				prompt(r)
			},
			want: "failed",
		},
		{
			name: "a turn reaching thinking clears a failure",
			ws:   oneShotWorkspace(),
			arrange: func(r sidebarResolver) {
				prompt(r)
				endTurn(r)
				prompt(r)
				r.AckTurn(theWS)
			},
			want: "armed",
		},
		{
			name: "a failed merge clears once a turn reaches thinking",
			ws:   oneShotWorkspace(),
			arrange: func(r sidebarResolver) {
				r.SetMerge(theWS, mergeFacts("failed"))
				prompt(r)
				r.AckTurn(theWS)
			},
			want: "armed",
		},
		{
			name: "a merge requested after a failure clears it",
			ws:   oneShotWorkspace(),
			arrange: func(r sidebarResolver) {
				prompt(r)
				endTurn(r)
				r.SetMergeRequested(theWS, true)
			},
			want: "armed",
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			r := live(t, arrange(t, tc.ws))

			// Act.
			tc.arrange(r)

			// Assert.
			if got := outcomeName(onlyRow(t, r)); got != tc.want {
				t.Fatalf("one_shot = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestOneShotOutcomeIsArmedOnThePushThatEndsARequestingTurn(t *testing.T) {
	// Arrange: the request is recorded inside the turn, as the merge verb does.
	r := live(t, arrange(t, oneShotWorkspace()))
	prompt(r)
	r.AckTurn(theWS)
	r.SetMergeRequested(theWS, true)

	// Act.
	endTurn(r)

	// Assert: the push the turn's end publishes, before any merge fact,
	// already reads the one-shot as armed.
	row := onlyRow(t, r)
	if got := statusName(row); got != "done" {
		t.Fatalf("status = %q, want done", got)
	}
	if got := outcomeName(row); got != "armed" {
		t.Fatalf("one_shot = %q, want armed on the push that ends the turn", got)
	}
}

func TestOneShotOutcomeSeedsFromTheDurableRecord(t *testing.T) {
	merged := epoch.Add(time.Minute)
	cases := []struct {
		name   string
		mutate func(ws *wsm.Workspace)
		want   string
	}{
		{
			name:   "no result stands",
			mutate: func(*wsm.Workspace) {},
			want:   "armed",
		},
		{
			name: "a result with no merge standing",
			mutate: func(ws *wsm.Workspace) {
				ws.Result = &wsm.TurnResult{End: wsm.TurnResultDone}
			},
			want: "failed",
		},
		{
			name: "a result with a merge standing",
			mutate: func(ws *wsm.Workspace) {
				ws.Result = &wsm.TurnResult{End: wsm.TurnResultDone}
				ws.MergePending = true
			},
			want: "armed",
		},
		{
			name: "a result with a landed merge",
			mutate: func(ws *wsm.Workspace) {
				ws.Result = &wsm.TurnResult{End: wsm.TurnResultDone}
				ws.MergedAt = &merged
			},
			want: "armed",
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			ws := oneShotWorkspace()
			tc.mutate(&ws)

			// Act.
			r := arrange(t, ws)

			// Assert: a merged workspace files under recently_merged.
			roster := latest(t, r)
			rows := append(repoRows(t, roster), roster.GetRecentlyMerged().GetRows().GetRows()...)
			row := rowFor(rows, string(theWS))
			if row == nil {
				t.Fatal("the roster carried no row for the workspace")
			}
			if got := outcomeName(row); got != tc.want {
				t.Fatalf("one_shot = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestOneShotSeedNeverOverridesALiveEdge(t *testing.T) {
	// Arrange: a live turn end fails the one-shot.
	ws := oneShotWorkspace()
	r := live(t, arrange(t, ws))
	prompt(r)
	endTurn(r)

	// Act: a later registry carries a record that would seed it armed.
	r.SetRegistry(registry(ws))

	// Assert.
	if got := outcomeName(onlyRow(t, r)); got != "failed" {
		t.Fatalf("one_shot = %q, want failed: the seed must not override a live edge", got)
	}
}

func TestOneShotOutcomeChangeIsRecorded(t *testing.T) {
	cases := []struct {
		name     string
		arrange  func(r sidebarResolver)
		wantEdge string
		wantFail bool
	}{
		{
			name:     "a bare turn end",
			arrange:  func(r sidebarResolver) { prompt(r); endTurn(r) },
			wantEdge: "turn_ended_without_merge",
			wantFail: true,
		},
		{
			name:     "a failed merge",
			arrange:  func(r sidebarResolver) { r.SetMerge(theWS, mergeFacts("failed")) },
			wantEdge: "merge_failed",
			wantFail: true,
		},
		{
			name: "a turn reaching thinking",
			arrange: func(r sidebarResolver) {
				prompt(r)
				endTurn(r)
				prompt(r)
				r.AckTurn(theWS)
			},
			wantEdge: "turn_reached_thinking",
			wantFail: false,
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			r := live(t, arrange(t, oneShotWorkspace()))

			// Act.
			tc.arrange(r)

			// Assert.
			for _, rec := range r.surfaces.Records() {
				if rec.Operation == "daemon.sidebar.one_shot_outcome" &&
					rec.Context["edge"] == tc.wantEdge && rec.Context["failed"] == tc.wantFail {
					return
				}
			}
			t.Fatalf("no daemon.sidebar.one_shot_outcome record with edge %q and failed %v", tc.wantEdge, tc.wantFail)
		})
	}
}
