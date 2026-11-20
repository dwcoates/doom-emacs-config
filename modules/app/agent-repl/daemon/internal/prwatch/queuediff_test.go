package prwatch

import (
	"reflect"
	"testing"

	"google.golang.org/protobuf/proto"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
)

func TestRunOutcomeJudgesTheWholeRun(t *testing.T) {
	tests := []struct {
		name        string
		runs        []remoteCheck
		gates       map[string]bool
		wantOutcome string
		wantFailed  []string
		wantCounted int
	}{
		{name: "no runs is undecided", wantOutcome: ""},
		{name: "every run passing passes", runs: []remoteCheck{done(1, "a", "success"), done(2, "b", "neutral"), done(3, "c", "skipped")}, wantOutcome: outcomePassed, wantCounted: 3},
		{name: "a run still going is undecided", runs: []remoteCheck{done(1, "a", "success"), {ID: 2, Name: "b"}}, wantOutcome: "", wantCounted: 2},
		{name: "a failure decides the run while others still go", runs: []remoteCheck{{ID: 1, Name: "a"}, done(2, "b", "failure")}, wantOutcome: outcomeFailed, wantFailed: []string{"b"}, wantCounted: 2},
		{name: "every conclusion but a pass fails", runs: []remoteCheck{done(1, "a", "cancelled"), done(2, "b", "timed_out"), done(3, "c", "action_required"), done(4, "d", "stale")},
			wantOutcome: outcomeFailed, wantFailed: []string{"a", "b", "c", "d"}, wantCounted: 4},
		{name: "the latest run of a name counts", runs: []remoteCheck{done(2, "a", "success"), done(1, "a", "failure")}, wantOutcome: outcomePassed, wantCounted: 1},
		{name: "a re-run in progress takes back its failure", runs: []remoteCheck{done(1, "a", "failure"), {ID: 2, Name: "a"}}, wantOutcome: "", wantCounted: 1},
		{name: "a gate check is not counted", runs: []remoteCheck{done(1, "a", "success"), done(2, "gate", "failure")}, gates: map[string]bool{"gate": true}, wantOutcome: outcomePassed, wantCounted: 1},
		{name: "a run of only gate checks has no outcome", runs: []remoteCheck{done(1, "gate", "success")}, gates: map[string]bool{"gate": true}, wantOutcome: ""},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			outcome, failed, counted := runOutcome(tt.runs, tt.gates)

			// Assert
			var names []string
			for _, c := range failed {
				names = append(names, c.Name)
			}
			if outcome != tt.wantOutcome || !reflect.DeepEqual(names, tt.wantFailed) || counted != tt.wantCounted {
				t.Fatalf("runOutcome = %q, %v, %d; want %q, %v, %d", outcome, names, counted, tt.wantOutcome, tt.wantFailed, tt.wantCounted)
			}
		})
	}
}

func TestDiffOutcomeTellsEachOutcomeOnce(t *testing.T) {
	passed := []remoteCheck{done(1, "a", "success")}
	failed := []remoteCheck{done(1, "a", "failure")}
	tests := []struct {
		name        string
		prevHead    string
		prev        string
		runs        []remoteCheck
		wantOutcome string
		wantTold    string
	}{
		{name: "a first outcome is told", prevHead: "h", runs: passed, wantOutcome: outcomePassed, wantTold: outcomePassed},
		{name: "an outcome already told is not told again", prevHead: "h", prev: outcomePassed, runs: passed, wantOutcome: outcomePassed},
		{name: "a failure after a pass is told", prevHead: "h", prev: outcomePassed, runs: failed, wantOutcome: outcomeFailed, wantTold: outcomeFailed},
		{name: "an undecided run is recorded undecided", prevHead: "h", prev: outcomeFailed, runs: []remoteCheck{{ID: 2, Name: "a"}}, wantOutcome: ""},
		{name: "a new head starts undecided", prevHead: "old", prev: outcomePassed, runs: passed, wantOutcome: outcomePassed, wantTold: outcomePassed},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			head, outcome, concluded := diffOutcome(tt.prevHead, tt.prev, "h", tt.runs, nil)

			// Assert
			told := ""
			switch {
			case concluded.GetPassed() != nil:
				told = outcomePassed
			case concluded.GetFailed() != nil:
				told = outcomeFailed
			}
			if head != "h" || outcome != tt.wantOutcome || told != tt.wantTold || (concluded != nil && concluded.GetCommitSha() != "h") {
				t.Fatalf("diffOutcome = %s, %q, %v; want %q told %q", head, outcome, concluded, tt.wantOutcome, tt.wantTold)
			}
		})
	}
}

func TestAFailedOutcomeNamesEachCheckThatDidNotPass(t *testing.T) {
	// Act
	_, _, concluded := diffOutcome("", "", "h", []remoteCheck{done(1, "b", "timed_out"), done(2, "a", "failure"), done(3, "c", "success")}, nil)

	// Assert
	want := &agentreplv1.PullRequestChecksFailed{FailedChecks: []*agentreplv1.PullRequestActivityCheckFinished{
		{Name: "a", Conclusion: conclusionProto("failure")}, {Name: "b", Conclusion: conclusionProto("timed_out")}}}
	if got := concluded.GetFailed(); !proto.Equal(got, want) {
		t.Fatalf("failed = %v, want %v", got, want)
	}
}

func TestAPassedOutcomeCountsTheChecksItJudged(t *testing.T) {
	// Act
	_, _, concluded := diffOutcome("", "", "h", []remoteCheck{done(1, "a", "success"), done(2, "b", "skipped"), done(3, "gate", "failure")}, map[string]bool{"gate": true})

	// Assert
	if got := concluded.GetPassed().GetCheckCount(); got != 2 {
		t.Fatalf("check count = %d, want 2", got)
	}
}

// armsOf names each activity's arm, timed first.
func armsOf(timed []timedActivity, after []*agentreplv1.PullRequestActivity) []string {
	var out []string
	for _, a := range timed {
		out = append(out, string(a.activity.ProtoReflect().WhichOneof(a.activity.ProtoReflect().Descriptor().Oneofs().ByName("activity")).Name()))
	}
	for _, a := range after {
		out = append(out, string(a.ProtoReflect().WhichOneof(a.ProtoReflect().Descriptor().Oneofs().ByName("activity")).Name()))
	}
	return out
}

func TestDiffQueueTellsThePullRequestsWayThroughItsQueue(t *testing.T) {
	added := remoteQueueEvent{ID: "a1", Actor: "dodge", At: t0}
	removed := remoteQueueEvent{ID: "r1", Removed: true, Reason: ptr("checks failed"), At: t0.Add(1)}
	queued := &remoteEntryFacts{State: "QUEUED"}
	tests := []struct {
		name      string
		q         prQueueSnapshot
		merged    bool
		prev      told
		wantArms  []string
		wantPlace string
	}{
		{name: "a joining is told with where it stands", q: prQueueSnapshot{Events: []remoteQueueEvent{added}, Entry: queued, Position: &position{2, 3}},
			prev: told{}, wantArms: []string{"merge_queue_entered"}, wantPlace: "2/3/QUEUED"},
		{name: "a joining already told is not told again", q: prQueueSnapshot{Events: []remoteQueueEvent{added}, Entry: queued, Position: &position{2, 3}},
			prev: told{QueueEvents: map[string]string{"a1": "added"}, Queue: "2/3/QUEUED"}, wantPlace: "2/3/QUEUED"},
		{name: "a move up is told", q: prQueueSnapshot{Events: []remoteQueueEvent{added}, Entry: queued, Position: &position{0, 2}},
			prev: told{QueueEvents: map[string]string{"a1": "added"}, Queue: "2/3/QUEUED"}, wantArms: []string{"merge_queue_moved"}, wantPlace: "0/2/QUEUED"},
		{name: "a state change is told", q: prQueueSnapshot{Events: []remoteQueueEvent{added}, Entry: &remoteEntryFacts{State: "AWAITING_CHECKS"}, Position: &position{0, 2}},
			prev: told{QueueEvents: map[string]string{"a1": "added"}, Queue: "0/2/QUEUED"}, wantArms: []string{"merge_queue_entry_state_changed"}, wantPlace: "0/2/AWAITING_CHECKS"},
		{name: "a removal is told with its reason", q: prQueueSnapshot{Events: []remoteQueueEvent{added, removed}},
			prev: told{QueueEvents: map[string]string{"a1": "added"}, Queue: "0/2/QUEUED"}, wantArms: []string{"merge_queue_removed"}},
		{name: "a removal read with the merge is the merge's", q: prQueueSnapshot{Events: []remoteQueueEvent{added, removed}}, merged: true,
			prev: told{QueueEvents: map[string]string{"a1": "added"}, Queue: "0/1/LOCKED"}},
		{name: "a joining and its removal in one gap are both told", q: prQueueSnapshot{Events: []remoteQueueEvent{added, removed}},
			prev: told{}, wantArms: []string{"merge_queue_entered", "merge_queue_removed"}},
		{name: "a merge group's finished check is told", q: prQueueSnapshot{Events: []remoteQueueEvent{added}, Entry: &remoteEntryFacts{State: "AWAITING_CHECKS", GroupHead: "g1"}, Position: &position{0, 1},
			GroupChecks: []remoteCheck{{ID: 5, Name: "ci", Completed: true, Conclusion: "failure"}}},
			prev: told{QueueEvents: map[string]string{"a1": "added"}, Queue: "0/1/AWAITING_CHECKS"}, wantArms: []string{"merge_group_checks_concluded"}, wantPlace: "0/1/AWAITING_CHECKS"},
		{name: "a merge group's failing gate check is not counted", q: prQueueSnapshot{Events: []remoteQueueEvent{added}, Entry: &remoteEntryFacts{State: "AWAITING_CHECKS", GroupHead: "g1"}, Position: &position{0, 1},
			GroupChecks: []remoteCheck{{ID: 5, Name: "gate", Completed: true, Conclusion: "failure"}, {ID: 6, Name: "ci"}}, Gates: map[string]bool{"gate": true}},
			prev: told{QueueEvents: map[string]string{"a1": "added"}, Queue: "0/1/AWAITING_CHECKS"}, wantPlace: "0/1/AWAITING_CHECKS"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			_, place, _, _, timed, after := diffQueue(tt.q, tt.merged, tt.prev)

			// Assert
			if got := armsOf(timed, after); !reflect.DeepEqual(got, tt.wantArms) || place != tt.wantPlace {
				t.Fatalf("diffQueue = %v at %q; want %v at %q", got, place, tt.wantArms, tt.wantPlace)
			}
		})
	}
}

func TestAJoiningCarriesWhereThePullRequestStands(t *testing.T) {
	// Arrange
	q := prQueueSnapshot{Events: []remoteQueueEvent{{ID: "a1", Actor: "dodge", At: t0}}, Entry: &remoteEntryFacts{State: "QUEUED"}, Position: &position{1, 2}}

	// Act
	_, _, _, _, timed, _ := diffQueue(q, false, told{})

	// Assert
	want := &agentreplv1.PullRequestActivityMergeQueueEntered{Actor: &agentreplv1.PullRequestActivityAuthor{Login: "dodge"},
		Position: &agentreplv1.PullRequestMergeQueuePosition{EntriesAhead: 1, EntriesTotal: 2}, EnteredAt: timed[0].activity.GetMergeQueueEntered().GetEnteredAt()}
	if got := timed[0].activity.GetMergeQueueEntered(); !proto.Equal(got, want) || !got.GetEnteredAt().AsTime().Equal(t0) {
		t.Fatalf("entered = %v, want %v", got, want)
	}
}

func TestAPlaceRoundTrips(t *testing.T) {
	// Act
	got, ok := parsePlace(placeOf(position{Ahead: 3, Total: 9}, "AWAITING_CHECKS"))
	_, bad := parsePlace("")

	// Assert
	if !ok || got != (place{pos: position{3, 9}, state: "AWAITING_CHECKS"}) || bad {
		t.Fatalf("parsePlace = %+v, %v; empty %v", got, ok, bad)
	}
}

func TestEntryStateProtoNamesEachStateGitHubDoes(t *testing.T) {
	tests := []struct {
		state string
		want  string
	}{
		{"QUEUED", "queued"}, {"AWAITING_CHECKS", "awaiting_checks"}, {"MERGEABLE", "mergeable"}, {"UNMERGEABLE", "unmergeable"}, {"LOCKED", "locked"}, {"NEW_STATE", ""},
	}
	for _, tt := range tests {
		t.Run(tt.state, func(t *testing.T) {
			// Act
			got := entryStateProto(tt.state)

			// Assert
			arm := ""
			if f := got.ProtoReflect().WhichOneof(got.ProtoReflect().Descriptor().Oneofs().ByName("state")); f != nil {
				arm = string(f.Name())
			}
			if arm != tt.want {
				t.Fatalf("entryStateProto(%s) = %q, want %q", tt.state, arm, tt.want)
			}
		})
	}
}
