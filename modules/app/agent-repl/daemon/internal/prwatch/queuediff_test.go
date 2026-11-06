package prwatch

import (
	"reflect"
	"testing"

	"google.golang.org/protobuf/proto"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
)

func TestDiffChecksTellsEachFinishedRunOnce(t *testing.T) {
	tests := []struct {
		name       string
		prevHead   string
		prevChecks map[string]string
		runs       []remoteCheck
		wantChecks map[string]string
		wantTold   []string
	}{
		{name: "a finished run is told", prevHead: "h", prevChecks: map[string]string{}, runs: []remoteCheck{{ID: 1, Name: "test", Completed: true, Conclusion: "success"}},
			wantChecks: map[string]string{"test": "1:success"}, wantTold: []string{"test"}},
		{name: "a run already told is not told again", prevHead: "h", prevChecks: map[string]string{"test": "1:success"}, runs: []remoteCheck{{ID: 1, Name: "test", Completed: true, Conclusion: "success"}},
			wantChecks: map[string]string{"test": "1:success"}},
		{name: "an unfinished run waits", prevHead: "h", prevChecks: map[string]string{}, runs: []remoteCheck{{ID: 1, Name: "test"}}, wantChecks: map[string]string{}},
		{name: "the latest run of a name counts", prevHead: "h", prevChecks: map[string]string{}, runs: []remoteCheck{{ID: 2, Name: "test", Completed: true, Conclusion: "failure"}, {ID: 1, Name: "test", Completed: true, Conclusion: "success"}},
			wantChecks: map[string]string{"test": "2:failure"}, wantTold: []string{"test"}},
		{name: "a new head starts afresh", prevHead: "old", prevChecks: map[string]string{"test": "1:success"}, runs: []remoteCheck{{ID: 1, Name: "test", Completed: true, Conclusion: "success"}},
			wantChecks: map[string]string{"test": "1:success"}, wantTold: []string{"test"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			head, checks, told := diffChecks(tt.prevHead, tt.prevChecks, "h", tt.runs)

			// Assert
			var names []string
			for _, c := range told {
				names = append(names, c.GetName())
			}
			if head != "h" || !reflect.DeepEqual(checks, tt.wantChecks) || !reflect.DeepEqual(names, tt.wantTold) {
				t.Fatalf("diffChecks = %s, %v, %v; want %v, %v", head, checks, names, tt.wantChecks, tt.wantTold)
			}
		})
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
			prev: told{QueueEvents: map[string]string{"a1": "added"}, Queue: "0/1/AWAITING_CHECKS"}, wantArms: []string{"merge_group_check_finished"}, wantPlace: "0/1/AWAITING_CHECKS"},
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
