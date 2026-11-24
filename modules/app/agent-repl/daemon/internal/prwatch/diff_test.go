package prwatch

import (
	"reflect"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/wsm"
)

// kinds names each activity's arm, in order.
func kinds(activity []*agentreplv1.PullRequestActivity) []string {
	var out []string
	for _, a := range activity {
		out = append(out, reflect.TypeOf(a.GetActivity()).Elem().Name())
	}
	return out
}

func openPR(head string) remotePR { return remotePR{Title: "t", URL: "u", Open: true, Head: head} }

func comment(id uint64, created, updated time.Time) remoteComment {
	return remoteComment{ID: id, Author: author{Login: "a", Association: "MEMBER"}, Body: "b", Created: created, Updated: updated}
}

func TestDiffTellsWhatChangedSinceTheLastTelling(t *testing.T) {
	later := t0.Add(time.Minute)
	base := snapshot{PR: openPR("h"), Comments: []remoteComment{comment(1, t0, t0)},
		ReviewComments: []remoteReviewComment{{remoteComment: comment(5, t0, t0), Path: "f.go"}},
		Reviews:        []remoteReview{{ID: 9, State: "COMMENTED", Body: "x", Submitted: t0}}}
	tests := []struct {
		name string
		then func(s snapshot) snapshot
		want []string
	}{
		{name: "nothing changed", then: func(s snapshot) snapshot { return s }},
		{name: "a new comment", then: func(s snapshot) snapshot {
			s.Comments = append(s.Comments, comment(2, later, later))
			return s
		}, want: []string{"PullRequestActivity_Comment"}},
		{name: "an edited comment", then: func(s snapshot) snapshot {
			s.Comments = []remoteComment{comment(1, t0, later)}
			return s
		}, want: []string{"PullRequestActivity_Edited"}},
		{name: "a deleted line comment", then: func(s snapshot) snapshot {
			s.ReviewComments = nil
			return s
		}, want: []string{"PullRequestActivity_Deleted"}},
		{name: "a new review, and a pending one ignored", then: func(s snapshot) snapshot {
			s.Reviews = append(s.Reviews, remoteReview{ID: 10, State: "APPROVED", Submitted: later}, remoteReview{ID: 11, State: "PENDING"})
			return s
		}, want: []string{"PullRequestActivity_Review"}},
		{name: "a dismissed review is told as it now stands", then: func(s snapshot) snapshot {
			s.Reviews = []remoteReview{{ID: 9, State: "DISMISSED", Body: "x", Submitted: t0}}
			return s
		}, want: []string{"PullRequestActivity_Review"}},
		{name: "a review whose body was edited", then: func(s snapshot) snapshot {
			s.Reviews = []remoteReview{{ID: 9, State: "COMMENTED", Body: "y", Submitted: t0}}
			return s
		}, want: []string{"PullRequestActivity_Edited"}},
		{name: "ready for review", then: func(s snapshot) snapshot {
			s.PR.Draft = false
			return s
		}, want: nil},
		{name: "a merge", then: func(s snapshot) snapshot {
			s.PR.Open, s.PR.Merged = false, true
			return s
		}, want: []string{"PullRequestActivity_StateChanged"}},
		{name: "timed items in GitHub's order, deletions after", then: func(s snapshot) snapshot {
			s.Comments = []remoteComment{comment(3, later.Add(time.Minute), later.Add(time.Minute)), comment(2, later, later)}
			return s
		}, want: []string{"PullRequestActivity_Comment", "PullRequestActivity_Comment", "PullRequestActivity_Deleted"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			prev := anchor(base)

			// Act
			_, activity := diff(tt.then(base), prev, true)

			// Assert
			if got := kinds(activity); !reflect.DeepEqual(got, tt.want) {
				t.Fatalf("activity = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestDiffOrdersCommentsByWhenTheyWerePosted(t *testing.T) {
	// Arrange
	snap := snapshot{PR: openPR("h"), Comments: []remoteComment{comment(3, t0.Add(2*time.Minute), t0), comment(2, t0.Add(time.Minute), t0)}}

	// Act
	_, activity := diff(snap, anchor(snapshot{PR: openPR("h")}), true)

	// Assert
	if len(activity) != 2 || activity[0].GetComment().GetCommentId() != 2 || activity[1].GetComment().GetCommentId() != 3 {
		t.Fatalf("activity = %v, want comment 2 then 3", activity)
	}
}

func TestDiffOfANeverAnchoredSubscriptionTellsItemsButNotAnOpenState(t *testing.T) {
	// Arrange
	prev, anchored, _ := decodeTold("")
	snap := snapshot{PR: openPR("h"), Comments: []remoteComment{comment(1, t0, t0)}}

	// Act
	_, activity := diff(snap, prev, anchored)

	// Assert
	if got := kinds(activity); !reflect.DeepEqual(got, []string{"PullRequestActivity_Comment"}) {
		t.Fatalf("activity = %v, want the existing comment told once and no state change", got)
	}
}

func TestDiffTellsTheHeadsWholeRunOnce(t *testing.T) {
	tests := []struct {
		name   string
		prev   []remoteCheck
		head   string
		checks []remoteCheck
		gates  map[string]bool
		want   []string
	}{
		{name: "a run that passed is told", checks: []remoteCheck{done(1, "test", "success")}, head: "h", want: []string{"h:passed"}},
		{name: "a run still going is not told", checks: []remoteCheck{done(1, "test", "success"), {ID: 2, Name: "lint"}}, head: "h"},
		{name: "a failure is told while the rest still run", checks: []remoteCheck{done(1, "test", "failure"), {ID: 2, Name: "lint"}}, head: "h", want: []string{"h:failed"}},
		{name: "a passed run is not told again", prev: []remoteCheck{done(1, "test", "success")}, head: "h", checks: []remoteCheck{done(1, "test", "success")}},
		{name: "a further failure is not told again", prev: []remoteCheck{done(1, "a", "failure"), {ID: 2, Name: "b"}}, head: "h",
			checks: []remoteCheck{done(1, "a", "failure"), done(2, "b", "failure")}},
		{name: "a re-run that passes is told", prev: []remoteCheck{done(1, "test", "failure")}, head: "h",
			checks: []remoteCheck{done(1, "test", "failure"), done(2, "test", "success")}, want: []string{"h:passed"}},
		{name: "a new head tells its run afresh", prev: []remoteCheck{done(1, "test", "success")}, head: "h2", checks: []remoteCheck{done(1, "test", "success")}, want: []string{"h2:passed"}},
		{name: "a failing gate check leaves the run passing", head: "h", checks: []remoteCheck{done(1, "test", "success"), done(2, "review-or-self-certified", "failure")},
			gates: map[string]bool{"review-or-self-certified": true}, want: []string{"h:passed"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			prev := anchor(snapshot{PR: openPR("h"), Checks: tt.prev, ChecksRead: true})

			// Act
			_, activity := diff(snapshot{PR: openPR(tt.head), Checks: tt.checks, ChecksRead: true, Gates: tt.gates}, prev, true)

			// Assert
			var got []string
			for _, a := range activity {
				c := a.GetChecksConcluded()
				outcome := outcomeFailed
				if c.GetPassed() != nil {
					outcome = outcomePassed
				}
				got = append(got, c.GetCommitSha()+":"+outcome)
			}
			if !reflect.DeepEqual(got, tt.want) {
				t.Fatalf("outcomes told = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestDiffKeepsTheChecksItDidNotRead(t *testing.T) {
	// Arrange
	prev := anchor(snapshot{PR: openPR("h"), Checks: []remoteCheck{done(1, "test", "success")}, ChecksRead: true})

	// Act
	next, activity := diff(snapshot{PR: openPR("h2")}, prev, true)

	// Assert
	if len(activity) != 0 || next.Head != "h" || next.ChecksOutcome != outcomePassed {
		t.Fatalf("next = %+v, activity %v; want the told outcome kept as it was", next, activity)
	}
}

func TestToldEncodesOneStateOneWay(t *testing.T) {
	// Arrange
	snap := snapshot{PR: openPR("h"), Comments: []remoteComment{comment(2, t0, t0), comment(1, t0, t0)}}

	// Act
	a, b := encodeTold(anchor(snap)), encodeTold(anchor(snapshot{PR: snap.PR, Comments: []remoteComment{snap.Comments[1], snap.Comments[0]}}))

	// Assert
	if a != b {
		t.Fatalf("encodings differ: %s vs %s", a, b)
	}
	if _, anchored, err := decodeTold(a); err != nil || !anchored {
		t.Fatalf("decodeTold = %v, %v; want an anchored snapshot", anchored, err)
	}
}

func TestDecodeToldRefusesAnUnreadableSnapshot(t *testing.T) {
	// Act
	_, _, err := decodeTold("{not json")

	// Assert
	if err == nil {
		t.Fatal("decodeTold = nil, want refused")
	}
}

func TestEventsOfAnActivityAnswersWhatTellsIt(t *testing.T) {
	stateChanged := func(state *agentreplv1.PullRequestState) *agentreplv1.PullRequestActivity {
		return &agentreplv1.PullRequestActivity{Activity: &agentreplv1.PullRequestActivity_StateChanged{StateChanged: &agentreplv1.PullRequestActivityStateChanged{State: state}}}
	}
	tests := []struct {
		name     string
		activity *agentreplv1.PullRequestActivity
		want     wsm.PullRequestEvents
	}{
		{name: "a merge is told to state and to merged", activity: stateChanged(&agentreplv1.PullRequestState{State: &agentreplv1.PullRequestState_Merged{Merged: &agentreplv1.PullRequestStateMerged{}}}),
			want: wsm.NewPullRequestEvents(wsm.PullRequestEventState, wsm.PullRequestEventMerged)},
		{name: "a close is told to state alone", activity: stateChanged(&agentreplv1.PullRequestState{State: &agentreplv1.PullRequestState_Closed{Closed: &agentreplv1.PullRequestStateClosed{}}}),
			want: wsm.NewPullRequestEvents(wsm.PullRequestEventState)},
		{name: "a comment is told to comment alone", activity: &agentreplv1.PullRequestActivity{Activity: &agentreplv1.PullRequestActivity_Comment{Comment: &agentreplv1.PullRequestActivityComment{}}},
			want: wsm.NewPullRequestEvents(wsm.PullRequestEventComment)},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := eventsOf(tt.activity)

			// Assert
			if got != tt.want {
				t.Fatalf("eventsOf = %s, want %s", got, tt.want)
			}
		})
	}
}
