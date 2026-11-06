package prwatch

import (
	"context"
	"errors"
	"strings"
	"testing"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ghclient"
	"agent-repld/internal/wsm"
)

func TestANewCommentIsToldOnceUnderItsOrigin(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.subscribe(t, wsm.PullRequestEventComment)
	r.gh.set(pathComments(), mustJSON([]any{commentJSONOf(11, "alice", "please rename it", t0, t0, nil)}))

	// Act
	r.tick()
	r.tick()

	// Assert
	told := r.told()
	if len(told) != 1 || !contains(told[0], "@alice (a member of the organization) commented") || !contains(told[0], "~~~untrusted\nplease rename it\n~~~") {
		t.Fatalf("told = %q, want the one comment, quoted untrusted", told)
	}
	if r.prompts.got[0].origin != Origin || r.prompts.got[0].ws != r.ws {
		t.Fatalf("submission = %+v, want the pull-request notice origin to the subscriber", r.prompts.got[0])
	}
	if !contains(told[0], "It now stands open, its head at head1") {
		t.Fatalf("told = %q, want the status line", told[0])
	}
}

func TestWhatTheSubscriptionDoesNotAskForIsNotToldNorToldLater(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.subscribe(t, wsm.PullRequestEventReview)
	r.gh.set(pathComments(), mustJSON([]any{commentJSONOf(11, "alice", "hi", t0, t0, nil)}))

	// Act
	r.tick()
	if _, err := r.db.SubscribePullRequest(context.Background(), wsm.PullRequestSubscription{Subscriber: r.ws, PullRequest: prK,
		Events: wsm.AllPullRequestEvents(), Origin: wsm.PullRequestOriginRequested, SubscribedAt: t0}); err != nil {
		t.Fatal(err)
	}
	r.tick()

	// Assert
	if told := r.told(); len(told) != 0 {
		t.Fatalf("told = %q, want nothing: the comment came before the subscription asked for comments", told)
	}
}

func TestAMergeIsToldAndEndsTheSubscription(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.subscribe(t, wsm.PullRequestEventState)
	r.gh.set(pathPull(), pullJSON("closed", false, true, "head1"))

	// Act
	r.tick()
	r.tick()

	// Assert
	told := r.told()
	if len(told) != 1 || !contains(told[0], "The pull request was merged by @dodge, as commit mergesha0000") || !contains(told[0], endedSentence) {
		t.Fatalf("told = %q, want the merge and the subscription's end, once", told)
	}
	live, err := r.db.LivePullRequestSubscriptions(context.Background())
	if err != nil || len(live) != 0 {
		t.Fatalf("live = %v, %v; want the subscription ended", live, err)
	}
}

func TestAFailingReadIsToldOnceThenItsRecovery(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.subscribe(t, wsm.PullRequestEventComment)
	r.gh.set(pathPull(), &ghclient.Failure{Kind: ghclient.KindUnreachable, Message: "no route to host"})

	// Act
	r.tick()
	r.tick()
	r.gh.set(pathPull(), pullJSON("open", false, false, "head1"))
	r.gh.set(pathComments(), mustJSON([]any{commentJSONOf(12, "bob", "back", t0, t0, nil)}))
	r.tick()

	// Assert
	told := r.told()
	if len(told) != 2 || !contains(told[0], "reads of this pull request are failing: no route to host") {
		t.Fatalf("told = %q, want the failure once, then the recovery", told)
	}
	if !contains(told[1], "@bob") || contains(told[1], "work again") {
		t.Fatalf("second notice = %q, want the comment it missed and no word of recovery", told[1])
	}
	if _, ok := dlog.FindRecord(r.log.Records(), dlog.RecordQuery{Level: "info", Operation: opPoll, Message: "the pull request could not be read; its subscriptions are failing"}); !ok {
		t.Fatalf("no record of the failed read: %+v", r.log.Records())
	}
}

// recoverQuietly fails one read of the rig's pull request, then lets the
// next succeed with nothing new on it.
func recoverQuietly(r *rig) {
	r.gh.set(pathPull(), &ghclient.Failure{Kind: ghclient.KindUnreachable, Message: "no route to host"})
	r.tick()
	r.gh.set(pathPull(), pullJSON("open", false, false, "head1"))
	r.tick()
}

func TestARecoveryWithNoActivityIsNotToldAsANotice(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.subscribe(t, wsm.PullRequestEventComment)

	// Act
	recoverQuietly(r)
	r.tick()

	// Assert
	told := r.told()
	if len(told) != 1 || !contains(told[0], "reads of this pull request are failing") {
		t.Fatalf("told = %q, want only the failure: a recovery with no activity is no notice", told)
	}
	if _, ok := dlog.FindRecord(r.log.Records(), dlog.RecordQuery{Level: "info", Operation: opPoll,
		Message: "the pull request's reads work again and it did nothing meanwhile; its subscriber is told nothing"}); !ok {
		t.Fatalf("no record of the quiet recovery: %+v", r.log.Records())
	}
}

func TestARecoveryWithNoActivityClearsTheSubscriptionsHealth(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.subscribe(t, wsm.PullRequestEventComment)

	// Act
	recoverQuietly(r)

	// Assert
	live, err := r.db.LivePullRequestSubscriptions(context.Background())
	if err != nil || len(live) != 1 || live[0].Failing != nil || live[0].LastRead.IsZero() {
		t.Fatalf("live = %+v, %v; want the one subscription healthy and read", live, err)
	}
}

func TestASpentRateLimitIsWaitedOut(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.subscribe(t, wsm.PullRequestEventComment)
	r.gh.set(pathPull(), &ghclient.Failure{Kind: ghclient.KindRateLimited, ResetsAt: t0.Add(time.Hour)})
	r.tick()
	reads := len(r.gh.calls)

	// Act
	r.clock.Set(t0.Add(time.Minute))
	r.tick()

	// Assert
	if len(r.gh.calls) != reads {
		t.Fatalf("calls %v, want no read before the limit resets", r.gh.calls[reads:])
	}
}

func TestABusySubscribersNoticeIsSupersededAndSaidAgain(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.holds.retired = true
	r.subscribe(t, wsm.PullRequestEventComment)
	r.gh.set(pathComments(), mustJSON([]any{commentJSONOf(11, "alice", "first", t0, t0, nil)}))
	r.tick()

	// Act
	r.gh.set(pathComments(), mustJSON([]any{commentJSONOf(11, "alice", "first", t0, t0, nil), commentJSONOf(12, "bob", "second", t0.Add(time.Minute), t0.Add(time.Minute), nil)}))
	r.tick()

	// Assert
	told := r.told()
	if len(told) != 2 || !contains(told[1], "first") || !contains(told[1], "second") || strings.Index(told[1], "first") > strings.Index(told[1], "second") {
		t.Fatalf("told = %q, want the second notice saying both, oldest first", told)
	}
}

func TestAFailedSubmissionStaysOwed(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.subscribe(t, wsm.PullRequestEventComment)
	r.gh.set(pathComments(), mustJSON([]any{commentJSONOf(11, "alice", "hi", t0, t0, nil)}))
	r.prompts.fail = errors.New("the queue is gone")

	// Act
	r.tick()
	r.prompts.fail = nil
	r.tick()

	// Assert
	if told := r.told(); len(told) != 1 || !contains(told[0], "@alice") {
		t.Fatalf("told = %q, want the owed comment told on the next tick", told)
	}
	if _, ok := dlog.FindRecord(r.log.Records(), dlog.RecordQuery{Level: "error", Operation: opNotice, Message: "could not submit a pull-request notice to its subscriber; it is told on a later tick"}); !ok {
		t.Fatalf("no error record of the failed submission: %+v", r.log.Records())
	}
}

func TestADaemonThatDoesNotServeNeitherReadsNorTells(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.subscribe(t, wsm.PullRequestEventComment)
	r.serves = false
	reads := len(r.gh.calls)

	// Act
	r.tick()

	// Assert
	if len(r.gh.calls) != reads || len(r.told()) != 0 {
		t.Fatalf("calls %v told %q, want nothing from a daemon that does not serve", r.gh.calls[reads:], r.told())
	}
}

func TestAnUnchangedPullRequestCostsOnlyConditionalReads(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.subscribe(t, wsm.PullRequestEventComment)

	// Act
	r.tick()

	// Assert
	want := []string{"GET " + pathPull(), "GET " + pathComments(), "GET " + pathLines(), "GET " + pathReviews()}
	got := r.gh.calls[len(r.gh.calls)-4:]
	for i := range want {
		if got[i] != want[i] {
			t.Fatalf("calls = %v, want %v", got, want)
		}
	}
	if len(r.told()) != 0 {
		t.Fatalf("told = %q, want nothing", r.told())
	}
}

func TestNewRefusesAMissingCollaborator(t *testing.T) {
	// Act
	_, err := New(Deps{})

	// Assert
	if err == nil {
		t.Fatal("New(Deps{}) = nil, want refused")
	}
}

func TestAPullRequestNoticeIsDeliveredUnderItsOwnBriefAndOrigin(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.subscribe(t, wsm.PullRequestEventComment)
	owed := wsm.PullRequestOwed{Subscriber: r.ws, PullRequest: prK, Pending: []string{"one."}, Status: "open"}
	if err := r.db.OwePullRequestNotice(context.Background(), r.ws, prK, []string{"one."}); err != nil {
		t.Fatal(err)
	}

	// Act
	n := pullRequestNotice(r.db, owed)
	errRecord := n.record(context.Background(), "turn-1", []string{"one."}, 1)

	// Assert
	if n.label != "pull request `o/r#7`" || n.brief != BriefNotice || n.origin != Origin || n.noun != "pull-request" || n.subject != "pull request" ||
		n.fields["pull_request"] != "o/r#7" || n.fields["subscriber"] != string(r.ws) {
		t.Fatalf("notice = %+v", n)
	}
	left, err := r.db.OwedPullRequestNotices(context.Background())
	if errRecord != nil || err != nil || len(left) != 0 {
		t.Fatalf("record = %v, owed after = %+v, %v; want it recorded in the store", errRecord, left, err)
	}
}

// queuedPR is the rig's pull request queued on main at its place among
// ENTRIES, with its timeline EVENTS.
func queuedPR(g *gql, state, group string, entries []remoteQueueEntry, events ...remoteQueueEvent) {
	g.setFacts(prK.Number, factsJSON("main", &remoteEntryFacts{State: state, GroupHead: group}, events...))
	g.setQueue("main", queueJSON(entries...))
}

func TestAPullRequestIsFollowedThroughItsMergeQueue(t *testing.T) {
	joined := remoteQueueEvent{ID: "a1", Actor: "dodge", At: t0}
	tests := []struct {
		name    string
		arrange func(g *gql, r *rig)
		want    []string
	}{
		{name: "joining the queue", arrange: func(g *gql, r *rig) {
			queuedPR(g, "QUEUED", "", []remoteQueueEntry{entry(3, "QUEUED"), entry(prK.Number, "QUEUED")}, joined)
		}, want: []string{"The pull request joined the merge queue by @dodge: it is 2 of 2 in the merge queue, 1 entry ahead of it.",
			"It now stands open, 2 of 2 in the merge queue, 1 entry ahead of it, waiting its turn"}},
		{name: "moving up to the head", arrange: func(g *gql, r *rig) {
			queuedPR(g, "QUEUED", "", []remoteQueueEntry{entry(3, "QUEUED"), entry(prK.Number, "QUEUED")}, joined)
			r.tick()
			queuedPR(g, "AWAITING_CHECKS", "", []remoteQueueEntry{entry(prK.Number, "AWAITING_CHECKS")}, joined)
		}, want: []string{"The pull request moved up the merge queue: it was 2 of 2 in the merge queue, 1 entry ahead of it, and is now at the head of the merge queue (of 1), merging next.",
			"The pull request's merge queue entry is now running its merge group's checks."}},
		{name: "a merge group check failing and the removal", arrange: func(g *gql, r *rig) {
			queuedPR(g, "AWAITING_CHECKS", "group1", []remoteQueueEntry{entry(prK.Number, "AWAITING_CHECKS")}, joined)
			r.tick()
			r.gh.set(pathChecks("group1"), mustJSON(map[string]any{"check_runs": []map[string]any{{"id": 9, "name": "ci", "status": "completed", "conclusion": "failure"}}}))
			g.setFacts(prK.Number, factsJSON("main", nil, joined, remoteQueueEvent{ID: "r1", Removed: true, Reason: ptr("the merge group's checks failed"), At: t0.Add(1)}))
		}, want: []string{"The pull request was removed from the merge queue: the merge group's checks failed."}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			r := newRig(t)
			g := r.withGQL()
			g.setFacts(prK.Number, factsJSON("main", nil))
			r.subscribe(t, wsm.PullRequestEventMergeQueue)
			tt.arrange(g, r)

			// Act
			r.tick()

			// Assert
			told := r.told()
			if len(told) == 0 {
				t.Fatal("nothing was told")
			}
			last := told[len(told)-1]
			for _, want := range tt.want {
				if !strings.Contains(last, want) {
					t.Fatalf("notice %q lacks %q", last, want)
				}
			}
		})
	}
}

func TestAMergeGroupsFinishedCheckIsToldOnce(t *testing.T) {
	// Arrange
	r := newRig(t)
	g := r.withGQL()
	joined := remoteQueueEvent{ID: "a1", At: t0}
	queuedPR(g, "AWAITING_CHECKS", "group1", []remoteQueueEntry{entry(prK.Number, "AWAITING_CHECKS")}, joined)
	r.subscribe(t, wsm.PullRequestEventMergeQueue)
	r.gh.set(pathChecks("group1"), mustJSON(map[string]any{"check_runs": []map[string]any{{"id": 9, "name": "ci", "status": "completed", "conclusion": "failure"}}}))

	// Act
	r.tick()
	r.tick()

	// Assert
	told := r.told()
	if len(told) != 1 || !strings.Contains(told[0], "Merge group check `ci`: failed.") {
		t.Fatalf("told = %v, want the merge group's check once", told)
	}
}

func TestAPullRequestNotAskingForTheQueueNeverReadsIt(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.withGQL()
	r.subscribe(t, wsm.PullRequestEventComment)

	// Act
	r.tick()

	// Assert
	if n := r.gqlCalls("PullRequestQueues") + r.gqlCalls("MergeQueue"); n != 0 {
		t.Fatalf("%d GraphQL calls, want none", n)
	}
}

func TestAPullRequestWhoseQueueCannotBeReadIsFailing(t *testing.T) {
	// Arrange
	r := newRig(t)
	g := r.withGQL()
	g.setFacts(prK.Number, factsJSON("main", nil))
	r.subscribe(t, wsm.PullRequestEventMergeQueue)
	g.fail["PullRequestQueues"] = &ghclient.Failure{Kind: ghclient.KindUnreachable, Method: "POST", Path: "graphql", Message: "down"}

	// Act
	r.tick()

	// Assert
	subs, _ := r.db.WorkspacePullRequestSubscriptions(context.Background(), r.ws)
	if len(subs) != 1 || subs[0].Failing == nil || subs[0].Failing.Detail != "down" {
		t.Fatalf("subscriptions = %+v, want one failing", subs)
	}
	if told := r.told(); len(told) != 1 || !strings.Contains(told[0], "reads of this pull request are failing: down") {
		t.Fatalf("told = %v, want the failure", told)
	}
}

func TestOneTickReadsAQueueOnceForEveryoneFollowingIt(t *testing.T) {
	// Arrange
	r := newRig(t)
	g := r.withGQL()
	queuedPR(g, "QUEUED", "", []remoteQueueEntry{entry(prK.Number, "QUEUED")}, remoteQueueEvent{ID: "a1", At: t0})
	r.subscribe(t, wsm.PullRequestEventMergeQueue)
	r.subscribeQueue(t)
	before := r.gqlCalls("MergeQueue")

	// Act
	r.tick()

	// Assert
	if got := r.gqlCalls("MergeQueue") - before; got != 1 {
		t.Fatalf("the queue was read %d times in one tick, want once", got)
	}
}
