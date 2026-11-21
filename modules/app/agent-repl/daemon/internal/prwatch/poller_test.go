package prwatch

import (
	"context"
	"errors"
	"fmt"
	"os"
	"reflect"
	"strings"
	"testing"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ghclient"
	"agent-repld/internal/ids"
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

func TestAFailingReadIsToldToNobodyAndWhatItMissedIsTold(t *testing.T) {
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
	if len(told) != 1 || !contains(told[0], "@bob") || contains(told[0], "failing") || contains(told[0], "work again") {
		t.Fatalf("told = %q, want only the comment it missed: no word of the failure or the recovery", told)
	}
	if _, ok := dlog.FindRecord(r.log.Records(), dlog.RecordQuery{Level: "info", Operation: opPoll, Message: "the pull request could not be read; its subscriptions are failing, and no subscriber is told"}); !ok {
		t.Fatalf("no record of the failed read: %+v", r.log.Records())
	}
}

func TestRejectedCredentialsAreToldOncePerFailure(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.subscribe(t, wsm.PullRequestEventComment)
	r.gh.set(pathPull(), &ghclient.Failure{Kind: ghclient.KindUnauthorized, Message: "Bad credentials", Status: 401})

	// Act: two failing polls of one standing failure.
	r.tick()
	r.tick()

	// Assert
	told := r.told()
	if len(told) != 1 || !contains(told[0], "GitHub rejected agent-repl's credentials") {
		t.Fatalf("told = %q, want one notice of the rejected credentials", told)
	}
	if _, ok := dlog.FindRecord(r.log.Records(), dlog.RecordQuery{Level: "info", Operation: opPoll,
		Message: "GitHub rejected the credentials; each subscription whose failure began is owed a notice to surface it"}); !ok {
		t.Fatalf("no record of the rejected credentials: %+v", r.log.Records())
	}
}

func TestRecordReadFailureRecordsANoticeItCannotOwe(t *testing.T) {
	// Arrange
	r := newRig(t)
	failure := &ghclient.Failure{Kind: ghclient.KindUnauthorized, Message: "Bad credentials", Status: 401}

	// Act
	r.w.recordReadFailure(context.Background(), opPoll, "pull request", failure, dlog.Context{"pull_request": "o/r#1"},
		func(wsm.PullRequestFailure) ([]ids.WorkspaceID, error) { return []ids.WorkspaceID{r.ws}, nil },
		func(ids.WorkspaceID, []string) error { return errors.New("store down") })

	// Assert
	record, ok := dlog.FindRecord(r.log.Records(), dlog.RecordQuery{Level: "error", Operation: opPoll,
		Message: "could not owe a subscriber the notice that GitHub rejected the credentials"})
	if !ok || record.Context["subscriber"] != string(r.ws) || record.Context["pull_request"] != "o/r#1" {
		t.Fatalf("no error record naming the subscriber: %+v", r.log.Records())
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
	if told := r.told(); len(told) != 0 {
		t.Fatalf("told = %q, want nothing: the failed read is told to no workspace, and a recovery with no activity is no notice", told)
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
			r.gh.set(pathChecks("group1"), checkRuns(done(9, "ci", "failure")))
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

func TestAMergeGroupsFailedRunIsToldOnce(t *testing.T) {
	// Arrange
	r := newRig(t)
	g := r.withGQL()
	joined := remoteQueueEvent{ID: "a1", At: t0}
	queuedPR(g, "AWAITING_CHECKS", "group1", []remoteQueueEntry{entry(prK.Number, "AWAITING_CHECKS")}, joined)
	r.subscribe(t, wsm.PullRequestEventMergeQueue)
	r.gh.set(pathChecks("group1"), checkRuns(done(9, "ci", "failure")))

	// Act
	r.tick()
	r.tick()

	// Assert
	told := r.told()
	if len(told) != 1 || !strings.Contains(told[0], "CI on the merge group failed: `ci` failed.") {
		t.Fatalf("told = %v, want the merge group's run once", told)
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
	if told := r.told(); len(told) != 0 {
		t.Fatalf("told = %v, want nothing: a read the daemon could not make is told to no workspace", told)
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

func TestRecordReadFailureRecordsWhatItCouldNotDo(t *testing.T) {
	boom := errors.New("store down")
	tests := []struct {
		name      string
		recordErr error
		wantLog   string
	}{
		{name: "a failure it could not record is recorded at error", recordErr: boom, wantLog: "could not record the merge queue's failed read"},
		{name: "a failure it recorded is told to no subscriber", wantLog: "the merge queue could not be read; its subscriptions are failing, and no subscriber is told"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			r := newRig(t)

			// Act
			r.w.recordReadFailure(context.Background(), opQueuePoll, "merge queue", errors.New("dns"), dlog.Context{"merge_queue": "o/r@main"},
				func(wsm.PullRequestFailure) ([]ids.WorkspaceID, error) { return []ids.WorkspaceID{r.ws}, tt.recordErr },
				func(ids.WorkspaceID, []string) error { t.Fatal("a read failure owed a notice"); return nil })

			// Assert
			if told := r.told(); len(told) != 0 {
				t.Fatalf("told = %v, want nothing", told)
			}
			record, ok := dlog.FindRecord(r.log.Records(), dlog.RecordQuery{Operation: opQueuePoll, Message: tt.wantLog})
			if !ok || record.Context["merge_queue"] != "o/r@main" {
				t.Fatalf("no record %q naming the queue: %+v", tt.wantLog, r.log.Records())
			}
		})
	}
}

func TestAPullRequestsEndTellsItsWorkspace(t *testing.T) {
	tests := []struct {
		name   string
		origin wsm.PullRequestOrigin
		merged bool
	}{
		{name: "an owned link's merge is told as a merge", origin: wsm.PullRequestOriginOwnedLink, merged: true},
		{name: "a pull request closed unmerged is told as a close", origin: wsm.PullRequestOriginOwnedLink, merged: false},
		{name: "a requested subscription's merge is told too, for the verb to judge ownership", origin: wsm.PullRequestOriginRequested, merged: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			r := newRig(t)
			r.subscribeAs(t, tt.origin, wsm.PullRequestEventState)
			r.gh.set(pathPull(), pullJSON("closed", false, tt.merged, "head1"))

			// Act
			r.tick()

			// Assert
			want := []endTold{{ws: r.ws, merged: tt.merged}}
			if !reflect.DeepEqual(r.ended, want) {
				t.Fatalf("told %+v, want %+v", r.ended, want)
			}
		})
	}
}

func TestAnOpenPullRequestTellsNoEnd(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.subscribeAs(t, wsm.PullRequestOriginOwnedLink, wsm.PullRequestEventState)

	// Act
	r.tick()

	// Assert
	if len(r.ended) != 0 {
		t.Fatalf("told %+v, want no end for a pull request still open", r.ended)
	}
}

func TestAnEndThatCannotBeActedOnIsRecorded(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.endedErr = errors.New("verbs not bound")
	r.subscribeAs(t, wsm.PullRequestOriginOwnedLink, wsm.PullRequestEventState)
	r.gh.set(pathPull(), pullJSON("closed", false, true, "head1"))

	// Act
	r.tick()

	// Assert
	if _, ok := dlog.FindRecord(r.log.Records(), dlog.RecordQuery{Level: "error", Operation: opPoll, Message: "could not act on the end of the workspace's pull request"}); !ok {
		t.Fatalf("records = %+v, want the failure at error", r.log.Records())
	}
}

// checkRuns is a commit's check-runs page holding RUNS.
func checkRuns(runs ...remoteCheck) string {
	var out []map[string]any
	for _, c := range runs {
		run := map[string]any{"id": c.ID, "name": c.Name, "status": "in_progress"}
		if c.Completed {
			run["status"], run["conclusion"] = "completed", c.Conclusion
		}
		out = append(out, run)
	}
	return mustJSON(map[string]any{"check_runs": out})
}

func TestTheHeadsWholeRunIsToldInOneSentence(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.subscribe(t, wsm.PullRequestEventChecks)
	var runs []remoteCheck
	for i := uint64(1); i <= 50; i++ {
		runs = append(runs, done(i, fmt.Sprintf("check-%02d", i), "success"))
	}
	r.gh.set(pathChecks("head1"), checkRuns(runs...))

	// Act
	r.tick()
	r.tick()

	// Assert
	told := r.told()
	if len(told) != 1 || !contains(told[0], "|CI on head head1 passed: all 50 checks succeeded.|") || contains(told[0], "check-01") {
		t.Fatalf("told = %q, want one sentence for the whole run", told)
	}
}

func TestAFailingGateCheckTheConfigNamesLeavesTheRunPassing(t *testing.T) {
	// Arrange
	r := newRig(t)
	g := r.withGQL()
	g.setFacts(prK.Number, factsJSON("main", nil))
	g.setConfig("head1", "pre_deploy: {}\npull_requests:\n  gate_checks: [review-or-self-certified]\n")
	r.subscribe(t, wsm.PullRequestEventChecks)
	r.gh.set(pathChecks("head1"), checkRuns(done(1, "unit", "success"), done(2, "review-or-self-certified", "failure")))

	// Act
	r.tick()

	// Assert
	if told := r.told(); len(told) != 1 || !contains(told[0], "CI on head head1 passed: its one check succeeded.") {
		t.Fatalf("told = %q, want the run passed with the gate left out", told)
	}
}

func TestACommitsConfigIsReadOnce(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.subscribe(t, wsm.PullRequestEventChecks)

	// Act
	r.tick()
	r.tick()

	// Assert
	if n := r.gqlCalls("LifecycleConfigAt"); n != 1 {
		t.Fatalf("%d config reads, want one for the one head", n)
	}
}

func TestAnUnusableConfigFailsTheSubscription(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.subscribe(t, wsm.PullRequestEventChecks)
	r.gh.set(pathPull(), pullJSON("open", false, false, "head2"))
	g := r.withGQL()
	g.setFacts(prK.Number, factsJSON("main", nil))
	g.setConfig("head2", "pre_deploy: {}\npull_requests:\n  gate_check: [x]\n")
	r.gh.set(pathChecks("head2"), checkRuns(done(1, "unit", "failure")))

	// Act
	r.tick()

	// Assert
	subs, _ := r.db.WorkspacePullRequestSubscriptions(context.Background(), r.ws)
	if len(subs) != 1 || subs[0].Failing == nil || !contains(subs[0].Failing.Detail, "o/r@head2:.agent-repl/lifecycle.yaml") {
		t.Fatalf("subscriptions = %+v, want one failing, naming the file", subs)
	}
	if told := r.told(); len(told) != 0 {
		t.Fatalf("told = %v, want nothing while the config is unusable", told)
	}
	record, ok := dlog.FindRecord(r.log.Records(), dlog.RecordQuery{Level: "info", Operation: opPoll, Message: "the pull request could not be read; its subscriptions are failing, and no subscriber is told"})
	if !ok || !contains(fmt.Sprint(record.Context["cause"]), "gate_check") {
		t.Fatalf("records = %+v, want the failed read recorded with its cause", r.log.Records())
	}
}

// failingStates is the rig's store with its linked pull requests' list, or
// its state record, failing.
type failingStates struct {
	Store
	linkedErr, recordErr error
}

func (f failingStates) LinkedPullRequests(ctx context.Context) ([]wsm.PullRequestKey, error) {
	if f.linkedErr != nil {
		return nil, f.linkedErr
	}
	return f.Store.LinkedPullRequests(ctx)
}

func (f failingStates) RecordPullRequestState(ctx context.Context, pr wsm.PullRequestKey, state wsm.PullRequestState, at time.Time) ([]ids.WorkspaceID, error) {
	if f.recordErr != nil {
		return nil, f.recordErr
	}
	return f.Store.RecordPullRequestState(ctx, pr, state, at)
}

func TestAFollowedPullRequestsReadRecordsItsState(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.link(t)
	r.subscribe(t, wsm.PullRequestEventComment)
	r.gh.set(pathPull(), pullJSON("open", true, false, "head1"))

	// Act
	r.tick()

	// Assert: the subscription's own read records it, and nothing reads the
	// pull request a second time for its state.
	if got := r.linkState(t); got != wsm.PullRequestStateDraft {
		t.Fatalf("link state = %q, want draft", got)
	}
	reads := 0
	for _, call := range r.gh.calls {
		if call == "GET "+pathPull() {
			reads++
		}
	}
	if reads != 2 {
		t.Fatalf("pull request read %d times (anchor and tick), want 2: %v", reads, r.gh.calls)
	}
}

func TestALinkNoSubscriptionFollowsIsReadForItsStateAlone(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.link(t)
	r.gh.set(pathPull(), pullJSON("closed", false, false, "head1"))

	// Act
	r.tick()

	// Assert
	if want := []string{"GET " + pathPull()}; !reflect.DeepEqual(r.gh.calls, want) {
		t.Fatalf("calls = %v, want %v", r.gh.calls, want)
	}
	if got := r.linkState(t); got != wsm.PullRequestStateClosed {
		t.Fatalf("link state = %q, want closed", got)
	}
}

func TestAChangedStateRedrawsTheLinkingWorkspaceOnce(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.link(t)

	// Act: a first read, an unchanged one, and a merge.
	r.tick()
	afterFirst := len(r.linksRedrawn)
	r.tick()
	afterUnchanged := len(r.linksRedrawn)
	r.gh.set(pathPull(), pullJSON("closed", false, true, "head1"))
	r.tick()

	// Assert
	if got := []int{afterFirst, afterUnchanged, len(r.linksRedrawn)}; !reflect.DeepEqual(got, []int{1, 1, 2}) {
		t.Fatalf("redraws after each tick = %v, want [1 1 2]", got)
	}
	if r.linksRedrawn[1] != r.ws {
		t.Fatalf("redrew %v, want the rig's workspace", r.linksRedrawn)
	}
	if _, ok := dlog.FindRecord(r.log.Records(), dlog.RecordQuery{Level: "info", Operation: opLinked, Message: "the pull request's state changed; every workspace linking it draws its links again"}); !ok {
		t.Fatalf("records = %+v, want the change recorded", r.log.Records())
	}
}

func TestAMergedLinkedPullRequestIsNeverReadAgain(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.link(t)
	r.gh.set(pathPull(), pullJSON("closed", false, true, "head1"))
	r.tick()
	reads := len(r.gh.calls)

	// Act
	r.tick()

	// Assert
	if len(r.gh.calls) != reads {
		t.Fatalf("calls %v, want no read of a merged pull request", r.gh.calls[reads:])
	}
}

func TestAFailedStateReadKeepsTheStateAndIsRecorded(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.link(t)
	r.tick()
	r.gh.set(pathPull(), &ghclient.Failure{Kind: ghclient.KindUnreachable, Message: "dns"})

	// Act
	r.tick()

	// Assert
	if got := r.linkState(t); got != wsm.PullRequestStateOpen {
		t.Fatalf("link state = %q, want open as last read", got)
	}
	rec, ok := dlog.FindRecord(r.log.Records(), dlog.RecordQuery{Level: "info", Operation: opLinked, Message: "could not read the linked pull request's state; its badge stands until a later read"})
	if !ok || rec.Context["pull_request"] != prK.String() || rec.Context["kind"] != string(wsm.PullRequestFailureUnreachable) {
		t.Fatalf("record = %+v, %v; want the failure naming the pull request and its kind", rec, ok)
	}
}

func TestASpentRateLimitOnAStateReadIsWaitedOut(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.link(t)
	r.gh.set(pathPull(), &ghclient.Failure{Kind: ghclient.KindRateLimited, ResetsAt: t0.Add(time.Hour)})
	r.tick()
	reads := len(r.gh.calls)

	// Act: before the reset, and after it.
	r.clock.Set(t0.Add(time.Minute))
	r.tick()
	waited := len(r.gh.calls) - reads
	r.clock.Set(t0.Add(2 * time.Hour))
	r.tick()

	// Assert
	if got := []int{waited, len(r.gh.calls) - reads}; !reflect.DeepEqual(got, []int{0, 1}) {
		t.Fatalf("reads before and after the reset = %v, want [0 1]", got)
	}
}

func TestStateFailuresAreRecorded(t *testing.T) {
	storeErr := errors.New("disk gone")
	tests := []struct {
		name    string
		arrange func(r *rig)
		message string
	}{
		{name: "the linked pull requests cannot be listed",
			arrange: func(r *rig) { r.overStore(failingStates{Store: r.db, linkedErr: storeErr}) },
			message: "could not read the linked pull requests; no state is read this tick"},
		{name: "a state cannot be recorded",
			arrange: func(r *rig) { r.overStore(failingStates{Store: r.db, recordErr: storeErr}) },
			message: "could not record the pull request's state; its links' badges stand until a later read"},
		{name: "a workspace's links cannot be drawn again",
			arrange: func(r *rig) { r.linksRedrawnErr = storeErr },
			message: "could not draw a workspace's links again after its pull request's state changed"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			r := newRig(t)
			r.link(t)
			tt.arrange(r)

			// Act
			r.tick()

			// Assert
			rec, ok := dlog.FindRecord(r.log.Records(), dlog.RecordQuery{Level: "error", Operation: opLinked, Message: tt.message})
			if !ok || !strings.Contains(fmt.Sprint(rec.Context), storeErr.Error()) {
				t.Fatalf("records = %+v, want %q at error with its cause", r.log.Records(), tt.message)
			}
		})
	}
}

func TestNewRefusesAWatcherThatCannotRedrawLinks(t *testing.T) {
	// Arrange
	r := newRig(t)
	deps := r.deps
	deps.LinksRedrawn = nil

	// Act
	_, err := New(deps)

	// Assert
	if err == nil {
		t.Fatal("New without LinksRedrawn = nil, want refused")
	}
}

func TestDeliverKindStopsTheDeliveryWhenItsNoticesCannotBeRead(t *testing.T) {
	tests := []struct {
		name      string
		cancelled bool
		wantLevel string
		wantMsg   string
	}{
		{name: "a failed read", wantLevel: "error", wantMsg: "could not read the owed workflow-run notices; none is told this tick"},
		{name: "a stopping daemon", cancelled: true, wantLevel: "debug", wantMsg: "the delivery stopped with its context"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			r := newRig(t)
			ctx, cancel := context.WithCancel(context.Background())
			if tt.cancelled {
				cancel()
			} else {
				defer cancel()
			}
			load := func(context.Context) ([]wsm.GithubWorkflowRunOwed, error) {
				return []wsm.GithubWorkflowRunOwed{{Subscriber: r.ws, Run: runK, Pending: []string{"x."}}}, errors.New("store down")
			}

			// Act
			delivered := deliverKind(ctx, r.w, r.prompts, "workflow-run", load, githubWorkflowRunNotice)

			// Assert
			if delivered || len(r.told()) != 0 {
				t.Fatalf("delivered %v, told %q; want the delivery stopped with nothing told", delivered, r.told())
			}
			if _, ok := dlog.FindRecord(r.log.Records(), dlog.RecordQuery{Level: tt.wantLevel, Operation: opNotice, Message: tt.wantMsg}); !ok {
				t.Fatalf("no %s record %q: %+v", tt.wantLevel, tt.wantMsg, r.log.Records())
			}
		})
	}
}

func TestEveryNoticeKindIsDeliveredThroughDeliverKind(t *testing.T) {
	// Arrange
	src, err := os.ReadFile("poller.go")
	if err != nil {
		t.Fatal(err)
	}

	// Assert
	if n := strings.Count(string(src), "could not read the owed "); n != 1 {
		t.Fatalf("poller.go reads owed notices in %d places; every kind goes through deliverKind", n)
	}
	if n := strings.Count(string(src), "deliverKind(ctx, w, prompts, "); n != 3 {
		t.Fatalf("deliverOwed delivers %d kinds through deliverKind, want all 3", n)
	}
}
