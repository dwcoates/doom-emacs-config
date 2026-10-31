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
	if !contains(told[1], recoveredSentence) || !contains(told[1], "@bob") {
		t.Fatalf("second notice = %q, want the recovery and the comment it missed", told[1])
	}
	if _, ok := dlog.FindRecord(r.log.Records(), dlog.RecordQuery{Level: "info", Operation: opPoll, Message: "the pull request could not be read; its subscriptions are failing"}); !ok {
		t.Fatalf("no record of the failed read: %+v", r.log.Records())
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
