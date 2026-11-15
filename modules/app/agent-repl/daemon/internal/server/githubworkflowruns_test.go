package server

import (
	"context"
	"errors"
	"reflect"
	"testing"
	"time"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ghclient"
	"agent-repld/internal/ids"
	"agent-repld/internal/prwatch"
	"agent-repld/internal/wsm"
)

func (d *fakeDB) SubscribeGithubWorkflowRun(_ context.Context, sub wsm.GithubWorkflowRunSubscription) (bool, error) {
	if d.runErr != nil {
		return false, d.runErr
	}
	for _, held := range d.runSubs {
		if held.Run == sub.Run {
			return true, nil
		}
	}
	d.runSubs = append(d.runSubs, sub)
	return false, nil
}

func (d *fakeDB) UnsubscribeGithubWorkflowRun(_ context.Context, _ ids.WorkspaceID, key wsm.GithubWorkflowRunKey) (bool, error) {
	for i, sub := range d.runSubs {
		if sub.Run == key {
			d.runSubs = append(d.runSubs[:i], d.runSubs[i+1:]...)
			return true, nil
		}
	}
	return false, nil
}

func (d *fakeDB) WorkspaceGithubWorkflowRunSubscriptions(context.Context, ids.WorkspaceID) ([]wsm.GithubWorkflowRunSubscription, error) {
	return d.runSubs, nil
}

func (d *fakeDB) OweGithubWorkflowRunCompletion(_ context.Context, c wsm.GithubWorkflowRunCompletion) (bool, error) {
	if d.runErr != nil {
		return false, d.runErr
	}
	d.runOwed = append(d.runOwed, c)
	return true, nil
}

var (
	testRun     = wsm.GithubWorkflowRunKey{Owner: "ChessCom", Repo: "explanation-engine", RunID: 43771255095}
	testRunURL  = "https://github.com/ChessCom/explanation-engine/actions/runs/43771255095"
	workflowRunReadAt = time.Date(2026, 11, 15, 12, 0, 0, 0, time.UTC)
)

// runAs is testRun as GitHub answers it, STATUS and CONCLUSION as given.
func runAs(status, conclusion string) prwatch.Run {
	return prwatch.Run{Name: "CEAC sandbox release", Status: status, Conclusion: conclusion, HeadSHA: "abc123", URL: testRunURL, ReadAt: workflowRunReadAt}
}

func subscribeRunURL() *connect.Request[agentreplv1.SubscribeGithubWorkflowRunRequest] {
	return connect.NewRequest(&agentreplv1.SubscribeGithubWorkflowRunRequest{Subscriber: ref(), Run: &agentreplv1.SubscribeGithubWorkflowRunRequest_Url{Url: testRunURL}})
}

func TestSubscribeGithubWorkflowRunRecordsASubscriptionToAnUnfinishedRun(t *testing.T) {
	tests := []struct {
		name      string
		req       *connect.Request[agentreplv1.SubscribeGithubWorkflowRunRequest]
		origin    string
		held      bool
		wantTaken string
	}{
		{name: "a run by its address", req: subscribeRunURL(), wantTaken: "subscribed"},
		{name: "a run named exactly", req: connect.NewRequest(&agentreplv1.SubscribeGithubWorkflowRunRequest{Subscriber: ref(),
			Run: &agentreplv1.SubscribeGithubWorkflowRunRequest_Named{Named: prwatch.RunRefProto(testRun)}}), wantTaken: "subscribed"},
		{name: "a run of the subscriber's own repository", req: connect.NewRequest(&agentreplv1.SubscribeGithubWorkflowRunRequest{Subscriber: ref(),
			Run: &agentreplv1.SubscribeGithubWorkflowRunRequest_InSubscribersRepository{InSubscribersRepository: &agentreplv1.SubscribeGithubWorkflowRunInSubscribersRepository{RunId: testRun.RunID}}}),
			origin: "git@github.com:ChessCom/explanation-engine.git", wantTaken: "subscribed"},
		{name: "a run already waited on", req: subscribeRunURL(), held: true, wantTaken: "already_subscribed"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.Remotes.url = tt.origin
			h.PRs.run = runAs("in_progress", "")
			if tt.held {
				h.DB.runSubs = []wsm.GithubWorkflowRunSubscription{{Subscriber: testWorkspaceID, Run: testRun}}
			}

			// Act
			resp, err := h.Client.SubscribeGithubWorkflowRun(context.Background(), tt.req)

			// Assert
			if err != nil {
				t.Fatalf("SubscribeGithubWorkflowRun: %v", err)
			}
			success := resp.Msg.GetSuccess()
			taken, sub := "subscribed", success.GetSubscribed().GetSubscription()
			if a := success.GetAlreadySubscribed(); a != nil {
				taken, sub = "already_subscribed", a.GetSubscription()
			}
			if taken != tt.wantTaken || prwatch.RunKeyOf(success.GetRun()) != testRun || success.GetStatus().GetUnfinished().GetStatus() != "in_progress" {
				t.Fatalf("success = %v, want %s of the run, unfinished", success, tt.wantTaken)
			}
			if sub.GetSubscriber().GetDir() != testWorkspaceDir || prwatch.RunKeyOf(sub.GetRun()) != testRun {
				t.Fatalf("subscription = %v, want the run naming its subscriber's directory", sub)
			}
			if !reflect.DeepEqual(h.PRs.runReads, []wsm.GithubWorkflowRunKey{testRun}) || len(h.DB.runSubs) != 1 || len(h.DB.runOwed) != 0 || h.PRs.wakes != 0 {
				t.Fatalf("reads %v, %d recorded, %d owed, %d wakes; want one read, one subscription, nothing owed", h.PRs.runReads, len(h.DB.runSubs), len(h.DB.runOwed), h.PRs.wakes)
			}
			if !tt.held && !h.DB.runSubs[0].SubscribedAt.Equal(workflowRunReadAt) {
				t.Fatalf("subscribed at %v, want the read's time %v", h.DB.runSubs[0].SubscribedAt, workflowRunReadAt)
			}
		})
	}
}

func TestSubscribeGithubWorkflowRunToAFinishedRunOwesItsNoticeAtOnce(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.PRs.run = runAs("completed", "success")

	// Act
	resp, err := h.Client.SubscribeGithubWorkflowRun(context.Background(), subscribeRunURL())

	// Assert
	if err != nil || resp.Msg.GetSuccess().GetAlreadyCompleted() == nil || resp.Msg.GetSuccess().GetStatus().GetCompleted().GetConclusion().GetSuccess() == nil {
		t.Fatalf("SubscribeGithubWorkflowRun = %v, %v; want already_completed with its success", resp, err)
	}
	want := []wsm.GithubWorkflowRunCompletion{{Subscriber: testWorkspaceID, Run: testRun, Held: false,
		Sentences: []string{`The run of workflow "CEAC sandbox release" finished: success.`}, Status: "Head commit abc123: " + testRunURL}}
	if !reflect.DeepEqual(h.DB.runOwed, want) {
		t.Fatalf("owed = %+v, want %+v", h.DB.runOwed, want)
	}
	if h.PRs.wakes != 1 || len(h.DB.runSubs) != 0 {
		t.Fatalf("wakes %d, %d recorded; want the watcher woken once and nothing subscribed", h.PRs.wakes, len(h.DB.runSubs))
	}
}

func TestSubscribeGithubWorkflowRunIsRefused(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness) *connect.Request[agentreplv1.SubscribeGithubWorkflowRunRequest]
		got     func(*agentreplv1.SubscribeGithubWorkflowRunError) bool
	}{
		{name: "an address that names no run", arrange: func(*harness) *connect.Request[agentreplv1.SubscribeGithubWorkflowRunRequest] {
			return connect.NewRequest(&agentreplv1.SubscribeGithubWorkflowRunRequest{Subscriber: ref(), Run: &agentreplv1.SubscribeGithubWorkflowRunRequest_Url{Url: "https://github.com/o/r/pull/4"}})
		}, got: func(e *agentreplv1.SubscribeGithubWorkflowRunError) bool {
			return e.GetUnparsable().GetText() == "https://github.com/o/r/pull/4"
		}},
		{name: "a repository not on github.com", arrange: func(h *harness) *connect.Request[agentreplv1.SubscribeGithubWorkflowRunRequest] {
			h.Remotes.url = "git@gitlab.com:o/r.git"
			return connect.NewRequest(&agentreplv1.SubscribeGithubWorkflowRunRequest{Subscriber: ref(),
				Run: &agentreplv1.SubscribeGithubWorkflowRunRequest_InSubscribersRepository{InSubscribersRepository: &agentreplv1.SubscribeGithubWorkflowRunInSubscribersRepository{RunId: 5}}})
		}, got: func(e *agentreplv1.SubscribeGithubWorkflowRunError) bool {
			return e.GetNotOnGithub().GetOrigin() == "git@gitlab.com:o/r.git"
		}},
		{name: "rejected credentials", arrange: func(h *harness) *connect.Request[agentreplv1.SubscribeGithubWorkflowRunRequest] {
			h.PRs.runErr = &ghclient.Failure{Kind: ghclient.KindUnauthorized, Status: 401, Message: "Bad credentials"}
			return subscribeRunURL()
		}, got: func(e *agentreplv1.SubscribeGithubWorkflowRunError) bool {
			return e.GetUnreadable().GetCause().GetUnauthorized() != nil
		}},
		{name: "a run GitHub cannot find", arrange: func(h *harness) *connect.Request[agentreplv1.SubscribeGithubWorkflowRunRequest] {
			h.PRs.runErr = &ghclient.Failure{Kind: ghclient.KindNotFound, Status: 404}
			return subscribeRunURL()
		}, got: func(e *agentreplv1.SubscribeGithubWorkflowRunError) bool {
			return e.GetUnreadable().GetCause().GetNotFound() != nil
		}},
		{name: "a subscriber that closed meanwhile", arrange: func(h *harness) *connect.Request[agentreplv1.SubscribeGithubWorkflowRunRequest] {
			h.PRs.run = runAs("queued", "")
			h.DB.runErr = wsm.ErrSubscriberClosed
			return subscribeRunURL()
		}, got: func(e *agentreplv1.SubscribeGithubWorkflowRunError) bool { return e.GetSubscriberClosed() != nil }},
		{name: "a subscriber forgotten before a finished run was owed", arrange: func(h *harness) *connect.Request[agentreplv1.SubscribeGithubWorkflowRunRequest] {
			h.PRs.run = runAs("completed", "failure")
			h.DB.runErr = wsm.ErrNotFound
			return subscribeRunURL()
		}, got: func(e *agentreplv1.SubscribeGithubWorkflowRunError) bool { return e.GetUnknownSubscriber() != nil }},
		{name: "an unknown subscriber", arrange: func(*harness) *connect.Request[agentreplv1.SubscribeGithubWorkflowRunRequest] {
			req := subscribeRunURL()
			req.Msg.Subscriber = &workspacev1.WorkspaceRef{Id: "ws-none"}
			return req
		}, got: func(e *agentreplv1.SubscribeGithubWorkflowRunError) bool { return e.GetUnknownSubscriber() != nil }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			req := tt.arrange(h)

			// Act
			resp, err := h.Client.SubscribeGithubWorkflowRun(context.Background(), req)

			// Assert
			if err != nil || !tt.got(resp.Msg.GetError()) || len(h.DB.runSubs) != 0 || len(h.DB.runOwed) != 0 || h.PRs.wakes != 0 {
				t.Fatalf("SubscribeGithubWorkflowRun = %v, %v with %d recorded, %d owed; want the refusal and nothing recorded", resp, err, len(h.DB.runSubs), len(h.DB.runOwed))
			}
		})
	}
}

func TestSubscribeGithubWorkflowRunRecordsARejectedCredential(t *testing.T) {
	// Arrange
	h := newHarness(t)
	log := dlog.NewTestLogger()
	h.Surfaces.workspace = log
	h.PRs.runErr = &ghclient.Failure{Kind: ghclient.KindUnauthorized, Method: "GET", Path: "repos/x", Status: 401, Message: "Bad credentials"}

	// Act
	_, err := h.Client.SubscribeGithubWorkflowRun(context.Background(), subscribeRunURL())

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	record, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "info", Operation: opGithubWorkflowRuns, Message: "refused a GitHub workflow-run subscription"})
	if !ok || record.Context["reason"] != h.PRs.runErr.Error() {
		t.Fatalf("no refusal record naming the rejected credentials: %+v", log.Records())
	}
}

func TestSubscribeGithubWorkflowRunFailsWhenTheStoreFails(t *testing.T) {
	for _, status := range []string{"in_progress", "completed"} {
		t.Run(status, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.PRs.run = runAs(status, "success")
			h.DB.runErr = errors.New("disk full")

			// Act
			_, err := h.Client.SubscribeGithubWorkflowRun(context.Background(), subscribeRunURL())

			// Assert
			if connect.CodeOf(err) != connect.CodeInternal || h.PRs.wakes != 0 {
				t.Fatalf("SubscribeGithubWorkflowRun = %v with %d wakes; want an internal failure and no wake", err, h.PRs.wakes)
			}
		})
	}
}

func TestSubscribeGithubWorkflowRunValidation(t *testing.T) {
	tests := []struct {
		name string
		req  *agentreplv1.SubscribeGithubWorkflowRunRequest
	}{
		{name: "no run", req: &agentreplv1.SubscribeGithubWorkflowRunRequest{Subscriber: ref()}},
		{name: "an empty address", req: &agentreplv1.SubscribeGithubWorkflowRunRequest{Subscriber: ref(), Run: &agentreplv1.SubscribeGithubWorkflowRunRequest_Url{}}},
		{name: "a named run missing its id", req: &agentreplv1.SubscribeGithubWorkflowRunRequest{Subscriber: ref(),
			Run: &agentreplv1.SubscribeGithubWorkflowRunRequest_Named{Named: &agentreplv1.GithubWorkflowRunRef{Repository: &agentreplv1.GithubRepositoryRef{Owner: "o", Repo: "r"}}}}},
		{name: "an own-repository run missing its id", req: &agentreplv1.SubscribeGithubWorkflowRunRequest{Subscriber: ref(),
			Run: &agentreplv1.SubscribeGithubWorkflowRunRequest_InSubscribersRepository{InSubscribersRepository: &agentreplv1.SubscribeGithubWorkflowRunInSubscribersRepository{}}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			_, err := h.Client.SubscribeGithubWorkflowRun(context.Background(), connect.NewRequest(tt.req))

			// Assert
			if connect.CodeOf(err) != connect.CodeInvalidArgument || len(h.PRs.runReads) != 0 {
				t.Fatalf("SubscribeGithubWorkflowRun = %v; want InvalidArgument and no read", err)
			}
		})
	}
}

func TestUnsubscribeGithubWorkflowRunAnswersWhetherOneStood(t *testing.T) {
	for _, held := range []bool{true, false} {
		t.Run(map[bool]string{true: "a subscription ended", false: "none held"}[held], func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			if held {
				h.DB.runSubs = []wsm.GithubWorkflowRunSubscription{{Subscriber: testWorkspaceID, Run: testRun}}
			}

			// Act
			resp, err := h.Client.UnsubscribeGithubWorkflowRun(context.Background(), connect.NewRequest(&agentreplv1.UnsubscribeGithubWorkflowRunRequest{
				Subscriber: ref(), Run: prwatch.RunRefProto(testRun)}))

			// Assert
			if err != nil || (resp.Msg.GetSuccess().GetUnsubscribed() != nil) != held || (resp.Msg.GetSuccess().GetNotSubscribed() != nil) == held {
				t.Fatalf("UnsubscribeGithubWorkflowRun = %v, %v; want ended %v", resp, err, held)
			}
		})
	}
}

func TestUnsubscribeGithubWorkflowRunValidation(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	_, err := h.Client.UnsubscribeGithubWorkflowRun(context.Background(), connect.NewRequest(&agentreplv1.UnsubscribeGithubWorkflowRunRequest{
		Subscriber: ref(), Run: &agentreplv1.GithubWorkflowRunRef{Repository: &agentreplv1.GithubRepositoryRef{Owner: "o", Repo: "r"}}}))

	// Assert
	if connect.CodeOf(err) != connect.CodeInvalidArgument {
		t.Fatalf("UnsubscribeGithubWorkflowRun = %v; want InvalidArgument for a run naming no id", err)
	}
}

func TestListGithubWorkflowRunSubscriptionsAnswersEveryOne(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.DB.runSubs = []wsm.GithubWorkflowRunSubscription{{Subscriber: testWorkspaceID, Run: testRun}}

	// Act
	resp, err := h.Client.ListGithubWorkflowRunSubscriptions(context.Background(), connect.NewRequest(&agentreplv1.ListGithubWorkflowRunSubscriptionsRequest{Subscriber: ref()}))

	// Assert
	subs := resp.Msg.GetSuccess().GetSubscriptions()
	if err != nil || len(subs) != 1 || prwatch.RunKeyOf(subs[0].GetRun()) != testRun || subs[0].GetSubscriber().GetDir() != testWorkspaceDir {
		t.Fatalf("ListGithubWorkflowRunSubscriptions = %v, %v; want the one held", resp, err)
	}
}
