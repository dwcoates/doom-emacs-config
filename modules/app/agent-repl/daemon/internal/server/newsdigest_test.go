package server

import (
	"context"
	"errors"
	"net/http"
	"strings"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
)

// webviewDaemonRequest is a webview's WatchDaemon request.
var webviewDaemonRequest = &agentreplv1.WatchDaemonRequest{
	Client: &agentreplv1.WatchDaemonRequest_Webview{Webview: &agentreplv1.WatchDaemonWebview{}},
}

// shownDigest is a standing digest with id.
func shownDigest(id string) *agentreplv1.NewsDigestStanding {
	return &agentreplv1.NewsDigestStanding{Standing: &agentreplv1.NewsDigestStanding_Shown{
		Shown: &frontendv1.NewsDigestOverlay{Id: &frontendv1.NewsDigestId{Value: id}},
	}}
}

func TestDismissNewsDigestDelegatesAndAnswersTheDigestsResponse(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.NewsDigest.dismiss = &agentreplv1.DismissNewsDigestResponse{Result: &agentreplv1.DismissNewsDigestResponse_Success{
		Success: &agentreplv1.DismissNewsDigestSuccess{},
	}}
	req := &agentreplv1.DismissNewsDigestRequest{Id: &frontendv1.NewsDigestId{Value: "d1"}}

	// Act.
	resp, err := h.Client.DismissNewsDigest(context.Background(), connect.NewRequest(req))

	// Assert.
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("DismissNewsDigest = (%v, %v), want the digest's success", resp, err)
	}
	if len(h.NewsDigest.dismissals) != 1 || h.NewsDigest.dismissals[0].GetId().GetValue() != "d1" {
		t.Fatalf("the digest saw %v, want the one dismiss of d1", h.NewsDigest.dismissals)
	}
}

func TestDismissNewsDigestRefusesARequestNamingNoDigest(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	_, err := h.Client.DismissNewsDigest(context.Background(), connect.NewRequest(&agentreplv1.DismissNewsDigestRequest{}))

	// Assert.
	var cerr *connect.Error
	if !errors.As(err, &cerr) || cerr.Code() != connect.CodeInvalidArgument {
		t.Fatalf("error = %v, want InvalidArgument", err)
	}
	if len(h.NewsDigest.dismissals) != 0 {
		t.Fatal("the digest was handed an unvalidated request")
	}
}

func TestDismissNewsDigestAnswersAStoreFailureAsInternal(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.NewsDigest.dismissErr = errors.New("the store failed")
	req := &agentreplv1.DismissNewsDigestRequest{Id: &frontendv1.NewsDigestId{Value: "d1"}}

	// Act.
	_, err := h.Client.DismissNewsDigest(context.Background(), connect.NewRequest(req))

	// Assert.
	var cerr *connect.Error
	if !errors.As(err, &cerr) || cerr.Code() != connect.CodeInternal {
		t.Fatalf("error = %v, want Internal", err)
	}
}

func TestRefreshNewsDigestDelegatesAndAnswersTheDigestsResponse(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.NewsDigest.refresh = &agentreplv1.RefreshNewsDigestResponse{Result: &agentreplv1.RefreshNewsDigestResponse_Error{
		Error: &agentreplv1.RefreshNewsDigestError{Cause: &agentreplv1.RefreshNewsDigestError_AlreadyRunning{
			AlreadyRunning: &agentreplv1.RefreshNewsDigestAlreadyRunning{},
		}},
	}}

	// Act.
	resp, err := h.Client.RefreshNewsDigest(context.Background(), connect.NewRequest(&agentreplv1.RefreshNewsDigestRequest{}))

	// Assert.
	if err != nil || resp.Msg.GetError().GetAlreadyRunning() == nil {
		t.Fatalf("RefreshNewsDigest = (%v, %v), want the digest's already_running", resp, err)
	}
	if h.NewsDigest.refreshes != 1 {
		t.Fatalf("refreshes = %d, want 1", h.NewsDigest.refreshes)
	}
}

func TestRefreshNewsDigestAnswersAFailureAsInternal(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.NewsDigest.refreshErr = errors.New("the store failed")

	// Act.
	_, err := h.Client.RefreshNewsDigest(context.Background(), connect.NewRequest(&agentreplv1.RefreshNewsDigestRequest{}))

	// Assert.
	var cerr *connect.Error
	if !errors.As(err, &cerr) || cerr.Code() != connect.CodeInternal {
		t.Fatalf("error = %v, want Internal", err)
	}
}

func TestAWebviewDaemonStreamIsToldTheNewsDigestStanding(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream := openDaemonStream(t, h, ctx, webviewDaemonRequest)

	// Act.
	h.NewsDigest.topic.Publish(shownDigest("d1"))

	// Assert.
	for stream.Receive() {
		if standing := stream.Msg().GetNewsDigest(); standing != nil {
			if standing.GetShown().GetId().GetValue() != "d1" {
				t.Fatalf("news_digest = %v, want d1 shown", standing)
			}
			return
		}
	}
	t.Fatalf("the stream ended without news_digest: %v", stream.Err())
}

func TestALateWebviewDaemonStreamIsHandedTheStandingDigest(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.NewsDigest.topic.Publish(shownDigest("d1"))
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Act.
	stream, err := h.Client.WatchDaemon(ctx, connect.NewRequest(webviewDaemonRequest))
	if err != nil {
		t.Fatalf("open the stream: %v", err)
	}

	// Assert.
	for stream.Receive() {
		if standing := stream.Msg().GetNewsDigest(); standing != nil {
			if standing.GetShown().GetId().GetValue() != "d1" {
				t.Fatalf("news_digest = %v, want d1 shown", standing)
			}
			return
		}
	}
	t.Fatalf("the stream ended without news_digest: %v", stream.Err())
}

func TestWhichDaemonStreamsSubscribeToTheNewsDigestStanding(t *testing.T) {
	tests := []struct {
		name string
		req  *agentreplv1.WatchDaemonRequest
		want int
	}{
		{"a webview stream subscribes", webviewDaemonRequest, 1},
		{"an Emacs stream does not", emacsDaemonRequest, 0},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()

			// Act.
			openDaemonStream(t, h, ctx, tc.req)

			// Assert.
			if got := h.NewsDigest.topic.Subscribers(); got != tc.want {
				t.Fatalf("news digest subscribers = %d, want %d", got, tc.want)
			}
		})
	}
}

func TestNewRefusesAMissingNewsDigest(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	deps := Deps{
		DB: h.DB, Prompts: h.Prompts, Queue: h.Queue, Verbs: h.Verbs, Merge: h.Merge,
		Drain: h.Drain, Rollout: h.Rollout, Deploy: h.Deployer, Health: h.Health, Login: h.Login,
		Ownership: h.Ownership, SuccessorAddress: func() string { return "" },
		Feed: h.Feed, Footer: h.Footer, Topbar: h.Topbar, Sidebar: h.Sidebar,
		Holds: h.Holds, LoudFaults: &h.LoudFaults, Focus: h.Focus, PersistentWifi: h.PersistentWifi,
		WebappDist: h.WebappDist, ImageOrigin: http.NotFoundHandler(), Log: h.Surfaces,
	}

	// Act.
	_, err := New(deps)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "the news digest") {
		t.Fatalf("New = %v, want the refusal naming the news digest", err)
	}
}
