package server

import (
	"context"
	"errors"
	"net/http"
	"strings"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
)

// joinedOn is a standing: joined, mode on.
func joinedOn() *agentreplv1.PersistentWifiState {
	return &agentreplv1.PersistentWifiState{
		Wifi: &agentreplv1.PersistentWifiState_Joined{Joined: &agentreplv1.PersistentWifiJoined{}},
		Mode: &agentreplv1.PersistentWifiState_On{On: &agentreplv1.PersistentWifiModeOn{}},
	}
}

func TestUpdatePersistentWifiModeDelegatesAndAnswersTheControllersResponse(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.PersistentWifi.response = &agentreplv1.UpdatePersistentWifiModeResponse{
		Result: &agentreplv1.UpdatePersistentWifiModeResponse_Success{
			Success: &agentreplv1.UpdatePersistentWifiModeSuccess{State: joinedOn()},
		},
	}
	req := &agentreplv1.UpdatePersistentWifiModeRequest{
		Action: &agentreplv1.UpdatePersistentWifiModeRequest_Toggle{Toggle: &agentreplv1.UpdatePersistentWifiModeToggle{}},
	}

	// Act.
	resp, err := h.Client.UpdatePersistentWifiMode(context.Background(), connect.NewRequest(req))

	// Assert.
	if err != nil {
		t.Fatalf("UpdatePersistentWifiMode() error = %v", err)
	}
	if resp.Msg.GetSuccess().GetState().GetOn() == nil {
		t.Fatalf("response = %v, want the controller's success", resp.Msg)
	}
	if len(h.PersistentWifi.requests) != 1 || h.PersistentWifi.requests[0].GetToggle() == nil {
		t.Fatalf("controller saw %v, want the one toggle", h.PersistentWifi.requests)
	}
}

func TestUpdatePersistentWifiModeRefusesARequestWithNoAction(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	_, err := h.Client.UpdatePersistentWifiMode(context.Background(),
		connect.NewRequest(&agentreplv1.UpdatePersistentWifiModeRequest{}))

	// Assert.
	var cerr *connect.Error
	if !errors.As(err, &cerr) || cerr.Code() != connect.CodeInvalidArgument {
		t.Fatalf("error = %v, want InvalidArgument", err)
	}
	if len(h.PersistentWifi.requests) != 0 {
		t.Fatal("the controller was handed an unvalidated request")
	}
}

func TestAnEmacsDaemonStreamIsToldThePersistentWifiStanding(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream := openDaemonStream(t, h, ctx, emacsDaemonRequest)

	// Act.
	h.PersistentWifi.topic.Publish(joinedOn())

	// Assert.
	for stream.Receive() {
		if standing := stream.Msg().GetPersistentWifi(); standing != nil {
			if standing.GetOn() == nil || standing.GetJoined() == nil {
				t.Fatalf("persistent_wifi = %v, want joined and on", standing)
			}
			return
		}
	}
	t.Fatalf("the stream ended without persistent_wifi: %v", stream.Err())
}

func TestWhichDaemonStreamsSubscribeToThePersistentWifiStanding(t *testing.T) {
	tests := []struct {
		name string
		req  *agentreplv1.WatchDaemonRequest
		want int
	}{
		{"an Emacs stream subscribes", emacsDaemonRequest, 1},
		{"a webview stream does not", &agentreplv1.WatchDaemonRequest{
			Client: &agentreplv1.WatchDaemonRequest_Webview{Webview: &agentreplv1.WatchDaemonWebview{}},
		}, 0},
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
			if got := h.PersistentWifi.topic.Subscribers(); got != tc.want {
				t.Fatalf("persistent-wifi subscribers = %d, want %d", got, tc.want)
			}
		})
	}
}

func TestNewRefusesAMissingPersistentWifiController(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	deps := Deps{
		DB: h.DB, Prompts: h.Prompts, Queue: h.Queue, Verbs: h.Verbs, Merge: h.Merge,
		Drain: h.Drain, Rollout: h.Rollout, Deploy: h.Deployer, Health: h.Health, Login: h.Login,
		Ownership: h.Ownership, SuccessorAddress: func() string { return "" },
		Feed: h.Feed, Footer: h.Footer, Topbar: h.Topbar, Sidebar: h.Sidebar,
		Holds: h.Holds, LoudFaults: &h.LoudFaults, Focus: h.Focus,
		WebappDist: h.WebappDist, ImageOrigin: http.NotFoundHandler(), Log: h.Surfaces,
	}

	// Act.
	_, err := New(deps)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "the persistent-wifi controller") {
		t.Fatalf("New = %v, want the refusal naming the persistent-wifi controller", err)
	}
}
