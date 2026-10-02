package main

import (
	"context"
	"net/http"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	"connectrpc.com/connect"
)

// WatchDaemon is held by Emacs AND every webview, so the request names its
// client, and an Emacs client states the elisp it has loaded. The fake refuses
// exactly what the real daemon refuses, as invalid_argument.
func TestWatchDaemonRefusesAnIncompleteClient(t *testing.T) {
	cases := []struct {
		name string
		req  *agentreplv1.WatchDaemonRequest
	}{
		{name: "no client named", req: &agentreplv1.WatchDaemonRequest{}},
		{name: "emacs with an empty build", req: &agentreplv1.WatchDaemonRequest{
			Client: &agentreplv1.WatchDaemonRequest_Emacs{Emacs: &agentreplv1.WatchDaemonEmacs{Focus: unfocused(), Instance: instance()}},
		}},
		{name: "emacs with no focus", req: &agentreplv1.WatchDaemonRequest{
			Client: &agentreplv1.WatchDaemonRequest_Emacs{Emacs: &agentreplv1.WatchDaemonEmacs{ElispBuild: "fixture-elisp-build", Instance: instance()}},
		}},
		{name: "emacs with a focus naming no arm", req: &agentreplv1.WatchDaemonRequest{
			Client: &agentreplv1.WatchDaemonRequest_Emacs{Emacs: &agentreplv1.WatchDaemonEmacs{
				ElispBuild: "fixture-elisp-build", Focus: &agentreplv1.EditorFocus{}, Instance: instance(),
			}},
		}},
		{name: "emacs with no instance", req: &agentreplv1.WatchDaemonRequest{
			Client: &agentreplv1.WatchDaemonRequest_Emacs{Emacs: &agentreplv1.WatchDaemonEmacs{
				ElispBuild: "fixture-elisp-build", Focus: unfocused(),
			}},
		}},
		{name: "emacs with an empty instance", req: &agentreplv1.WatchDaemonRequest{
			Client: &agentreplv1.WatchDaemonRequest_Emacs{Emacs: &agentreplv1.WatchDaemonEmacs{
				ElispBuild: "fixture-elisp-build", Focus: unfocused(), Instance: &agentreplv1.EditorInstance{},
			}},
		}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			_, baseURL := newTestServer(t)
			client := newTestClient(t, baseURL)

			// Act.
			stream, err := client.WatchDaemon(context.Background(), connect.NewRequest(tc.req))
			if err != nil {
				t.Fatalf("open WatchDaemon: %v", err)
			}
			defer stream.Close()
			stream.Receive()

			// Assert.
			if connect.CodeOf(stream.Err()) != connect.CodeInvalidArgument {
				t.Fatalf("WatchDaemon error = %v, want invalid_argument", stream.Err())
			}
		})
	}
}

func TestWatchDaemonAcceptsAWebviewClient(t *testing.T) {
	// Arrange.
	server, baseURL := newTestServer(t)
	client := newTestClient(t, baseURL)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Act.
	_, err := client.WatchDaemon(ctx, connect.NewRequest(&agentreplv1.WatchDaemonRequest{
		Client: &agentreplv1.WatchDaemonRequest_Webview{Webview: &agentreplv1.WatchDaemonWebview{}},
	}))
	if err != nil {
		t.Fatalf("open WatchDaemon: %v", err)
	}

	// Assert: a webview states no build here, and its watch stands.
	server.awaitSubscribers(streamDaemon, "", 1)
}

func TestPushDeliversAReloadElispToTheDaemonStream(t *testing.T) {
	// Arrange.
	server, baseURL := newTestServer(t)
	client := newTestClient(t, baseURL)
	stream, cancel := openDaemon(t, server, client)
	defer cancel()

	// Act.
	status, body := controlPost(t, baseURL, "/_fake/push",
		`{"stream":"daemon","message":{"reloadElisp":{"moduleRoot":"/root/","build":"abc"}}}`)
	if status != http.StatusOK {
		t.Fatalf("/_fake/push = %d %s", status, body)
	}

	// Assert.
	if !stream.Receive() {
		t.Fatalf("no push received: %v", stream.Err())
	}
	reload := stream.Msg().GetReloadElisp()
	if reload.GetModuleRoot() != "/root/" || reload.GetBuild() != "abc" {
		t.Fatalf("push = %v, want reloadElisp{/root/, abc}", stream.Msg())
	}
}

func TestDefaultDeployIsASuccess(t *testing.T) {
	// Arrange.
	_, baseURL := newTestServer(t)
	client := newTestClient(t, baseURL)

	// Act.
	resp, err := client.Deploy(context.Background(),
		connect.NewRequest(&agentreplv1.DeployRequest{Force: true}))
	if err != nil {
		t.Fatalf("Deploy: %v", err)
	}

	// Assert.
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("Deploy default is %v, want success", resp.Msg)
	}
}

func TestReportEditorFocusAnswersSuccessByDefault(t *testing.T) {
	// Arrange.
	_, baseURL := newTestServer(t)
	client := newTestClient(t, baseURL)

	// Act.
	resp, err := client.ReportEditorFocus(context.Background(), connect.NewRequest(&agentreplv1.ReportEditorFocusRequest{
		Focus: unfocused(),
	}))

	// Assert.
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("ReportEditorFocus = (%v, %v), want success", resp, err)
	}
}

func TestReportEditorFocusRefusesAnUnsetFocus(t *testing.T) {
	// Arrange.
	_, baseURL := newTestServer(t)
	client := newTestClient(t, baseURL)

	// Act.
	_, err := client.ReportEditorFocus(context.Background(), connect.NewRequest(&agentreplv1.ReportEditorFocusRequest{}))

	// Assert.
	if connect.CodeOf(err) != connect.CodeInvalidArgument {
		t.Fatalf("ReportEditorFocus error = %v, want invalid_argument", err)
	}
}
