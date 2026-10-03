package main

import (
	"context"
	"net/http"
	"strings"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"
	"connectrpc.com/connect"
)

// Streams are ACCEPTED on response headers, which the daemon flushes the
// moment the subscription is registered, so an open returns immediately even
// when nothing has been pushed.  These helpers therefore open synchronously
// and then wait on the SERVER-side registration before pushing, so a push can
// never race a subscribe.
//
// Callers do NOT Close() a stream: Close drains the response body, and a
// STANDING stream never finishes producing one.  Cancelling the request
// context is the graceful close on this contract (elisp.md, "Stream
// lifecycle").

func openHost(t *testing.T, server *fakeServer, client agentreplv1connectClient, id string) (*connect.ServerStreamForClient[agentreplv1.WatchHostWorkspaceResponse], context.CancelFunc) {
	t.Helper()
	ctx, cancel := context.WithCancel(context.Background())
	stream, err := client.WatchHostWorkspace(ctx,
		connect.NewRequest(&agentreplv1.WatchHostWorkspaceRequest{
			Workspace: &workspacev1.WorkspaceRef{Id: id, Dir: "/tmp/" + id}}))
	if err != nil {
		cancel()
		t.Fatalf("WatchHostWorkspace(%s): %v", id, err)
	}
	server.mustAwaitSubscribers(t, streamHost, id, 1)
	return stream, cancel
}

func openDaemon(t *testing.T, server *fakeServer, client agentreplv1connectClient) (*connect.ServerStreamForClient[agentreplv1.WatchDaemonResponse], context.CancelFunc) {
	t.Helper()
	ctx, cancel := context.WithCancel(context.Background())
	stream, err := client.WatchDaemon(ctx, connect.NewRequest(emacsWatchDaemon()))
	if err != nil {
		cancel()
		t.Fatalf("WatchDaemon: %v", err)
	}
	server.mustAwaitSubscribers(t, streamDaemon, "", 1)
	return stream, cancel
}

func TestPushReachesTheKeyedWorkspaceStream(t *testing.T) {
	// Arrange.
	server, baseURL := newTestServer(t)
	client := newTestClient(t, baseURL)
	stream, cancel := openHost(t, server, client, "ws-a")
	defer cancel()

	// Act.
	status, body := controlPost(t, baseURL, "/_fake/push",
		`{"stream":"host","workspace_id":"ws-a","message":{"reloadWebapp":{}}}`)
	if status != http.StatusOK {
		t.Fatalf("/_fake/push = %d %s", status, body)
	}

	// Assert: WatchHostWorkspace is ONE subscription per open workspace, so a
	// push keyed by that workspace lands on it.
	if !stream.Receive() {
		t.Fatalf("stream ended without a push: %v", stream.Err())
	}
	if stream.Msg().GetReloadWebapp() == nil {
		t.Fatalf("received %v, want the reload_webapp arm", stream.Msg())
	}
}

func TestPushDoesNotReachAnotherWorkspacesStream(t *testing.T) {
	// Arrange: two open workspaces, each with its own subscription.
	server, baseURL := newTestServer(t)
	client := newTestClient(t, baseURL)
	streamA, cancelA := openHost(t, server, client, "ws-a")
	defer cancelA()
	streamB, cancelB := openHost(t, server, client, "ws-b")
	defer cancelB()

	// Act.
	if status, body := controlPost(t, baseURL, "/_fake/push",
		`{"stream":"host","workspace_id":"ws-a","message":{"reloadWebapp":{}}}`); status != http.StatusOK {
		t.Fatalf("/_fake/push = %d %s", status, body)
	}
	if !streamA.Receive() {
		t.Fatalf("ws-a stream ended without the push: %v", streamA.Err())
	}

	// Assert: ws-b is ended cleanly and drained; a workspace-keyed push must
	// have put NOTHING on another workspace's stream.  Ending it is what makes
	// "nothing arrived" observable without waiting on a clock.
	if status, body := controlPost(t, baseURL, "/_fake/end",
		`{"stream":"host","workspace_id":"ws-b"}`); status != http.StatusOK {
		t.Fatalf("/_fake/end = %d %s", status, body)
	}
	received := 0
	for streamB.Receive() {
		received++
	}
	if streamB.Err() != nil {
		t.Fatalf("ws-b stream ended with %v, want a clean end", streamB.Err())
	}
	if received != 0 {
		t.Fatalf("ws-b received %d messages, want none", received)
	}
}

func TestSnapshotIsReplayedToALaterSubscriber(t *testing.T) {
	// Arrange: the schedule is stored BEFORE anyone subscribes.
	server, baseURL := newTestServer(t)
	client := newTestClient(t, baseURL)
	if status, body := controlPost(t, baseURL, "/_fake/push",
		`{"stream":"daemon","snapshot":true,"message":{"drainScheduled":{"atMs":"1735689600000","reason":{"deploy":{}}}}}`); status != http.StatusOK {
		t.Fatalf("/_fake/push = %d %s", status, body)
	}

	// Act.
	stream, cancel := openDaemon(t, server, client)
	defer cancel()

	// Assert: streams are "now", never "since" — a late subscriber gets the
	// standing state as its first push.
	if !stream.Receive() {
		t.Fatalf("stream ended without the snapshot: %v", stream.Err())
	}
	if stream.Msg().GetDrainScheduled().GetReason().GetDeploy() == nil {
		t.Fatalf("received %v, want the standing drain schedule", stream.Msg())
	}
}

func TestPushRefusesAnUnknownStream(t *testing.T) {
	// Arrange.
	_, baseURL := newTestServer(t)

	// Act.
	status, body := controlPost(t, baseURL, "/_fake/push", `{"stream":"footer","message":{}}`)

	// Assert.
	if status != http.StatusBadRequest {
		t.Fatalf("/_fake/push on an unknown stream = %d %s, want 400", status, body)
	}
}

func TestPushRefusesAnUnknownMessageField(t *testing.T) {
	// Arrange.
	_, baseURL := newTestServer(t)

	// Act.
	status, body := controlPost(t, baseURL, "/_fake/push",
		`{"stream":"daemon","message":{"drainSchedule":{}}}`)

	// Assert: the push is validated against the generated response type, so a
	// misspelled arm is a 400 here rather than a puzzle in the suite.
	if status != http.StatusBadRequest || !strings.Contains(body, "drainSchedule") {
		t.Fatalf("/_fake/push with an unknown field = %d %s, want 400 naming it", status, body)
	}
}

func TestPushRefusesAHostPushWithoutAWorkspaceId(t *testing.T) {
	// Arrange.
	_, baseURL := newTestServer(t)

	// Act.
	status, body := controlPost(t, baseURL, "/_fake/push",
		`{"stream":"host","message":{"reloadWebapp":{}}}`)

	// Assert: the host stream is keyed by workspace; an unkeyed push has no
	// defensible destination.
	if status != http.StatusBadRequest {
		t.Fatalf("/_fake/push = %d %s, want 400", status, body)
	}
}

func TestPushRefusesAWorkspaceIdOnAGlobalStream(t *testing.T) {
	// Arrange.
	_, baseURL := newTestServer(t)

	// Act.
	status, body := controlPost(t, baseURL, "/_fake/push",
		`{"stream":"daemon","workspace_id":"ws-a","message":{"drainCancelled":{}}}`)

	// Assert: WatchDaemon is the workspace-INDEPENDENT channel by ruling.
	if status != http.StatusBadRequest {
		t.Fatalf("/_fake/push = %d %s, want 400", status, body)
	}
}

func TestEndWithErrorDeliversTheConnectError(t *testing.T) {
	// Arrange.
	server, baseURL := newTestServer(t)
	client := newTestClient(t, baseURL)
	stream, cancel := openDaemon(t, server, client)
	defer cancel()

	// Act.
	if status, body := controlPost(t, baseURL, "/_fake/end",
		`{"stream":"daemon","error":{"code":"unavailable","message":"daemon went away"}}`); status != http.StatusOK {
		t.Fatalf("/_fake/end = %d %s", status, body)
	}

	// Assert: an end frame carrying an error is a stream FAILURE the client
	// must surface, not a graceful close.
	for stream.Receive() {
	}
	if connect.CodeOf(stream.Err()) != connect.CodeUnavailable {
		t.Fatalf("stream error = %v, want unavailable", stream.Err())
	}
}

func TestCleanEndDeliversNoError(t *testing.T) {
	// Arrange.
	server, baseURL := newTestServer(t)
	client := newTestClient(t, baseURL)
	stream, cancel := openDaemon(t, server, client)
	defer cancel()

	// Act.
	if status, body := controlPost(t, baseURL, "/_fake/end", `{"stream":"daemon"}`); status != http.StatusOK {
		t.Fatalf("/_fake/end = %d %s", status, body)
	}

	// Assert: the end frame arrives cleanly.  (A STANDING stream's consumer
	// treats that as a failure of its own — that policy is the client's.)
	for stream.Receive() {
	}
	if stream.Err() != nil {
		t.Fatalf("stream error = %v, want a clean end", stream.Err())
	}
}

func TestAbortEndsTheStreamWithoutAnEndFrame(t *testing.T) {
	// Arrange.
	server, baseURL := newTestServer(t)
	client := newTestClient(t, baseURL)
	stream, cancel := openDaemon(t, server, client)
	defer cancel()

	// Act.
	if status, body := controlPost(t, baseURL, "/_fake/end",
		`{"stream":"daemon","abort":true}`); status != http.StatusOK {
		t.Fatalf("/_fake/end = %d %s", status, body)
	}

	// Assert: a producer-side end WITHOUT a terminal frame is a transport
	// failure, and must not read as a clean close.
	for stream.Receive() {
	}
	if stream.Err() == nil {
		t.Fatalf("aborted stream reported a clean close; want a transport failure")
	}
}

func TestEndRefusesAbortCarryingAnError(t *testing.T) {
	// Arrange.
	_, baseURL := newTestServer(t)

	// Act.
	status, body := controlPost(t, baseURL, "/_fake/end",
		`{"stream":"daemon","abort":true,"error":{"code":"internal","message":"x"}}`)

	// Assert: abort writes no frame at all, so it cannot also carry one.
	if status != http.StatusBadRequest {
		t.Fatalf("/_fake/end = %d %s, want 400", status, body)
	}
}

func TestEndRefusesAnUnknownConnectCode(t *testing.T) {
	// Arrange.
	_, baseURL := newTestServer(t)

	// Act.
	status, body := controlPost(t, baseURL, "/_fake/end",
		`{"stream":"daemon","error":{"code":"teapot","message":"x"}}`)

	// Assert.
	if status != http.StatusBadRequest {
		t.Fatalf("/_fake/end = %d %s, want 400", status, body)
	}
}

func TestSubscribersListsOpenStreams(t *testing.T) {
	// Arrange.
	server, baseURL := newTestServer(t)
	client := newTestClient(t, baseURL)
	_, cancel := openHost(t, server, client, "ws-a")
	defer cancel()

	// Act.
	status, body := controlGet(t, baseURL, "/_fake/subscribers")

	// Assert.
	if status != http.StatusOK || !strings.Contains(body, `"workspace_id":"ws-a"`) {
		t.Fatalf("/_fake/subscribers = %d %s", status, body)
	}
}

func TestControlBodyRefusesAnUnknownField(t *testing.T) {
	// Arrange.
	_, baseURL := newTestServer(t)

	// Act.
	status, body := controlPost(t, baseURL, "/_fake/push",
		`{"stream":"daemon","messages":{}}`)

	// Assert: a control body is parsed strictly too, so a typo in a helper is
	// a loud 400 rather than a silently ignored instruction.
	if status != http.StatusBadRequest {
		t.Fatalf("/_fake/push with an unknown control field = %d %s, want 400", status, body)
	}
}

func TestNotificationClickedPushIsAccepted(t *testing.T) {
	// Arrange: the fake validates every push against the REGENERATED types,
	// so this also proves the bindings in the tree carry the click arm.
	server, baseURL := newTestServer(t)
	client := newTestClient(t, baseURL)
	stream, cancel := openHost(t, server, client, "ws-a")
	defer cancel()

	// Act.
	status, body := controlPost(t, baseURL, "/_fake/push",
		`{"stream":"host","workspace_id":"ws-a","message":{"notificationClicked":{}}}`)
	if status != http.StatusOK {
		t.Fatalf("/_fake/push = %d %s", status, body)
	}

	// Assert.
	if !stream.Receive() {
		t.Fatalf("stream ended without the push: %v", stream.Err())
	}
	if stream.Msg().GetNotificationClicked() == nil {
		t.Fatalf("received %v, want the notification_clicked arm", stream.Msg())
	}
}

func TestTheRetiredNotificationPushIsRefused(t *testing.T) {
	// Arrange.
	_, baseURL := newTestServer(t)

	// Act: `notification' is a reserved field of WatchHostWorkspaceResponse.
	status, body := controlPost(t, baseURL, "/_fake/push",
		`{"stream":"host","workspace_id":"ws-a","message":{"notification":{"text":"x"}}}`)

	// Assert.
	if status != http.StatusBadRequest || !strings.Contains(body, "notification") {
		t.Fatalf("retired notification push = %d %s, want 400 naming it", status, body)
	}
}
