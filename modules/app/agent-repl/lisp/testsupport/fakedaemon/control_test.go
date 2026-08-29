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

// streamResult carries a client stream open that completes only once the
// server has flushed response headers.  Connect over HTTP/1.1 does not flush
// them until the handler's first Send (or its end frame), so a stream that
// has been pushed nothing keeps the OPEN outstanding — which these tests use
// as the observable for "nothing was delivered".
type streamResult[T any] struct {
	stream *connect.ServerStreamForClient[T]
	err    error
}

func openAsync[T any](open func(context.Context) (*connect.ServerStreamForClient[T], error)) (<-chan streamResult[T], context.CancelFunc) {
	ctx, cancel := context.WithCancel(context.Background())
	out := make(chan streamResult[T], 1)
	go func() {
		stream, err := open(ctx)
		out <- streamResult[T]{stream: stream, err: err}
	}()
	return out, cancel
}

func openHostAsync(client agentreplv1connectClient, id string) (<-chan streamResult[agentreplv1.WatchHostWorkspaceResponse], context.CancelFunc) {
	return openAsync(func(ctx context.Context) (*connect.ServerStreamForClient[agentreplv1.WatchHostWorkspaceResponse], error) {
		return client.WatchHostWorkspace(ctx,
			connect.NewRequest(&agentreplv1.WatchHostWorkspaceRequest{
				Workspace: &workspacev1.WorkspaceRef{Id: id, Dir: "/tmp/" + id}}))
	})
}

func openDaemonAsync(client agentreplv1connectClient) (<-chan streamResult[agentreplv1.WatchDaemonResponse], context.CancelFunc) {
	return openAsync(func(ctx context.Context) (*connect.ServerStreamForClient[agentreplv1.WatchDaemonResponse], error) {
		return client.WatchDaemon(ctx, connect.NewRequest(&agentreplv1.WatchDaemonRequest{}))
	})
}

// awaitOpen returns the opened stream.  Callers do NOT Close() it: Close
// drains the response body, and a STANDING stream never finishes producing
// one — cancelling the request context is the graceful close on this
// contract (elisp.md, "Stream lifecycle").
func awaitOpen[T any](t *testing.T, ch <-chan streamResult[T]) *connect.ServerStreamForClient[T] {
	t.Helper()
	res := <-ch
	if res.err != nil {
		t.Fatalf("stream open failed: %v", res.err)
	}
	return res.stream
}

func TestPushReachesTheKeyedWorkspaceStream(t *testing.T) {
	// Arrange.
	server, baseURL := newTestServer(t)
	client := newTestClient(t, baseURL)
	opened, cancel := openHostAsync(client, "ws-a")
	defer cancel()
	server.awaitSubscribers(streamHost, "ws-a", 1)

	// Act.
	status, body := controlPost(t, baseURL, "/_fake/push",
		`{"stream":"host","workspace_id":"ws-a","message":{"reloadWebapp":{}}}`)
	if status != http.StatusOK {
		t.Fatalf("/_fake/push = %d %s", status, body)
	}

	// Assert: WatchHostWorkspace is ONE subscription per open workspace, so a
	// push keyed by that workspace lands on it.
	stream := awaitOpen(t, opened)
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
	openedA, cancelA := openHostAsync(client, "ws-a")
	defer cancelA()
	openedB, cancelB := openHostAsync(client, "ws-b")
	defer cancelB()
	server.awaitSubscribers(streamHost, "ws-a", 1)
	server.awaitSubscribers(streamHost, "ws-b", 1)

	// Act.
	if status, body := controlPost(t, baseURL, "/_fake/push",
		`{"stream":"host","workspace_id":"ws-a","message":{"reloadWebapp":{}}}`); status != http.StatusOK {
		t.Fatalf("/_fake/push = %d %s", status, body)
	}

	// Assert: ws-a's open completes (headers flushed by the delivery) while
	// ws-b's is still outstanding — nothing was routed to the other workspace.
	streamA := awaitOpen(t, openedA)
	if !streamA.Receive() {
		t.Fatalf("ws-a stream ended without a push: %v", streamA.Err())
	}
	select {
	case res := <-openedB:
		t.Fatalf("ws-b stream received something: %+v", res)
	default:
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
	opened, cancel := openDaemonAsync(client)
	defer cancel()
	server.awaitSubscribers(streamDaemon, "", 1)

	// Assert: streams are "now", never "since" — a late subscriber gets the
	// standing state as its first push.
	stream := awaitOpen(t, opened)
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
	opened, cancel := openDaemonAsync(client)
	defer cancel()
	server.awaitSubscribers(streamDaemon, "", 1)

	// Act.
	if status, body := controlPost(t, baseURL, "/_fake/end",
		`{"stream":"daemon","error":{"code":"unavailable","message":"daemon went away"}}`); status != http.StatusOK {
		t.Fatalf("/_fake/end = %d %s", status, body)
	}

	// Assert: an end frame carrying an error is a stream FAILURE the client
	// must surface, not a graceful close.
	stream := awaitOpen(t, opened)
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
	opened, cancel := openDaemonAsync(client)
	defer cancel()
	server.awaitSubscribers(streamDaemon, "", 1)

	// Act.
	if status, body := controlPost(t, baseURL, "/_fake/end", `{"stream":"daemon"}`); status != http.StatusOK {
		t.Fatalf("/_fake/end = %d %s", status, body)
	}

	// Assert: the end frame arrives cleanly.  (A STANDING stream's consumer
	// treats that as a failure of its own — that policy is the client's.)
	stream := awaitOpen(t, opened)
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
	opened, cancel := openDaemonAsync(client)
	defer cancel()
	server.awaitSubscribers(streamDaemon, "", 1)

	// Act.
	if status, body := controlPost(t, baseURL, "/_fake/end",
		`{"stream":"daemon","abort":true}`); status != http.StatusOK {
		t.Fatalf("/_fake/end = %d %s", status, body)
	}

	// Assert: a producer-side end WITHOUT a terminal frame is a transport
	// failure, and must not read as a clean close.
	res := <-opened
	if res.err != nil {
		return
	}
	stream := res.stream
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
	_, cancel := openHostAsync(client, "ws-a")
	defer cancel()
	server.awaitSubscribers(streamHost, "ws-a", 1)

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
