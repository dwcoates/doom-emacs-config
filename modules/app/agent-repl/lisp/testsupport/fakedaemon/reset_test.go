package main

import (
	"context"
	"net/http"
	"strings"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	"connectrpc.com/connect"
)

// /_fake/reset is what lets ONE fake daemon serve a whole suite: every test
// starts against a fake indistinguishable from a freshly spawned one, without
// paying a process boot per test.

func TestResetClearsTheRecordedCalls(t *testing.T) {
	// Arrange.
	server, baseURL := newTestServer(t)
	client := newTestClient(t, baseURL)
	if _, err := client.DaemonHealth(context.Background(),
		connect.NewRequest(&agentreplv1.DaemonHealthRequest{})); err != nil {
		t.Fatalf("DaemonHealth: %v", err)
	}
	server.awaitRecordedCalls("DaemonHealth", 1)

	// Act.
	if status, body := controlPost(t, baseURL, "/_fake/reset", `{}`); status != http.StatusOK {
		t.Fatalf("/_fake/reset answered %d: %s", status, body)
	}

	// Assert.
	status, body := controlGet(t, baseURL, "/_fake/calls")
	if status != http.StatusOK {
		t.Fatalf("/_fake/calls answered %d: %s", status, body)
	}
	if calls := decodeCalls(t, body); len(calls) != 0 {
		t.Fatalf("reset left %d call(s) on the record: %s", len(calls), body)
	}
}

func TestResetReportsWhatItCleared(t *testing.T) {
	// Arrange.
	server, baseURL := newTestServer(t)
	client := newTestClient(t, baseURL)
	if _, err := client.DaemonHealth(context.Background(),
		connect.NewRequest(&agentreplv1.DaemonHealthRequest{})); err != nil {
		t.Fatalf("DaemonHealth: %v", err)
	}
	server.awaitRecordedCalls("DaemonHealth", 1)

	// Act.
	_, body := controlPost(t, baseURL, "/_fake/reset", `{}`)

	// Assert.
	if !strings.Contains(body, `"calls":1`) {
		t.Fatalf("/_fake/reset did not report the cleared call: %s", body)
	}
}

func TestResetDropsAScriptedResponse(t *testing.T) {
	// Arrange.
	_, baseURL := newTestServer(t)
	client := newTestClient(t, baseURL)
	if status, body := controlPost(t, baseURL, "/_fake/script",
		`{"method":"RegisterWorkspace","response":{"error":{}}}`); status != http.StatusOK {
		t.Fatalf("/_fake/script answered %d: %s", status, body)
	}

	// Act.
	if status, body := controlPost(t, baseURL, "/_fake/reset", `{}`); status != http.StatusOK {
		t.Fatalf("/_fake/reset answered %d: %s", status, body)
	}

	// Assert: the default synthesis answers again, so the script is gone.
	res, err := client.RegisterWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.RegisterWorkspaceRequest{Dir: "/tmp/ws"}))
	if err != nil {
		t.Fatalf("RegisterWorkspace: %v", err)
	}
	if res.Msg.GetError() != nil {
		t.Fatalf("reset left the scripted response in place: %v", res.Msg)
	}
}

func TestResetDropsAStoredSnapshot(t *testing.T) {
	// Arrange.
	server, baseURL := newTestServer(t)
	client := newTestClient(t, baseURL)
	if status, body := controlPost(t, baseURL, "/_fake/push",
		`{"stream":"host","workspace_id":"ws-a","snapshot":true,"message":{"reloadWebapp":{}}}`); status != http.StatusOK {
		t.Fatalf("/_fake/push answered %d: %s", status, body)
	}

	// Act.
	if status, body := controlPost(t, baseURL, "/_fake/reset", `{}`); status != http.StatusOK {
		t.Fatalf("/_fake/reset answered %d: %s", status, body)
	}

	// Assert: a NEW subscriber gets no replay, so the snapshot is gone.  The
	// stream is ended from the control plane, and a replayed snapshot would
	// have arrived before that end frame.
	stream, cancel := openHost(t, server, client, "ws-a")
	defer cancel()
	if status, body := controlPost(t, baseURL, "/_fake/end", `{"stream":"host","workspace_id":"ws-a"}`); status != http.StatusOK {
		t.Fatalf("/_fake/end answered %d: %s", status, body)
	}
	if stream.Receive() {
		t.Fatalf("reset left a snapshot to replay: %v", stream.Msg())
	}
}

func TestResetReleasesAnArmedGate(t *testing.T) {
	// Arrange.
	server, baseURL := newTestServer(t)
	client := newTestClient(t, baseURL)
	if status, body := controlPost(t, baseURL, "/_fake/gate",
		`{"method":"AdoptHostWorkspace"}`); status != http.StatusOK {
		t.Fatalf("/_fake/gate answered %d: %s", status, body)
	}
	answered := make(chan error, 1)
	go func() {
		_, err := client.AdoptHostWorkspace(context.Background(), adoptRequest())
		answered <- err
	}()
	server.awaitRecordedCalls("AdoptHostWorkspace", 1)

	// Act.
	if status, body := controlPost(t, baseURL, "/_fake/reset", `{}`); status != http.StatusOK {
		t.Fatalf("/_fake/reset answered %d: %s", status, body)
	}

	// Assert: the held call answers rather than hanging into the next test.
	if err := <-answered; err != nil {
		t.Fatalf("the gate reset did not release the held call: %v", err)
	}
}

func TestResetEndsAStandingStream(t *testing.T) {
	// Arrange.
	server, baseURL := newTestServer(t)
	client := newTestClient(t, baseURL)
	stream, cancel := openHost(t, server, client, "ws-a")
	defer cancel()

	// Act.
	if status, body := controlPost(t, baseURL, "/_fake/reset", `{}`); status != http.StatusOK {
		t.Fatalf("/_fake/reset answered %d: %s", status, body)
	}

	// Assert: a clean end frame, so the client sees a finished stream and no
	// error, and the subscriber registry drains.
	if stream.Receive() {
		t.Fatalf("the stream delivered a message instead of ending: %v", stream.Msg())
	}
	if err := stream.Err(); err != nil {
		t.Fatalf("reset ended the stream with an error: %v", err)
	}
	server.mustAwaitSubscribers(t, streamHost, "ws-a", 0)
}

func TestResetRequiresPost(t *testing.T) {
	// Arrange.
	_, baseURL := newTestServer(t)

	// Act.
	status, body := controlGet(t, baseURL, "/_fake/reset")

	// Assert.
	if status != http.StatusBadRequest {
		t.Fatalf("GET /_fake/reset answered %d: %s", status, body)
	}
}

func TestResetRefusesAnUnknownField(t *testing.T) {
	// Arrange.
	_, baseURL := newTestServer(t)

	// Act.
	status, body := controlPost(t, baseURL, "/_fake/reset", `{"nope":true}`)

	// Assert.
	if status != http.StatusBadRequest {
		t.Fatalf("/_fake/reset accepted an unknown field: %d %s", status, body)
	}
}
