package main

import (
	"context"
	"net/http"
	"strings"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"
	"connectrpc.com/connect"
)

func adoptRequest() *connect.Request[agentreplv1.AdoptHostWorkspaceRequest] {
	return connect.NewRequest(&agentreplv1.AdoptHostWorkspaceRequest{
		Workspace: &workspacev1.WorkspaceRef{Id: "ws-a", Dir: "/tmp/ws-a"}})
}

// A gated method RECORDS its call immediately: a scenario asserting on the
// client's state while the answer is in flight needs to see the call land.
func TestAGatedCallIsRecordedBeforeItIsAnswered(t *testing.T) {
	// Arrange.
	server, baseURL := newTestServer(t)
	client := newTestClient(t, baseURL)
	if status, body := controlPost(t, baseURL, "/_fake/gate",
		`{"method":"AdoptHostWorkspace"}`); status != 200 {
		t.Fatalf("/_fake/gate answered %d: %s", status, body)
	}

	// Act: the call is issued on its own goroutine and left in flight.
	answered := make(chan error, 1)
	go func() {
		_, err := client.AdoptHostWorkspace(context.Background(), adoptRequest())
		answered <- err
	}()
	// Real synchronization, not a sleep: wait on the recording itself.
	server.awaitRecordedCalls("AdoptHostWorkspace", 1)

	// Assert: the call is on the record while the answer is still withheld.
	select {
	case err := <-answered:
		t.Fatalf("the gated call answered before release: %v", err)
	default:
	}

	// Act: release.
	if status, body := controlPost(t, baseURL, "/_fake/gate",
		`{"method":"AdoptHostWorkspace","release":true}`); status != 200 {
		t.Fatalf("release answered %d: %s", status, body)
	}

	// Assert.
	if err := <-answered; err != nil {
		t.Fatalf("the released call failed: %v", err)
	}
}

// Releasing a gate nobody armed is a loud 400: the scenario and the fake
// disagree about what is held, and a silent success would hide it.
func TestReleasingAnUnarmedGateIsRefused(t *testing.T) {
	// Arrange.
	_, baseURL := newTestServer(t)

	// Act.
	status, body := controlPost(t, baseURL, "/_fake/gate",
		`{"method":"AdoptHostWorkspace","release":true}`)

	// Assert.
	if status != 400 {
		t.Fatalf("status = %d, want 400 (body %s)", status, body)
	}
	if !strings.Contains(body, "no gate is armed") {
		t.Fatalf("body = %s, want the unarmed-gate reason", body)
	}
}

// A gate names a unary method or one of Emacs's streams; anything else is a
// caller mistake.
func TestGatingAnUnknownMethodIsRefused(t *testing.T) {
	// Arrange.
	_, baseURL := newTestServer(t)

	// Act.
	status, body := controlPost(t, baseURL, "/_fake/gate", `{"method":"NotAMethod"}`)

	// Assert.
	if status != 400 {
		t.Fatalf("status = %d, want 400 (body %s)", status, body)
	}
	if !strings.Contains(body, "unknown unary or stream method") {
		t.Fatalf("body = %s, want the unknown-method reason", body)
	}
}

// A GATED STREAM is a stream that was dialled but not accepted: the fake
// withholds the response header block, which is the one thing a client reads
// as "this subscription stands" (fanout §3 STANDING-STREAM ACCEPTANCE).  No
// unary gate can stage that, and without it a scenario cannot observe a
// client that must wait for acceptance before it acts.
func TestAGatedStreamWithholdsItsAcceptance(t *testing.T) {
	// Arrange.
	server, baseURL := newTestServer(t)
	if status, body := controlPost(t, baseURL, "/_fake/gate",
		`{"method":"WatchDaemon"}`); status != 200 {
		t.Fatalf("/_fake/gate answered %d: %s", status, body)
	}

	// Act: dial, and leave the dial standing on its own goroutine.
	type opened struct {
		resp *http.Response
		err  error
	}
	headers := make(chan opened, 1)
	ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
	defer cancel()
	req, err := http.NewRequestWithContext(ctx, http.MethodPost,
		baseURL+"/agentrepl.v1.AgentRepl/WatchDaemon", connectStreamBody(emacsWatchDaemonJSON))
	if err != nil {
		t.Fatalf("build WatchDaemon request: %v", err)
	}
	req.Header.Set("Content-Type", "application/connect+json")
	req.Header.Set("Connect-Protocol-Version", "1")
	go func() {
		resp, err := http.DefaultClient.Do(req)
		headers <- opened{resp, err}
	}()
	// Real synchronization: the call is on the record the moment it lands.
	server.awaitRecordedCalls("WatchDaemon", 1)

	// Assert: recorded, yet neither accepted nor subscribed.
	select {
	case out := <-headers:
		if out.resp != nil {
			out.resp.Body.Close()
		}
		t.Fatalf("the gated stream was accepted before release (err=%v)", out.err)
	default:
	}
	if infos := server.subscriberInfos(); len(infos) != 0 {
		t.Fatalf("subscribers = %v, want none while acceptance is withheld", infos)
	}

	// Act: release.
	if status, body := controlPost(t, baseURL, "/_fake/gate",
		`{"method":"WatchDaemon","release":true}`); status != 200 {
		t.Fatalf("release answered %d: %s", status, body)
	}

	// Assert: NOW the headers land and the subscription stands.
	out := <-headers
	if out.err != nil {
		t.Fatalf("the released stream never sent headers: %v", out.err)
	}
	defer out.resp.Body.Close()
	if out.resp.StatusCode != http.StatusOK {
		t.Fatalf("released stream answered %d, want 200", out.resp.StatusCode)
	}
	server.mustAwaitSubscribers(t, streamDaemon, "", 1)
}

// The gate's method set is closed over the rpcs this fake serves: a stream it
// refuses outright, or a name that is no rpc at all, must fail loudly rather
// than arm a gate nothing will ever hit.
func TestGatingAWebappOnlyStreamIsRefused(t *testing.T) {
	// Arrange.
	_, baseURL := newTestServer(t)

	// Act.
	status, body := controlPost(t, baseURL, "/_fake/gate", `{"method":"WatchFeed"}`)

	// Assert.
	if status != 400 {
		t.Fatalf("/_fake/gate WatchFeed answered %d, want 400 (body %s)", status, body)
	}
	if !strings.Contains(body, "unknown unary or stream method") {
		t.Fatalf("refusal body = %s, want it to name the unknown method", body)
	}
}
