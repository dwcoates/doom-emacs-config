package main

import (
	"context"
	"strings"
	"testing"

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

// A gate names a unary method; anything else is a caller mistake.
func TestGatingAnUnknownMethodIsRefused(t *testing.T) {
	// Arrange.
	_, baseURL := newTestServer(t)

	// Act.
	status, body := controlPost(t, baseURL, "/_fake/gate", `{"method":"NotAMethod"}`)

	// Assert.
	if status != 400 {
		t.Fatalf("status = %d, want 400 (body %s)", status, body)
	}
	if !strings.Contains(body, "unknown unary method") {
		t.Fatalf("body = %s, want the unknown-method reason", body)
	}
}
