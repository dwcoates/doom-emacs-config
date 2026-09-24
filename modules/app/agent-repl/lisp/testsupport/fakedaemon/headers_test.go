package main

import (
	"context"
	"io"
	"net/http"
	"testing"
)

// /_fake/calls records the request headers, because the Connect protocol is
// not only a body shape: the content type and the protocol-version header are
// part of what a correct client sends.
func TestRecordedCallEchoesTheRequestHeaders(t *testing.T) {
	// Arrange.
	_, baseURL := newTestServer(t)

	// Act.
	if status, body := rawUnary(t, baseURL, "RegisterWorkspace", `{"dir":"/tmp/ws-a"}`); status != 200 {
		t.Fatalf("RegisterWorkspace answered %d: %s", status, body)
	}
	_, listing := controlGet(t, baseURL, "/_fake/calls")
	calls := decodeCalls(t, listing)

	// Assert.
	if len(calls) != 1 {
		t.Fatalf("want 1 recorded call, got %d", len(calls))
	}
	if got := calls[0].Headers["Content-Type"]; got != "application/json" {
		t.Fatalf("Content-Type = %q, want application/json", got)
	}
	if got := calls[0].Headers["Connect-Protocol-Version"]; got != "1" {
		t.Fatalf("Connect-Protocol-Version = %q, want 1", got)
	}
}

// A STREAM's own content type is a different one, and the recording keeps
// each request's headers separately rather than one global set.
func TestRecordedStreamCallKeepsItsOwnContentType(t *testing.T) {
	// Arrange.
	_, baseURL := newTestServer(t)
	req, err := http.NewRequestWithContext(context.Background(), http.MethodPost,
		baseURL+"/agentrepl.v1.AgentRepl/WatchDaemon", connectStreamBody(emacsWatchDaemonJSON))
	if err != nil {
		t.Fatalf("build request: %v", err)
	}
	req.Header.Set("Content-Type", "application/connect+json")
	req.Header.Set("Connect-Protocol-Version", "1")

	// Act.
	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		t.Fatalf("POST WatchDaemon: %v", err)
	}
	// The stream stands until the client goes away; the subscription is
	// registered (and so recorded) before the headers are flushed, so
	// reading the header block is enough and the body is then abandoned.
	if resp.StatusCode != 200 {
		out, _ := io.ReadAll(resp.Body)
		resp.Body.Close()
		t.Fatalf("WatchDaemon answered %d: %s", resp.StatusCode, out)
	}
	resp.Body.Close()

	_, listing := controlGet(t, baseURL, "/_fake/calls")
	calls := decodeCalls(t, listing)

	// Assert.
	if len(calls) != 1 {
		t.Fatalf("want 1 recorded call, got %d", len(calls))
	}
	if calls[0].Method != "WatchDaemon" {
		t.Fatalf("method = %q, want WatchDaemon", calls[0].Method)
	}
	if got := calls[0].Headers["Content-Type"]; got != "application/connect+json" {
		t.Fatalf("Content-Type = %q, want application/connect+json", got)
	}
}
