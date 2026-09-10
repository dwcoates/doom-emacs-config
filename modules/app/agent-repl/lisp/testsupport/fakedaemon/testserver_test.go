package main

import (
	"bytes"
	"context"
	"encoding/json"
	"io"
	"net/http"
	"net/http/httptest"
	"testing"

	"agentrepl/proto/agentrepl/v1/agentreplv1connect"
	"connectrpc.com/connect"
)

// newTestServer starts the same handler main() serves, over HTTP/1.1, with
// the connection capture wired so /_fake/end {abort} has a conn to drop.
func newTestServer(t *testing.T) (*fakeServer, string) {
	t.Helper()
	server := newFakeServer()
	httpTest := httptest.NewUnstartedServer(nil)
	httpTest.Config.Handler = newHandler(server, func() {})
	httpTest.Config.ConnContext = connContext
	httpTest.Start()
	t.Cleanup(httpTest.Close)
	return server, httpTest.URL
}

// newTestClient returns a JSON-codec client, the codec Emacs speaks.
func newTestClient(t *testing.T, baseURL string) agentreplv1connect.AgentReplClient {
	t.Helper()
	return agentreplv1connect.NewAgentReplClient(
		httpTestClient(), baseURL, connect.WithProtoJSON())
}

func httpTestClient() connect.HTTPClient { return http.DefaultClient }

// controlPost drives one /_fake/ endpoint and returns its status and body.
func controlPost(t *testing.T, baseURL, path, body string) (int, string) {
	t.Helper()
	resp, err := http.Post(baseURL+path, "application/json", bytes.NewBufferString(body))
	if err != nil {
		t.Fatalf("POST %s: %v", path, err)
	}
	defer resp.Body.Close()
	out, err := io.ReadAll(resp.Body)
	if err != nil {
		t.Fatalf("read %s body: %v", path, err)
	}
	return resp.StatusCode, string(out)
}

func controlGet(t *testing.T, baseURL, path string) (int, string) {
	t.Helper()
	resp, err := http.Get(baseURL + path)
	if err != nil {
		t.Fatalf("GET %s: %v", path, err)
	}
	defer resp.Body.Close()
	out, err := io.ReadAll(resp.Body)
	if err != nil {
		t.Fatalf("read %s body: %v", path, err)
	}
	return resp.StatusCode, string(out)
}

// rawUnary posts a hand-written JSON body at one rpc, bypassing the generated
// client — the only way to send a field the generated request type has no
// name for.
func rawUnary(t *testing.T, baseURL, method, body string) (int, string) {
	t.Helper()
	req, err := http.NewRequestWithContext(context.Background(), http.MethodPost,
		baseURL+"/agentrepl.v1.AgentRepl/"+method, bytes.NewBufferString(body))
	if err != nil {
		t.Fatalf("build request: %v", err)
	}
	req.Header.Set("Content-Type", "application/json")
	req.Header.Set("Connect-Protocol-Version", "1")
	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		t.Fatalf("POST %s: %v", method, err)
	}
	defer resp.Body.Close()
	out, _ := io.ReadAll(resp.Body)
	return resp.StatusCode, string(out)
}

func decodeCalls(t *testing.T, body string) []recordedCall {
	t.Helper()
	var calls []recordedCall
	if err := json.Unmarshal([]byte(body), &calls); err != nil {
		t.Fatalf("decode /_fake/calls: %v (body %s)", err, body)
	}
	return calls
}

// agentrepl1connectClient names the generated client interface without
// spelling the long package-qualified type at every call site.
type agentreplv1connectClient = agentreplv1connect.AgentReplClient
