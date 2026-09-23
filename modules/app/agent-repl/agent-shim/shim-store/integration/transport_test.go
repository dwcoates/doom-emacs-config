// transport_test.go — SUBJECT 10: the transport surface.
//
// The store is one h2c handler over a unix socket, which is what makes HTTP/1.1
// and HTTP/2 clients and both Connect codecs work without the store knowing
// anything about any of them. The one thing it owes on a malformed call is an
// ANSWER: a wrong procedure is a Connect error, never a hang.
package integration

import (
	"bytes"
	"encoding/json"
	"io"
	"net/http"
	"testing"

	connect "connectrpc.com/connect"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/proto/store/v1/storev1connect"
)

// TestBothHTTPVersionsServeTheSameStore.
func TestBothHTTPVersionsServeTheSameStore(t *testing.T) {
	tests := []struct {
		name   string
		client func(*storeProcess) storev1connect.ShimStoreClient
		label  string
	}{
		{name: "h2c", client: (*storeProcess).client, label: "over h2c"},
		{name: "http1.1", client: (*storeProcess).http1Client, label: "over HTTP/1.1"},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			store := startStore(t, storeOptions{})
			ctx, cancel := callContext(t)
			defer cancel()
			cli := tc.client(store)
			shim := streamProducer(cli)

			// Act.
			shim.write(ctx, t,
				shim.agentEntry("w-"+tc.name, "u-"+tc.name,
					frameLine(agentID("main"), responseFrame("main", "act-1", tc.label))),
			)

			// Assert: writes AND the streaming read both work on this version.
			opened := openSession(ctx, t, cli, "main", 10, nil)
			assertTexts(t, "the page "+tc.label, pageTexts(opened.GetPage()), []string{tc.label})

			stream := watchStream(ctx, t, cli, opened.GetWatch())
			defer closeOrFail(t, stream)
			shim.write(ctx, t,
				shim.agentEntry("w-"+tc.name+"-2", "u-"+tc.name+"-2",
					frameLine(agentID("main"), responseFrame("main", "act-2", "tailed"))),
			)
			assertTexts(t, "the tail "+tc.label, receivedTexts(receiveLines(t, stream, 1)), []string{"tailed"})
			store.assertNoErrorRecords()
		})
	}
}

// TestJSONCodecServesARawPost: the JSON codec is not a separate endpoint or a
// separate build — it is the same handler answering a different content type,
// which is what makes the surface curl-able for a human debugging it.
func TestJSONCodecServesARawPost(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	shim := streamProducer(store.client())
	shim.write(ctx, t,
		shim.agentEntry("w-json-1", "u-json-1",
			frameLine(agentID("main"), subagentSpawnFrame("main", "act-spawn", "sub-json", "work", 1000))),
	)

	req, err := http.NewRequestWithContext(ctx, http.MethodPost,
		baseURL+storev1connect.ShimStoreGetLiveWorkProcedure, bytes.NewReader([]byte(`{"session":{"value":"main"}}`)))
	if err != nil {
		t.Fatalf("building the raw JSON request: %v", err)
	}
	req.Header.Set("Content-Type", "application/json")

	// Act.
	resp, err := http1HTTPClient(store.socket).Do(req)
	if err != nil {
		t.Fatalf("raw JSON POST over the unix socket: %v", err)
	}
	defer closeOrFail(t, resp.Body)
	body, err := io.ReadAll(resp.Body)
	if err != nil {
		t.Fatalf("reading the JSON response: %v", err)
	}

	// Assert.
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("raw JSON POST answered HTTP %d: %s", resp.StatusCode, body)
	}
	var decoded struct {
		Success struct {
			LiveAgents []struct {
				Value string `json:"value"`
			} `json:"liveAgents"`
		} `json:"success"`
	}
	if err := json.Unmarshal(body, &decoded); err != nil {
		t.Fatalf("the JSON codec answered something that is not JSON: %v\nbody: %s", err, body)
	}
	found := false
	for _, agent := range decoded.Success.LiveAgents {
		if agent.Value == "sub-json" {
			found = true
		}
	}
	if !found {
		t.Errorf("the JSON answer does not carry the live agent: %s", body)
	}
	store.assertNoErrorRecords()
}

// TestUnknownProcedureIsAConnectErrorNotAHang: the store answers everything it
// is asked, including questions it does not have.
func TestUnknownProcedureIsAConnectErrorNotAHang(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	wrong := connect.NewClient[storev1.GetLiveWorkRequest, storev1.GetLiveWorkResponse](
		h2cHTTPClient(store.socket),
		baseURL+"/store.v1.ShimStore/NoSuchProcedure",
	)

	// Act.
	_, err := wrong.CallUnary(ctx, connect.NewRequest(&storev1.GetLiveWorkRequest{Session: agentID("main")}))

	// Assert.
	if err == nil {
		t.Fatalf("an unknown procedure answered success")
	}
	if code := connect.CodeOf(err); code != connect.CodeUnimplemented {
		t.Errorf("an unknown procedure answered Connect code %v, want %v (error: %v)", code, connect.CodeUnimplemented, err)
	}
}
